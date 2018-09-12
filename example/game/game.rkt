#lang typed/racket

(require math/array
         racket/block
         threading
         typed/racket/class

         "../../color.rkt"
         "game-types.rkt"
         "map.rkt")

(require/typed racket/base
  [(vector->values map-indexes) (-> Any (Values Integer Integer))])

(require/typed "../../console.rkt"
  [#:opaque Console console?]
  [#:opaque Key key?]
  [console-blit (-> Console Integer Integer Integer Integer Integer Integer Integer Void)]
  [console-flush (-> Void)]
  [console-init-root (-> Integer Integer String Boolean Symbol Void)]
  [console-is-key-pressed (-> Symbol Boolean)]
  [console-is-window-closed (-> Boolean)]
  [console-new (-> Integer Integer Console)]
  [console-put-char (-> Console Integer Integer Char Symbol Void)]
  [console-put-char-ex (-> Console Integer Integer Char Color Color Void)]
  [console-root Integer]
  [console-set-char-background (-> Console Integer Integer Color Symbol Void)]
  [console-set-default-foreground (-> Console Color Void)]
  [console-set-window-title (-> String Void)]
  [console-wait-for-keypress (-> Boolean Key)]
  [key-vk (-> Key Symbol)]
  )

(require/typed "../../fov.rkt"
  [#:opaque FovMap fov-map?]
  [map-compute-fov (-> FovMap Integer Integer Integer Boolean Integer Void)]
  [map-is-in-fov (-> FovMap Integer Integer Boolean)]
  [map-new (-> Integer Integer FovMap)]
  [map-set-properties (-> FovMap Integer Integer Boolean Boolean Void)]
  )

(require/typed "../../mouse.rkt"
  [#:opaque Mouse mouse?]
  )

(require/typed "../../sys.rkt"
  [sys-wait-for-event (-> Symbol Boolean (Values Symbol Key Mouse))]
  )


(define game-logger (make-logger))

;;;
;;; Colors
;;;

(define color-dark-wall (make-color 0 0 100))
(define color-light-wall (make-color 130 110 50))
(define color-dark-ground (make-color 50 50 150))
(define color-light-ground (make-color 200 180 50))

(define FOV-ALGO 0)
(define FOV-LIGHT-WALLS #t)
(define TORCH-RADIUS 10)

(define SCREEN-WIDTH 80)
(define SCREEN-HEIGHT 50)



; NOTE: That we have to use type Any for fov here, because the type checker would
;       fail even if the given type is the expected type.
(struct game-state ([player : GameObject]
                    [exit : Boolean]
                    [objects : (Listof GameObject)]
                    [map : (Array Tile)]
                    [fov : FovMap]
                    [fov-recompute : Boolean]
                    [mode : Symbol]
                    [action : Symbol]))
(define-type GameState game-state)

(define default-player (make-game-object (position 0 0)
                                         #\@
                                         color-white
                                         'player
                                         "namra"
                                         #:fighter (make-fighter #:hp 30
                                                                 #:defense 2
                                                                 #:power 5)))

(log-debug "Default player: ~a" default-player)

(define root-console (console-init-root SCREEN-WIDTH
                                   SCREEN-HEIGHT
                                   "Example"
                                   #f
                                   'RENDERER_SDL))
(define offscreen-console (console-new SCREEN-WIDTH SCREEN-HEIGHT))
;(console-set-keyboard-repeat 1 25)

(: draw (-> GameObject FovMap Console Void))
(define (draw a-object fov-map con)
  ;(log-debug (format "Draw object: ~s" a-object))
  (define obj-posn (game-object-position a-object))
  (define x (position-x obj-posn))
  (define y (position-y obj-posn))

  (when (map-is-in-fov fov-map x y)
    (console-set-default-foreground con (game-object-color a-object))
    (console-put-char con
                      x
                      y
                      (game-object-char a-object)
                      'BKGND_NONE)))

(: render-all (-> GameState Console GameState))
(define (render-all state con)
  ;(log-debug "Render screen")

  (define a-map (game-state-map state))

  ;(log-debug "Draw tiles")
  (define (is-wall? t) (tile-block-sight t))

  (define player (game-state-player state))
  (define player-x (~> player game-object-position position-x))
  (define player-y (~> player game-object-position position-y))

  ;(lob-debug "Compute FoV")
  (define fov-map (game-state-fov state))
  (define new-state
    (cond
      [(game-state-fov-recompute state)
       (map-compute-fov fov-map player-x player-y TORCH-RADIUS FOV-LIGHT-WALLS FOV-ALGO)
       (for ([y MAP-HEIGHT])
         (for ([x MAP-WIDTH])
           (let ([is-visible? : Boolean (map-is-in-fov fov-map x y)]
                  [a-tile : Tile (array-ref a-map `#(,y ,x))])
             (cond
               [(not is-visible?)
                (when (tile-explored a-tile)
                  (if (is-wall? a-tile)
                      (console-set-char-background con x y color-dark-wall 'BKGND_SET)
                      (console-set-char-background con x y color-dark-ground 'BKGND_SET)))]
               [else
                (cond
                  [(is-wall? a-tile)
                   (console-put-char-ex con x y #\# color-white color-dark-blue)
                   (console-set-char-background con x y color-light-wall 'BKGND_SET)]
                  [else
                   (console-put-char-ex con x y #\. color-white color-dark-blue)
                   (console-set-char-background con x y color-light-ground 'BKGND_SET)])
                (set-tile-explored! a-tile #t)]))))
       (struct-copy game-state state [fov-recompute #f])]
      [else state]))

  ;(log-debug "Draw game objects")
  (for-each (lambda ([obj : GameObject]) (draw obj fov-map con))
            (cons player (game-state-objects new-state)))

  ;(log-debug "Blit drawing offscreen-console to root-console")
  (console-blit offscreen-console 0 0 SCREEN-WIDTH SCREEN-HEIGHT console-root 0 0)
  (console-flush)

  new-state)

(: clear (-> Position Console FovMap Void))
(define (clear posn con fov-map)
  (when (map-is-in-fov fov-map (position-x posn) (position-y posn))
    (console-put-char con
                      (position-x posn) (position-y posn)
                      #\.
                      'BKGND_NONE)))

(: distance-to (-> GameObject GameObject Real))
(define (distance-to obj other)
  (define obj-posn (game-object-position obj))
  (define other-posn (game-object-position other))
  (define dx (- (position-x other-posn) (position-x obj-posn)))
  (define dy (- (position-y other-posn) (position-y obj-posn)))
  (sqrt (+ (sqr dx) (sqr dy))))

(: move-towards (-> GameState GameObject Integer Integer GameObject))
(define (move-towards state obj target-x target-y)
  (define posn (game-object-position obj))

  ; vector from obj to target and distance
  (define dx (- target-x (position-x posn)))
  (define dy (- target-y (position-y posn)))
  (define distance (sqrt (+ (sqr dx) (sqr dy))))

  ; normalize vector to length of 1 (preserving direction)
  ; rounding is done to get an integer that is restricted to the map grid
  (move state
        obj
        (~> dx (/ distance) (cast Real) exact-round)
        (~> dy (/ distance) (cast Real) exact-round)))

; Produces two new values for the given object and the player
(: basic-monster-take-turn (-> GameState GameObject (Values GameObject GameObject)))
(define (basic-monster-take-turn state obj)
  (define obj-posn (game-object-position obj))
  (define player (game-state-player state))
  (define player-posn (game-object-position player))

  (if (not (map-is-in-fov (game-state-fov state)
                          (position-x obj-posn)
                          (position-y obj-posn)))
      (values obj player)
      (cond [(>= (distance-to obj player) 2)
             (values (move-towards state
                                   obj
                                   (position-x player-posn)
                                   (position-y player-posn))
                     player)]
            [else
             (values obj (attack state obj player))])))

(: move (-> GameState GameObject Integer Integer GameObject))
(define (move state obj dx dy)
  (define current-posn (game-object-position obj))
  (define-values (x y)
    (values (+ (position-x current-posn) dx)
            (+ (position-y current-posn) dy)))

  (cond [(is-blocked? x y (game-state-map state) (game-state-objects state))
         obj]
        [else
         (define new-posn (position x y))
         ;(log-debug (format "New object position: ~s" new-posn))
         (struct-copy game-object obj [position new-posn])]))

(: get-move-deltas (-> (Values Integer Integer)))
(define (get-move-deltas)
  (cond
    [(console-is-key-pressed 'UP)
     (values 0 -1)]
    [(console-is-key-pressed 'DOWN)
     (values 0 1)]
    [(console-is-key-pressed 'LEFT)
     (values -1 0)]
    [(console-is-key-pressed 'RIGHT)
     (values 1 0)]
    [else (values 0 0)]))

(: attack? (-> GameState Integer Integer (U Boolean GameObject)))
(define (attack? state dx dy)
  (define current-posn (~> state game-state-player game-object-position))
  (define-values (x y)
    (values (+ (position-x current-posn) dx)
            (+ (position-y current-posn) dy)))

  (for/or : (U GameObject Boolean) ([o : GameObject (game-state-objects state)])
    (define obj-posn (game-object-position o))
    (if (and (= x (position-x obj-posn))
             (= y (position-y obj-posn)))
        o
        #f)))

(: take-damage (-> GameObject Integer GameObject))
(define (take-damage obj damage)
  (define a-fighter (game-object-fighter obj))
  ; This is a good place where lenses would make things better
  (cond [(> damage 0)
         (struct-copy game-object
                      obj
                      [fighter (struct-copy fighter
                                            a-fighter
                                            [hp (- (fighter-hp a-fighter)
                                                   damage)])])]
        [else obj]))

; returns the attacked target
(: attack (-> GameState GameObject GameObject GameObject))
(define (attack state attacker target)
  (define attacker-fighter (game-object-fighter attacker))
  (define target-fighter (game-object-fighter target))
  (define damage (- (fighter-power (cast attacker-fighter Fighter))
                    (fighter-defense (cast target-fighter Fighter))))

  (cond [(> damage 0)
         (log-debug "~s attacks ~s for ~v hit points."
                    (game-object-name attacker)
                    (game-object-name target)
                    damage)
         (take-damage target damage)]
        [else
         (log-debug "~s attacks ~s but is has no effect!"
                    (game-object-name attacker)
                    (game-object-name target))
         target]))

(: player-move-or-attack (-> GameState GameObject GameState))
(define (player-move-or-attack state player)
  (define-values (dx dy) (get-move-deltas))
  (define target (attack? state dx dy))
  (cond [(game-object? target)
         (struct-copy game-state
                      state
                      [objects (for/list : (Listof GameObject)
                                   ([obj : GameObject (game-state-objects state)])
                                 (if (equal? obj target)
                                     (attack state player target)
                                     obj))])]
        [else (struct-copy game-state
                           state
                           [fov-recompute #t]
                           [player (move state player dx dy)])]))

(: handle-keys (-> GameState GameState))
(define (handle-keys state)
  (define key (console-wait-for-keypress #t))
  ;;(define-values (event key mouse) (sys-wait-for-event 'KEY #t))
  (define vk (key-vk key))

  ;(log-debug (format "EVENT: ~s" event))
  ;(log-debug (format "KEY EVENT: ~s - ~s" vk (key-c key)))

  (cond
    [(equal? vk 'ESCAPE)
     (struct-copy game-state state [exit #t] [action 'exit])]
    [(and (eq? 'playing (game-state-mode state))
          (member vk '(UP DOWN LEFT RIGHT)))
     (struct-copy game-state
                  (player-move-or-attack state (game-state-player state))
                  [action 'turn])]
    [else (struct-copy game-state state [action 'no-turn])]))

(: clear-object-positions (-> GameState GameState))
(define (clear-object-positions state)
  ;(log-debug "Clear old position of objects")
  (define player (game-state-player state))
  (define fov-map (game-state-fov state))
  (for-each (lambda ([obj : GameObject])
              (clear (game-object-position obj) offscreen-console fov-map))
            (cons player (game-state-objects state)))

  state)

(: objects-take-turn (-> GameState GameState))
(define (objects-take-turn state)
  (cond [(and (eq? 'playing (game-state-mode state))
              (eq? 'turn (game-state-action state)))
         (define-values (new-objs new-player)
           (for/fold
               ([latest-objs : (Listof GameObject) '()]
                [latest-player : GameObject (game-state-player state)])
               ([current-obj (game-state-objects state)])
             (cond [(null? (game-object-ai current-obj))
                    (values (cons current-obj latest-objs) latest-player)]
                   [else
                    (define-values (latest-obj latest-player)
                      (basic-monster-take-turn state current-obj))
                    (values (cons latest-obj latest-objs)
                            latest-player)])))
         (struct-copy game-state state [player new-player] [objects new-objs])]
        [else state]))

(: game-loop (-> GameState Void))
(define (game-loop state)
  (unless (console-is-window-closed)
    (define new-state
      (~> state
          (render-all offscreen-console)
          clear-object-positions
          handle-keys
          objects-take-turn))

    (game-loop (if (game-state-exit new-state)
                   (exit)
                   new-state))))

(log-debug "Set window title")
(console-set-window-title "Testerlie")

(log-debug "Start game loop")
(define initial-game-state
  (let-values ([(a-map objs player-start-position) (make-map)])
    (log-debug "Initialize FoV map")
    (define fov-map (map-new MAP-WIDTH MAP-HEIGHT))
    (for ([y MAP-HEIGHT])
      (for ([x MAP-WIDTH])
        (let ([t : Tile (array-ref a-map `#(,y ,x))])
          (map-set-properties fov-map
                              x y
                              (not (tile-block-sight t)) (not (tile-blocked t))))))

    (game-state (struct-copy game-object default-player [position player-start-position])
                #f  ; exit game
                objs
                a-map
                fov-map
                #f  ; recompute fov
                'playing  ; mode
                'no-turn  ; action
                )))
(game-loop initial-game-state)
