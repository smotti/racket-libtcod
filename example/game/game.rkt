#lang typed/racket

(require math/array
         racket/block
         racket/format
         threading
         typed/racket/class

         "../../color.rkt"
         "../../console.rkt"
         "../../fov.rkt"
         "../../mouse.rkt"
         "../../sys.rkt"
         "game-constants.rkt"
         "game-types.rkt"
         "map.rkt")

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

(define default-player
  (make-game-object (position 0 0)
                    #\@
                    color-white
                    'player
                    "namra"
                    'alive
                    #:fighter (make-fighter #:hp 30
                                            #:defense 2
                                            #:power 5
                                            #:die-proc die-player)))

(define root-console (console-init-root SCREEN-WIDTH
                                   SCREEN-HEIGHT
                                   "Example"
                                   #f
                                   'RENDERER_SDL))
(define offscreen-console (console-new MAP-WIDTH MAP-HEIGHT))
(define panel (console-new SCREEN-WIDTH PANEL-HEIGHT))
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

(: render-bar (-> Console Integer Integer Integer String Integer Integer Color Color Void))
(define (render-bar console x y total-width name value maximum bar-color back-color)
  (console-set-default-background console back-color)
  (console-rect console x y total-width 1 #f 'BKGND_SCREEN)

  (define bar-width (exact-round (* (/ value maximum) total-width)))
  (console-set-default-background console bar-color)
  (when (> bar-width 0)
    (console-rect console x y bar-width 1 #f 'BKGND_SCREEN))

  (console-set-default-foreground console color-white)
  (console-print-ex console
                    (exact-round (/ (+ x total-width) 2)) y
                    'BKGND_NONE 'CENTER
                    (format "~a: ~a/~a" name value maximum)))

(: get-names-under-mouse (-> Mouse (Listof GameObject) FovMap String))
(define (get-names-under-mouse mouse objects fov-map)
  (define-values (x y) (values (mouse-cx mouse) (mouse-cy mouse)))
  (define names
    (for/fold
        ([ns : (Listof String) '()])
        ([obj objects])
      (define obj-position (game-object-position obj))
      (define obj-x (position-x obj-position))
      (define obj-y (position-y obj-position))
      (if (and (= obj-x x) (= obj-y y)
               (map-is-in-fov fov-map obj-x obj-y))
          (cons (game-object-name obj) ns)
          ns)))

  (string-join names ", "))

(: render-all (-> GameState Console Console GameState))
(define (render-all state con panel)
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

  ;(log-debug "Draw dead game objects")
  (for-each (lambda ([obj : GameObject]) (draw obj fov-map con))
            (game-state-dead new-state))

  ;(log-debug "Draw alive game objects")
  (for-each (lambda ([obj : GameObject]) (draw obj fov-map con))
            (game-state-objects new-state))
  (draw player fov-map con)

  ;(log-debug "Render HP status bar")
  (console-set-default-background panel color-black)
  (console-clear panel)
  (define player-fighter (game-object-fighter player))
  (render-bar panel
              1 1
              BAR-WIDTH
              "HP"
              (fighter-hp (cast player-fighter Fighter))
              (fighter-max-hp (cast player-fighter Fighter))
              color-light-red color-darker-red)

  ;(log-debug "Render object names under mouse cursor")
  (console-set-default-foreground panel color-light-gray)
  (console-print-ex panel
                    1 0
                    'BKGND_NONE 'LEFT
                    (get-names-under-mouse (game-state-mouse new-state)
                                           (game-state-objects new-state)
                                           fov-map))

  ;(log-debug "Render message log")
  (define messages (unbox message-log))
  (for ([y (length messages)]
        [m messages])
    ;(log-debug "Render message: ~v in color ~v at ~v" (car m) (cdr m) y)
    (console-set-default-foreground panel (cdr m))
    (console-print-ex panel MSG-X (add1 y) 'BKGND_NONE 'LEFT (car m)))

  (console-blit panel 0 0 SCREEN-WIDTH PANEL-HEIGHT console-root 0 PANEL-Y)

  ;(log-debug "Blit drawing offscreen-console to root-console")
  (console-blit con 0 0 MAP-WIDTH MAP-HEIGHT console-root 0 0)
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
             (= y (position-y obj-posn))
             (not (null? (game-object-fighter o))))
        o
        #f)))

(: take-damage (-> GameObject Integer GameObject))
(define (take-damage obj damage)
  (define a-fighter (game-object-fighter obj))
  ; This is a good place where lenses would make things better
  (define new-fighter (struct-copy fighter
                                   a-fighter
                                   [hp (- (fighter-hp a-fighter)
                                          damage)]))
  (define new-obj-w-fighter (struct-copy game-object obj [fighter new-fighter]))
  (if (<= (fighter-hp new-fighter) 0)
      ((fighter-die-proc new-fighter) new-obj-w-fighter)
      new-obj-w-fighter))

; returns the attacked target
(: attack (-> GameState GameObject GameObject GameObject))
(define (attack state attacker target)
  (define attacker-fighter (game-object-fighter attacker))
  (define target-fighter (game-object-fighter target))
  (define damage (- (fighter-power (cast attacker-fighter Fighter))
                    (fighter-defense (cast target-fighter Fighter))))

  (cond [(> damage 0)
         (message-add (format "~s attacks ~s for ~v hit points."
                              (game-object-name attacker)
                              (game-object-name target)
                              damage))
         (take-damage target damage)]
        [else
         (message-add (format "~s attacks ~s but is has no effect!"
                       (game-object-name attacker)
                       (game-object-name target)))
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
  ;;(define-values (event key mouse) (sys-wait-for-event 'KEY #t))
  (define key (game-state-key state))
  (define vk (key-vk key))

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
         (define-values (new-state new-objs)
           (for/fold
               ([latest-state : GameState state]
                [latest-objs : (Listof GameObject) '()])
               ([current-obj (game-state-objects state)])
             (cond [(null? (game-object-ai current-obj))
                    (values state (cons current-obj latest-objs))]
                   [else
                    (define-values (latest-obj latest-player)
                      (basic-monster-take-turn latest-state current-obj))
                    (values (struct-copy game-state state [player latest-player])
                            (cons latest-obj latest-objs))])))
         (struct-copy game-state new-state [objects new-objs])]
        [else state]))

(: filter-objects (-> GameState GameState))
(define (filter-objects state)
  (define-values (alive-objs dead-objs)
    (for/fold ([as : (Listof GameObject) '()]
               [ds : (Listof GameObject) (game-state-dead state)])
              ([o (game-state-objects state)])
      (cond [(null? o) (values as ds)]
            [(alive? o) (values (cons o as) ds)]
            [(dead? o) (values as (cons o ds))]
            [else (values as ds)])))
  (struct-copy game-state state [objects alive-objs] [dead dead-objs]))

(: wait-for-event (-> GameState GameState))
(define (wait-for-event state)
  (define-values (event key mouse) (sys-wait-for-event 'KEY_PRESS_MOUSE_MOVE
                                                       #t))
  (struct-copy game-state
               state [event event] [key key] [mouse mouse]))

(: game-loop (-> GameState Void))
(define (game-loop state)
  (unless (console-is-window-closed)
    (define new-state
      (~> state
          (render-all offscreen-console panel)
          wait-for-event
          clear-object-positions
          handle-keys
          objects-take-turn
          filter-objects))

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

    (make-game-state
     (struct-copy game-object default-player [position player-start-position])
     objs
     a-map fov-map)))

(message-add "Welcome stranger! Prepare to perish in the Tombs of the Ancient Kings."
             #:color color-red)
(game-loop initial-game-state)
