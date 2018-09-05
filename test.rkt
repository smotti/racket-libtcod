#lang typed/racket

(require math/array
         racket/block)

(require/typed racket/base
  [(vector->values map-indexes) (-> Any (Values Integer Integer))])

(require/typed "libtcod.rkt"
  [#:opaque Color color?]
  [#:opaque Console console?]
  [#:opaque Key key?]
  [#:opaque Mouse mouse?]
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
  [color-dark-blue Color]
  [color-white Color]
  [color-yellow Color]
  [key-vk (-> Key Symbol)]
  [make-color (-> Integer Integer Integer Color)]
  [sys-wait-for-event (-> Symbol Boolean (Values Symbol Key Mouse))])

(define game-logger (make-logger))

(define SCREEN-WIDTH 80)
(define SCREEN-HEIGHT 50)

(define MAP-WIDTH 80)
(define MAP-HEIGHT 45)

(define color-dark-wall (make-color 0 0 100))
(define color-dark-ground (make-color 50 50 150))

(struct position ([x : Integer] [y : Integer]) #:transparent)
(define-type Position position)

(struct object
  ([position : Position] [char : Char] [color : Color] [type : String])
  #:transparent)
(define-type Object object)

(struct player object ([nickname : String]))
(define-type Player player)

(struct tile ([blocked : Boolean] [block-sight : Boolean]))
(define-type Tile tile)

(struct game-state ([a-player : Player]
                    [exit : Boolean]
                    [objects : (Listof Object)]
                    [map : (Array Tile)]))
(define-type GameState game-state)

(define player-start-position (position (cast (/ SCREEN-WIDTH 2) Integer)
                                        (cast (/ SCREEN-HEIGHT 2) Integer)))
(define default-player (player player-start-position
                               #\@
                               color-white
                               "player"
                               "namra"))
(define default-npc (object (position (cast (- (/ SCREEN-WIDTH 2) 5) Integer)
                                      (cast (/ SCREEN-HEIGHT 2) Integer))
                            #\@
                            color-yellow
                            "npc"))

(define root-console (console-init-root SCREEN-WIDTH
                                   SCREEN-HEIGHT
                                   "Example"
                                   #f
                                   'RENDERER_SDL))
(define offscreen-console (console-new SCREEN-WIDTH SCREEN-HEIGHT))
;(console-set-keyboard-repeat 1 25)

(define (make-map)
  (build-array (vector-immutable MAP-HEIGHT MAP-WIDTH)
               (lambda (is)
                 (define-values (y x) (map-indexes is))
                 (if (and (= x 22)
                          (member y '(30 31)))
                     (tile #t #t)
                     (tile #f #f)))))

(: draw (-> Object Console Void))
(define (draw a-object con)
  ;(log-debug (format "Draw object: ~s" a-object))
  (console-set-default-foreground con (object-color a-object))
  (define a-object-position (object-position a-object))
  (console-put-char con
                    (position-x a-object-position)
                    (position-y a-object-position)
                    (object-char a-object)
                    'BKGND_NONE))

(: render-all (-> GameState Console Void))
(define (render-all state con)
  ;(log-debug "Render screen")
  (define a-player (game-state-a-player state))
  (define map (game-state-map state))

  ;(log-debug "Draw tiles")
  (define (is-wall? t) (tile-block-sight t))
  (for ([y MAP-HEIGHT])
    (for ([x MAP-WIDTH])
      (if (is-wall? (array-ref map `#(,y ,x)))
          (console-put-char-ex con x y #\# color-white color-dark-blue)
          (console-put-char-ex con x y #\. color-white color-dark-blue))))

  ;(log-debug "Draw game objects")
  (for-each (lambda (#{obj : Object}) (draw obj con))
            (cons a-player (game-state-objects state)))

  ;(log-debug "Blit drawing offscreen-console to root-console")
  (console-blit offscreen-console 0 0 SCREEN-WIDTH SCREEN-HEIGHT console-root 0 0))

(: clear (-> Position Console Void))
(define (clear posn con)
  (console-put-char-ex con
                       (position-x posn) (position-y posn)
                       #\.
                       color-white color-dark-blue))

(: move (-> Integer Integer Position GameState Position))
(define (move dx dy current-posn state)
  (define map (game-state-map state))
  (define-values (x y)
    (values (+ (position-x current-posn) dx)
            (+ (position-y current-posn) dy)))
  (: t Tile)
  (define t (array-ref map `#(,y ,x)))

  (if (not (tile-blocked t))
   (block
    (define new-posn (position x y))
    (log-debug (format "New object position: ~s" new-posn))
    new-posn)
   current-posn))

(: move-player (-> Position GameState Position))
(define (move-player current-posn state)
  (cond
    [(console-is-key-pressed 'UP)
     (move 0 -1 current-posn state)]
    [(console-is-key-pressed 'DOWN)
     (move 0 1 current-posn state)]
    [(console-is-key-pressed 'LEFT)
     (move -1 0 current-posn state)]
    [(console-is-key-pressed 'RIGHT)
     (move 1 0 current-posn state)]
    [else current-posn]))

(: handle-keys (-> GameState GameState))
(define (handle-keys state)
  (define-values (event key mouse) (sys-wait-for-event 'KEY #t))
  (define vk (key-vk key))
  
  ;(log-debug (format "EVENT: ~s" event))
  ;(log-debug (format "KEY EVENT: ~s - ~s" vk (key-c key)))

  (cond
    [(equal? vk 'ESCAPE)
     (struct-copy game-state state [exit #t])]
    [(member vk '(UP DOWN LEFT RIGHT))
     (define current-posn (object-position (game-state-a-player state)))
     (define new-player
       (struct-copy player default-player [position #:parent object
                                                (move-player current-posn state)]))
     (struct-copy game-state state [a-player new-player])]
    [else state]))

(: game-loop (-> GameState Void))
(define (game-loop state)
  (let ([closed? (console-is-window-closed)])
    (unless closed?
      (render-all state offscreen-console)
      (console-flush)

      ;(log-debug "Clear old position of objects")
      (define a-player (game-state-a-player state))
      (for-each (lambda (#{obj : Object})
                  (clear (object-position obj) offscreen-console))
                (cons a-player (game-state-objects state)))

      (game-loop (let ([new-state (handle-keys state)])
                   (if (game-state-exit new-state)
                       (exit)
                       new-state))))))

(log-debug "Set window title")
(console-set-window-title "Testerlie")

(log-debug "Start game loop")
(game-loop (game-state default-player #f (list default-npc) (make-map)))
