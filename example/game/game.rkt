#lang racket

(require racket/format
         threading

         "../../color.rkt"
         "../../console.rkt"
         "../../fov.rkt"
         "../../mouse.rkt"
         "../../sys.rkt"

         "ai.rkt"
         "game-constants.rkt"
         "game-object.rkt"
         "map.rkt"
         "message-log.rkt"
         "player.rkt"
         "types.rkt"
         )


;;;
;;; Constants
;;;

(define FOV-ALGO 0)
(define FOV-LIGHT-WALLS #t)
(define TORCH-RADIUS 10)

;;;
;;; Globals
;;;

(define game-logger (make-logger))
(define root-console (console-init-root SCREEN-WIDTH SCREEN-HEIGHT
                                        "Example"
                                        #f
                                        'RENDERER_SDL))
; Used for drawing the state before it gets copied to the actual root console
(define offscreen-console (console-new MAP-WIDTH MAP-HEIGHT))
; A status panel
(define panel (console-new SCREEN-WIDTH PANEL-HEIGHT))

;;;
;;; Colors
;;;

(define color-dark-wall (make-color 0 0 100))
(define color-light-wall (make-color 130 110 50))
(define color-dark-ground (make-color 50 50 150))
(define color-light-ground (make-color 200 180 50))


(define (check-for-input)
  (sys-check-for-event 'KEY_PRESS_MOUSE_MOVE))

(define (process-input state)
  (define-values (event key mouse) (check-for-input))
  (define key-pressed (key-vk key))

  ;(log-debug "Process Input")
  ;(log-debug (format "Event: ~v --- Key: ~v --- Mouse: ~v" event key-pressed mouse))

  (define exit? (equal? 'ESCAPE key-pressed))
  (define action (if (or (eq? 'NONE event)
                         (eq? 'NONE key-pressed))
                     'no-turn 'turn))
  (struct-copy game-state state
               [input (game-input event key mouse)] [exit exit?] [action action]))

(define (update-player state)
  (define player (game-state-player state))
  (cond [(equal? 'no-turn (game-state-action state)) state]
        [(game-object-dead? player) state]
        [else 
         (struct-copy game-state (~> player
                                     (player-handle-input (game-state-input state) state)
                                     (player-update state))
                      [fov-recompute? #t])]))

(define (update-objects state)
  (define (updated? obj) (game-object-turn-taken? obj))

  (define (updater current-object updated-objects latest-state)
    (cond [(null? current-object) latest-state]
          [else
           (define-values (new-object new-state)
             (~> current-object
                 (ai-handle-state-transition latest-state)
                 (ai-update latest-state)))
           (define objects-to-update
             (filter-not updated? (game-state-objects new-state)))

           (if (null? objects-to-update)
               (struct-copy game-state new-state
                            [objects (cons new-object updated-objects)])
               (updater (first objects-to-update)
                     (cons new-object updated-objects)
                     (struct-copy game-state new-state
                                  [objects (append updated-objects
                                                   (rest objects-to-update))])))]))

  (define current-objects (filter-map
                           (lambda (obj)
                             (and (game-object-alive? obj)
                                  (struct-copy game-object obj [turn-taken? #f])))
                           (game-state-objects state)))
  (define dead-objects (append (game-state-dead state)
                               (filter (lambda (obj)
                                         (game-object-dead? obj))
                                       (game-state-objects state))))
  (if (not (eq? 'turn (game-state-action state)))
      state
      (updater (first current-objects)
               '()
               (struct-copy game-state state
                            [objects (rest current-objects)]
                            [dead dead-objects]))))

(define (update state)
  (~> state
      update-player
      update-objects
      ))

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

(define (render-game-object a-object fov-map)
  (define x (game-object-x a-object))
  (define y (game-object-y a-object))

  (when (map-is-in-fov fov-map x y)
    (console-set-default-foreground offscreen-console
                                    (game-object-color a-object))
    (console-put-char offscreen-console
                      x y
                      (game-object-char a-object)
                      'BKGND_NONE)))

(define (clear x y fov-map)
  (when (map-is-in-fov fov-map x y)
    (console-put-char offscreen-console x y #\. 'BKGND_NONE)))

(define (clear-object-positions player fov-map objects)
  ;(log-debug "Clear old position of objects")
  (for-each (lambda (obj)
              (clear (game-object-x obj) (game-object-y obj) fov-map))
            objects))

(define (names-under-mouse input objects fov-map)
  (define mouse (game-input-mouse input))
  (define-values (x y) (values (mouse-cx mouse) (mouse-cy mouse)))
  (define names
    (for/fold ([ns '()])
              ([obj objects])
      (define obj-x (game-object-x obj))
      (define obj-y (game-object-y obj))
      (if (and (= obj-x x) (= obj-y y)
               (map-is-in-fov fov-map obj-x obj-y))
          (cons (game-object-name obj) ns)
          ns)))

  (string-join names ", "))

(define (render state)
  (define a-map (game-state-map state))
  (define fov-map (game-state-fov-map state))
  (define (visible? x y) (map-is-in-fov fov-map x y))
  (define player (game-state-player state))
  (define player-x (game-object-x player))
  (define player-y (game-object-y player))

  ;(log-debug "Render tiles")
  (when (game-state-fov-recompute? state)
    (map-compute-fov fov-map player-x player-y TORCH-RADIUS FOV-LIGHT-WALLS FOV-ALGO)
    (clear-object-positions player fov-map (game-state-objects state))
    (for ([y MAP-HEIGHT])
      (for ([x MAP-WIDTH])
        (define a-tile (map-ref a-map x y))
        (cond
          [(not (visible? x y))
           (when (tile-explored? a-tile)
             (if (tile-wall? a-tile)
                 (console-put-char-ex offscreen-console
                                      x y
                                      #\#
                                      color-dark-ground
                                      color-dark-blue)
                 (console-put-char-ex offscreen-console
                                      x y
                                      #\.
                                      color-dark-ground
                                      color-dark-blue)))]
          [else
           (cond [(tile-wall? a-tile)
                  (console-put-char-ex offscreen-console
                                       x y
                                       #\#
                                       color-white color-light-wall)]
                 [else
                  (console-put-char-ex offscreen-console x y
                                       #\.
                                       color-white color-light-ground)])
           (set-tile-explored?! a-tile #t)]))))

  ;(log-debug "Render dead objects")
  (for-each (lambda (obj) (render-game-object obj fov-map))
            (game-state-dead state))

  ;(log-debug "Render objects")
  (for-each (lambda (obj) (render-game-object obj fov-map))
            (game-state-objects state))

  ;(log-debug "Render player")
  (render-game-object player fov-map)

  ;(log-debug "Render panel components")
  (console-set-default-background panel color-black)
  (console-clear panel)

  (define player-fighter (game-object-fighter player))
  (render-bar panel
              1 1
              BAR-WIDTH
              "HP"
              (fighter-hp player-fighter)
              (fighter-max-hp player-fighter)
              color-light-red color-darker-red)

  ;(log-debug "Render object names under mouse cursor")
  (console-set-default-foreground panel color-light-gray)
  (console-print-ex panel
                    1 0
                    'BKGND_NONE 'LEFT
                    (names-under-mouse (game-state-input state)
                                       (game-state-objects state)
                                       fov-map))

  ;(log-debug "Render message log")
  (define messages (message-log-entries))
  (for ([y (length messages)]
        [m messages])
    ;(log-debug "Render message: ~v in color ~v at ~v" (car m) (cdr m) y)
    (console-set-default-foreground panel (cdr m))
    (console-print-ex panel MSG-X (add1 y) 'BKGND_NONE 'LEFT (car m)))

  (console-blit panel 0 0 SCREEN-WIDTH PANEL-HEIGHT console-root 0 PANEL-Y)

  ;(log-debug "Render to root-console")
  (console-blit offscreen-console 0 0 MAP-WIDTH MAP-HEIGHT console-root 0 0)
  (console-flush)

  (struct-copy game-state state [fov-recompute? #f]))

(define (game-loop state)
  (unless (console-is-window-closed)
    (define new-state
      (~> state
          process-input
          update
          render
          ))

    (if (game-state-exit new-state)
        (exit)
        (game-loop new-state))))

;(log-debug "Initialize game")
(sys-set-fps 60)
(define initial-game-state
  (let-values ([(a-map objects player-start-position) (make-map)])
    (define player (make-player (car player-start-position)
                                (cdr player-start-position)
                                "namra"))
    (define fov-map (make-fov-map MAP-WIDTH MAP-HEIGHT a-map))
    (make-game-state player objects a-map fov-map)))

;(log-debug "Enter game loop")
(message-add "Welcome stranger! Prepare to perish in the Tombs of the Ancient Kings."
             #:color color-red)
(game-loop initial-game-state)
