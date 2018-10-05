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
         "entity.rkt"
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
        [(entity-dead? player) state]
        [else 
         (struct-copy game-state (~> player
                                     (player-handle-input (game-state-input state) state)
                                     (player-update state))
                      [fov-recompute? #t])]))

(define (update-entities state)
  (define (updated? enty) (entity-turn-taken? enty))

  (define (updater current-entity updated-entities latest-state)
    (cond [(null? current-entity) latest-state]
          [else
           (define-values (new-entity new-state)
             (~> current-entity
                 (ai-handle-state-transition latest-state)
                 (ai-update latest-state)))
           (define entities-to-update
             (filter-not updated? (game-state-entities new-state)))

           (if (null? entities-to-update)
               (struct-copy game-state new-state
                            [entities (cons new-entity updated-entities)])
               (updater (first entities-to-update)
                     (cons new-entity updated-entities)
                     (struct-copy game-state new-state
                                  [entities (append updated-entities
                                                    (rest entities-to-update))])))]))

  (define current-entities
    (filter-map (lambda (enty) (and (entity-alive? enty)
                                    (struct-copy entity enty [turn-taken? #f])))
                (game-state-entities state)))
  (define dead-entities
    (append (game-state-dead state)
            (filter (lambda (enty) (entity-dead? enty))
                    (game-state-entities state))))
  (if (not (eq? 'turn (game-state-action state)))
      state
      (updater (first current-entities)
               '()
               (struct-copy game-state state
                            [entities (rest current-entities)]
                            [dead dead-entities]))))

(define (update state)
  (~> state
      update-player
      update-entities
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

(define (render-entity an-entity fov-map)
  (define x (entity-x an-entity))
  (define y (entity-y an-entity))

  (when (map-is-in-fov fov-map x y)
    (console-set-default-foreground offscreen-console
                                    (entity-color an-entity))
    (console-put-char offscreen-console
                      x y
                      (entity-char an-entity)
                      'BKGND_NONE)))

(define (clear x y fov-map)
  (when (map-is-in-fov fov-map x y)
    (console-put-char offscreen-console x y #\. 'BKGND_NONE)))

(define (clear-object-positions player fov-map entities)
  ;(log-debug "Clear old position of entities")
  (for-each (lambda (enty)
              (clear (entity-x enty) (entity-y enty) fov-map))
            entities))

(define (names-under-mouse input entities fov-map)
  (define mouse (game-input-mouse input))
  (define-values (x y) (values (mouse-cx mouse) (mouse-cy mouse)))
  (define names
    (for/fold ([ns '()])
              ([enty entities])
      (define enty-x (entity-x enty))
      (define enty-y (entity-y enty))
      (if (and (= enty-x x) (= enty-y y)
               (map-is-in-fov fov-map enty-x enty-y))
          (cons (entity-name enty) ns)
          ns)))

  (string-join names ", "))

(define (render state)
  (define a-map (game-state-map state))
  (define fov-map (game-state-fov-map state))
  (define (visible? x y) (map-is-in-fov fov-map x y))
  (define player (game-state-player state))
  (define player-x (entity-x player))
  (define player-y (entity-y player))

  ;(log-debug "Render tiles")
  (when (game-state-fov-recompute? state)
    (map-compute-fov fov-map player-x player-y TORCH-RADIUS FOV-LIGHT-WALLS FOV-ALGO)
    (clear-object-positions player fov-map (game-state-entities state))
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

  ;(log-debug "Render dead entites")
  (for-each (lambda (enty) (render-entity enty fov-map))
            (game-state-dead state))

  ;(log-debug "Render alive entities")
  (for-each (lambda (enty) (render-entity enty fov-map))
            (game-state-entities state))

  ;(log-debug "Render player")
  (render-entity player fov-map)

  ;(log-debug "Render panel components")
  (console-set-default-background panel color-black)
  (console-clear panel)

  (define player-fighter (entity-fighter player))
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
                                       (game-state-entities state)
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
  (let-values ([(a-map entities player-start-position) (make-map)])
    (define player (make-player (car player-start-position)
                                (cdr player-start-position)
                                "namra"))
    (define fov-map (make-fov-map MAP-WIDTH MAP-HEIGHT a-map))
    (make-game-state player entities a-map fov-map)))

;(log-debug "Enter game loop")
(message-add "Welcome stranger! Prepare to perish in the Tombs of the Ancient Kings."
             #:color color-red)
(game-loop initial-game-state)
