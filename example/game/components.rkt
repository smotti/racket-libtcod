#lang racket

(provide any-fighter-being-attacked?
         fighter-attack-target
         fighter-being-attacked?
         )

(require "message-log.rkt"
         "types.rkt"
         )

(define (any-fighter-being-attacked? attacker-x attacker-y objects)
  (for/or ([t objects])
    (if (fighter-being-attacked? attacker-x attacker-y t)
        t
        #f)))

(define (fighter-being-attacked? attacker-x attacker-y target)
  (define-values (target-x target-y) (values (game-object-x target)
                                             (game-object-y target)))
  (and (= attacker-x target-x) (= attacker-y target-y)
       (not (false? (game-object-fighter target)))))

(define (fighter-deal-damage damage target-fighter)
  (struct-copy fighter target-fighter [hp (- (fighter-hp target-fighter)
                                             damage)]))

(define (fighter-attack-target attacker-object target-object)
  (define attacker (game-object-fighter attacker-object))
  (define target (game-object-fighter target-object))
  (define damage (- (fighter-power attacker)
                    (fighter-defense target)))

  (cond [(> damage 0)
         (message-add (format "~s attacks ~s for ~v hit points."
                              (game-object-name attacker-object)
                              (game-object-name target-object)
                              damage))
         (define new-target (fighter-deal-damage damage target))
         (cond [(> (fighter-hp new-target) 0)
                (struct-copy game-object target-object [fighter new-target])]
               [else (struct-copy game-object
                                  ((fighter-die new-target) target-object)
                                  [fighter (struct-copy fighter new-target
                                                        [hp 0])])])]
        [else
         (message-add (format "~s attacks ~s but is has no effect!"
                              (game-object-name attacker-object)
                              (game-object-name target-object)))
         target-object]))
