#lang typed/racket

(provide any-fighter-being-attacked?
         fighter-attack-target
         fighter-being-attacked?
         )

(require "message-log.rkt"
         "types.rkt"
         )

(: any-fighter-being-attacked? (-> Integer Integer (Listof GameObject) (Option GameObject)))
(define (any-fighter-being-attacked? attacker-x attacker-y objects)
  (for/or : (Option GameObject) ([t : GameObject objects])
    (if (fighter-being-attacked? attacker-x attacker-y t)
        t
        #f)))

(: fighter-being-attacked? (-> Integer Integer GameObject Boolean))
(define (fighter-being-attacked? attacker-x attacker-y target)
  (define-values (target-x target-y) (values (game-object-x target)
                                             (game-object-y target)))
  (and (= attacker-x target-x) (= attacker-y target-y)
       (not (false? (game-object-fighter target)))))

(: fighter-deal-damage (-> Integer Fighter Fighter))
(define (fighter-deal-damage damage target-fighter)
  (struct-copy fighter target-fighter [hp (- (fighter-hp target-fighter)
                                             damage)]))

(: fighter-attack-target (-> GameObject GameObject GameObject))
(define (fighter-attack-target attacker-object target-object)
  (define attacker (cast (game-object-fighter attacker-object) Fighter))
  (define target (cast (game-object-fighter target-object) Fighter))
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
                                  [fighter new-target])])]
        [else
         (message-add (format "~s attacks ~s but is has no effect!"
                              (game-object-name attacker-object)
                              (game-object-name target-object)))
         target-object]))
