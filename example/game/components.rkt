#lang racket

(provide any-fighter-being-attacked?
         fighter-attack-target
         fighter-being-attacked?
         )

(require "message-log.rkt"
         "types.rkt"
         )

(define (any-fighter-being-attacked? attacker-x attacker-y entities)
  (for/or ([t entities])
    (if (fighter-being-attacked? attacker-x attacker-y t)
        t
        #f)))

(define (fighter-being-attacked? attacker-x attacker-y target)
  (define-values (target-x target-y) (values (entity-x target)
                                             (entity-y target)))
  (and (= attacker-x target-x) (= attacker-y target-y)
       (not (false? (entity-fighter target)))))

(define (fighter-deal-damage damage target-fighter)
  (struct-copy fighter target-fighter [hp (- (fighter-hp target-fighter)
                                             damage)]))

(define (fighter-attack-target attacker-entity target-entity)
  (define attacker (entity-fighter attacker-entity))
  (define target (entity-fighter target-entity))
  (define damage (- (fighter-power attacker)
                    (fighter-defense target)))

  (cond [(> damage 0)
         (message-add (format "~s attacks ~s for ~v hit points."
                              (entity-name attacker-entity)
                              (entity-name target-entity)
                              damage))
         (define new-target (fighter-deal-damage damage target))
         ; TODO: Maybe here we can use a lens-transform with lens-compose
         (cond [(> (fighter-hp new-target) 0)
                (struct-copy entity target-entity [fighter new-target])]
               [else (struct-copy entity
                                  ((fighter-die new-target) target-entity)
                                  [fighter (struct-copy fighter new-target
                                                        [hp 0])])])]
        [else
         (message-add (format "~s attacks ~s but is has no effect!"
                              (entity-name attacker-entity)
                              (entity-name target-entity)))
         target-entity]))
