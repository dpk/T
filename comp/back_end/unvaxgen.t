(herald unvaxgen)
                             
;;; we can do unsafe things here once we set the foreign call cont

(define (generate-foreign-call node)
  (destructure (((#f foreign rep-list value-rep . args) (call-args node)))
    (emit vax/movl SP (reg-offset TASK task/foreign-call-cont))
    (emit vax/movl TASK (reg-offset nil-reg slink/current-task))
    (iterate loop ((args (reverse args)) 
                   (reps (map cadr (leaf-value rep-list)))
                   (count 0))
      (cond ((null? args)
             (walk (lambda (node) (kill (leaf-value node))) args)
             (let ((reg (->register 'pointer node (leaf-value foreign) '*)))
               (emit vax/movl (reg-offset reg 6) reg)
               (emit vax/calls (machine-num count) (reg-offset reg 0))))
             ((eq? (car reps) 'rep/double) 
              (->register 'scratch node (leaf-value (car args)) S0) 
              (generate-push (reg-offset S0 6))
              (generate-push (reg-offset S0 2))
              (loop (cdr args) (cdr reps) (fx+ count 2)))
             (else
              (rep-push node (leaf-value (car args)) (car reps))
              (loop (cdr args) (cdr reps) (fx+ count 1)))))
    (case (leaf-value value-rep)
      ((rep/undefined ignore))
      ((rep/double)
       (emit vax/movl S1 S3)                            ; save high longword
       (emit vax/movl (machine-num 8) S1)            ; 2 words for double
       (emit vax/movl (machine-num header/double-float) AN)
       (generate-slink-jump slink/make-extend)
       (emit vax/movl S3 (reg-offset AN 6))
       (emit vax/movl S0 (reg-offset AN 2))
       (emit vax/movl AN A1))                         ; return consed flonum
      (else
       (really-rep-convert node S0 (leaf-value value-rep) A1 'rep/pointer)))
    (emit vax/clrl (reg-offset TASK task/foreign-call-cont))))
 


