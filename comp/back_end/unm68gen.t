(herald unm68gen)

;;; we can do unsafe things here once we set the foreign call cont

(define (generate-foreign-call node)
  (destructure (((cont foreign rep-list value-rep . args) (call-args node)))
    (emit m68/move .l SP (reg-offset TASK task/foreign-call-cont))
    (generate-push nil-reg)   ; save slink
    (emit m68/move .l TASK (d@nil slink/current-task))
    (iterate loop ((args (reverse args)) 
                   (reps (map cadr (leaf-value rep-list))))
      (cond ((null? args)
             (walk (lambda (node) (kill (leaf-value node))) args))
            ((eq? (car reps) 'rep/double)
             (let ((reg (->register 'pointer node (leaf-value (car args)) '*)))
               (emit m68/move .l (reg-offset reg 6) (@-r 15))
               (emit m68/move .l (reg-offset reg 2) (@-r 15))
               (loop (cdr args) (cdr reps))))
            (else
             (rep-push node (leaf-value (car args)) (car reps))
             (loop (cdr args) (cdr reps)))))
    (let ((reg (->register 'pointer node (leaf-value foreign) '*))) ; get xenoid
      (emit m68/move .l (reg-offset reg 6) P))  ; P must be A0, get 2nd slot
    (emit m68/jsr (@r 8))   ; a0 = P
    (clear-slots)
    (emit m68/move .l (reg-offset TASK task/foreign-call-cont) SP)
    (emit m68/move .l (reg-offset sp -4) nil-reg)  ; restore slink
    (emit m68/clr .l (reg-offset TASK task/foreign-call-cont))
    (case (leaf-value value-rep)
      ((rep/undefined ignore))                                        
      ((rep/double)                                     ; cons a flonum
       (emit m68/move .l S1 SCRATCH)                    ; save hign longword
       (emit m68/move .l (machine-num 8) S1)            ; 2 words for double
       (emit m68/move .l (machine-num header/double-float) AN)
       (generate-slink-jump slink/make-extend)
       (emit m68/move .l SCRATCH (reg-offset AN 6))
       (emit m68/move .l S0 (reg-offset AN 2))
       (emit m68/move .l AN A1))                         ; return consed flonum
      (else
       (really-rep-convert node S0 (leaf-value value-rep) A1 'rep/pointer)))
    (generate-return 1)))
