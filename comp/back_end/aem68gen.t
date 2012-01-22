(herald aem68gen)

(define (generate-foreign-call node)
  (destructure (((cont foreign rep-list value-rep . arg-list) (call-args node)))
    (let ((reps (leaf-value rep-list))
          (value-rep (leaf-value value-rep))
          (args (reverse arg-list)))
      (emit m68/move .l SP (reg-offset TASK task/foreign-call-cont))
      (generate-push nil-reg)   ; save slink
      (generate-move nil-reg AN)
      (emit m68/move .l TASK (reg-offset AN slink/current-task))
      (walk (lambda (arg rep)               ; rep is (type rep name)
              (case (cadr rep)
                ((rep/extend rep/string rep/value))
                ((rep/string-pointer)
                 (rep-push node (leaf-value arg) 'rep/string))
                ((rep/extend-pointer)
                 (rep-push node (leaf-value arg) 'rep/extend))
                (else
                 (rep-push node (leaf-value arg) (cadr rep)))))
            arg-list
            (reverse reps))
      (iterate loop ((reps reps) (args args) (slots-used 0) (pos 0))
        (cond ((null? reps))
              (else
               (case (cadar reps)
                 ((rep/extend rep/string rep/value)
                  (rep-push node (leaf-value (car args)) (cadar reps))
                  (loop (cdr reps) (cdr args) slots-used (fx+ pos 4)))
                 (else
                  (emit m68/pea (reg-offset SP (fx+ slots-used pos)))
                  (loop (cdr reps) (cdr args) (fx+ slots-used 4) (fx+ pos 4)))))))
                                               ;  TASK must be A6 which 
                                               ; is saved and restored by aegis
      (let ((reg (->register 'pointer node (leaf-value foreign) '*))) ; get xeno
        (emit m68/move .l (reg-offset reg 6) P))  ; P must be A0, get 2nd slot
      (emit m68/jsr (@r 8))   ; a0 = P
      (clear-slots)
      (receive (reg count) (case value-rep
                             ((ignore) (return A1 0))
                             ((rep/address)
                              (generate-move P SCRATCH)
                              (emit m68/asl .l (machine-num 2) SCRATCH)
                              (generate-move SCRATCH A1)
                              (lock A1)
                              (return A2 1))
                             (else
                              (really-rep-convert node S0 value-rep A1 'rep/pointer)
                              (lock A1)
                              (return A2 1)))
        (iterate loop ((args arg-list) (reg reg) (reps (reverse reps)) (pos 0) (count count))
          (cond ((null? args)
                 (clear-slots)
                 (emit m68/move .l (reg-offset TASK task/foreign-call-cont) SP)
                 (emit m68/move .l (reg-offset sp -4) nil-reg)  ; restore slink
                 (emit m68/clr .l (reg-offset TASK task/foreign-call-cont))
                 (generate-return count))            
                ((fx= reg AN)
                 (loop args (fx+ (fx+ AN 1) *argument-registers*) reps pos count))    
                (else
                 (case (caar reps)
                   ((ignore in)
                    (loop (cdr args) reg (cdr reps) (fx+ pos 4) count))
                   (else
                    (let ((rep (cadar reps)))
                      (case rep
                        ((rep/extend)
                         (really-rep-convert node (reg-offset SP pos) 
                                   rep reg 'rep/pointer)
                         (lock reg)
                         (loop (cdr args) (fx+ reg 1) (cdr reps) (fx+ pos 4)
                           (fx+ count 1)))
                        (else
                         (emit m68/move .l (reg-offset SP pos) P)
                         (really-rep-convert node (@r 8) rep reg 'rep/pointer)
                         (lock reg)
                         (loop (cdr args) (fx+ reg 1) (cdr reps) (fx+ pos 4)
                           (fx+ count 1))))))))))))))
