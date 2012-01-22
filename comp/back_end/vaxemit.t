(herald vaxemit)

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.
;;; This material was developed by the T Project at the Yale University Computer 
;;; Science Department.  Permission to copy this software, to redistribute it, 
;;; and to use it for any purpose is granted, subject to the following restric-
;;; tions and understandings.
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;


(define (generate-move ref1 ref2)
  (if (neq? ref1 ref2)
      (if (and (pair? ref1) (null? (cdr ref1)))
          (emit vax/moval (car ref1) ref2)
          (emit vax/movl ref1 ref2))))

(define (generate-push access)
  (increment-stack)
  (if (and (pair? access) (null? (cdr access)))
      (emit vax/pushal (car access))
      (emit vax/pushl access )))

(define-integrable (generate-pop access)
  (emit vax/movl (@r+ SP 0) access))

(define (adjust-stack-pointer n)
  (if (fxn= n 0) (emit vax/addl2 ($ n) SP)))
                                     
(define (generate-move-address from to)
  (cond ((register? to)
         (if (or (atom? from)
                 (neq? (car from) to)
                 (neq? (cdr from) 0))
             (emit vax/moval from to)))
        (else
         (emit vax/moval from to))))

(define-integrable (generate-slink-jump offset)
  (emit vax/jsb (*d@r 11 offset)))
  
(define-integrable (generate-jump-to-subroutine fg)
  (emit vax/jsb fg))
                   
(define-integrable (generate-jump-absolute fg)
  (emit vax/jmp fg))
                   
(define (generate-jump label)
  (emit-jump 'jmp label nil))

(define (generate-avoid-jump label)
  (emit-avoid-jump 'jmp label nil))

(define (generate-return n-args)               
  (emit vax/mnegl (machine-num (fx+ 1 n-args)) NARGS)
  (emit vax/movl (@r 14) TP)
  (emit vax/jmp (@r 10)))

(define (generate-return-without-nargs)
  (emit vax/movl (@r 14) TP)
  (emit vax/jmp (@r 10)))


(define (generate-general-call proc-var n-args)
  (emit vax/movl  (machine-num (fx+ n-args 1)) NARGS)
  (cond ((and (or (variable-binder proc-var)
		  (var-is-vcell? proc-var)))
         (emit vax/jmp (*d@r 11 slink/icall)))
        (else
         (emit vax/movl (d@r P -2) TP)
         (emit vax/jmp (@r 10)))))
    
    
(define-integrable (generate-push-address access)
  (increment-stack)
  (emit vax/pushal access))

      
(define-integrable (increment-stack)
  (set *stack-pos* (fx+ *stack-pos* CELL)))

(define-integrable (n-decrement-stack n)
  (set *stack-pos* (fx- *stack-pos* (fx* n CELL))))

(define (emit op . args)
  (apply %emit op (map! ->field-group args)))

(define (->field-group operand)
  (cond ((fg? operand) operand)
        ((fixnum? operand)
         (register->field-group operand))
        ((fixnum? (car operand))
         (d@r (symbolic->machine-reg (car operand)) (cdr operand)))
        (else
         (index (d@r (symbolic->machine-reg (caar operand)) (cdr operand))
                (symbolic->machine-reg (cdar operand))))))

(define (symbolic->machine-reg reg)
  (cond ((fx< reg 0)
         (vref *reserved-registers* (fx- 0 reg)))
        (else reg)))

(define (register->field-group reg)
  (cond ((fx< reg 0)
         (r (vref *reserved-registers* (fx- 0 reg))))
        ((fx< reg *real-registers*)
         (r reg))
        (else
         (d@r 12 (fx* (fx- reg *real-registers*) CELL)))))


(define *reserved-registers*
  '#(nil 10 11 14 12))
;        TP nil SP TASK


