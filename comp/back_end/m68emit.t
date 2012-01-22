(herald m68emit)

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
          (generate-move-address (car ref1) ref2)
          (emit m68/move .l ref1 ref2))))

(define-integrable (generate-move-word ref1 ref2)
  (if (neq? ref1 ref2)
      (emit m68/move .w ref1 ref2)))

(define-integrable (generate-move-byte ref1 ref2)
  (if (neq? ref1 ref2)
      (emit m68/move .b ref1 ref2)))

(define (generate-push access)
  (increment-stack)
  (if (and (pair? access) (null? (cdr access)))
      (emit m68/pea (car access))
      (emit m68/move .l access (@-r 15))))

(define-integrable (generate-pop access)
  (emit m68/move .l (@r+ 15) access))


(define (adjust-stack-pointer n)
  (if (fxn= n 0) (emit m68/lea (d@r 15 n) SP)))

                                     
(define (generate-move-address from to)
  (cond ((register? to)
         (if (or (atom? from)
                 (neq? (car from) to)
                 (neq? (cdr from) 0))
             (emit m68/lea from to)))
        ((locked? AN)
         (emit m68/pea from)
         (generate-pop to))
        (else
         (emit m68/lea from AN)
         (emit m68/move .l AN to))))
               
(define-integrable (generate-slink-jump offset)
  (emit m68/jsr (*d@nil offset)))


(define-integrable (generate-jump-to-subroutine fg)
  (emit m68/jsr fg))
                   
(define-integrable (generate-jump-absolute fg)
  (emit m68/jmp fg))
                   
(define-integrable (generate-jump label)
  (emit-jump 'jmp label nil))

(define-integrable (generate-avoid-jump label)
  (emit-avoid-jump 'jmp label nil))

(define (generate-return n-args)               
  (emit m68/move .l (machine-num (fx- -1 n-args)) NARGS)
  (emit m68/move .l (@r 15) TP)
  (emit m68/jmp (@r 13)))

(define (generate-return-without-nargs)
  (emit m68/move .l (@r 15) TP)
  (emit m68/jmp (@r 13)))


(define (generate-general-call proc-var n-args)
  (emit m68/move .l  (machine-num (fx+ n-args 1)) NARGS)
  (cond ((and (or (variable-binder proc-var)
		  (var-is-vcell? proc-var)))
	 (emit m68/jmp (*d@nil slink/icall)))
	(else
         (emit m68/move .l (reg-offset P -2) TP)
         (emit m68/jmp (@r 13)))))

    
(define-integrable (generate-push-address access)
  (increment-stack)
  (emit m68/pea access))

      
(define-integrable (increment-stack)
  (set *stack-pos* (fx+ *stack-pos* CELL)))

(define-integrable (n-decrement-stack n)
  (set *stack-pos* (fx- *stack-pos* (fx* n CELL))))

(define (emit op . args)
  (m68emit (apply op (map! ->field-group args))))


(define (indexer address offset data)
  (cons (cons address data) offset))

(define (->field-group operand)
  (cond ((fg? operand) operand)
        ((fixnum? operand)
         (register->field-group operand))
        ((atom? operand) operand)
        ((fg? (car operand)) operand)
        ((fixnum? (car operand))
         (d@r (symbolic->machine-reg (car operand)) (cdr operand)))
        (else
         (index (d@r (symbolic->machine-reg (caar operand)) (cdr operand))
                (symbolic->machine-reg (cdar operand))))))

(define (symbolic->machine-reg reg)
  (cond ((fx< reg 0)
         (vref *reserved-registers* (fx- 0 reg)))
        ((fx>= reg 6) (fx+ reg 2))
        (else reg)))

(define (register->field-group reg)
  (cond ((fx< reg 0)
         (r (vref *reserved-registers* (fx- 0 reg))))
        ((fx< reg 6)
         (r reg))   
        ((fx< reg *real-registers*)
         (r (fx+ reg 2)))
        (else
         (d@r 14 (fx* (fx- reg *real-registers*) CELL)))))


(define *reserved-registers*
  '#(nil 13 7   15 14    6))
;        TP nil SP TASK SCRATCH


(define (emit-hacked-branch jump-op displ)
  (%emit m68/hack-jbcc (jump-op->m68-cc jump-op) displ))