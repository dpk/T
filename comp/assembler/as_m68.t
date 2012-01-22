(herald (assembler as_m68 t 0)
        (env t (assembler as)))

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


;;; 68000 machine information
                         
(define *m68-machine* (make-machine))

;;; Create 68000 lap environment under orbit.

(new-lap-env *m68-machine* orbit-env '*m68-lap-env*)

(define (*define-lap-m68 sym val) 
   (*define-lap *m68-machine* sym val))

;;; Lap interface for the compiler

(define (losing-m68-process-lap-list items)
    (if (null? *current-ib*) (emit-tag (generate-symbol 'lap-entry)))
    (process-lap-list items *m68-machine*))

(define lap-transduce losing-m68-process-lap-list)

;;; For testing lap

(define-syntax (m68lap . items)  
  `(test-lap ',items *m68-machine*))

;;; Some quick definitions for what follows

(define *m68-pseudo-operands* 
   (pseudos-alist 
      ((label tag)    (m68/label tag))   
      ((template tag) (m68/label tag))   
      ((to tag)       (data-current-label tag))
      ((static id)    (static id))
      ))

;++ fix the damn compiler!!          

(define *m68-pseudo-ops* 
  (append!
   (pseudos-alist 
       ((j=  1tag) (m68-lap-jbcc jump-op/j=  1tag))
       ((jn= 1tag) (m68-lap-jbcc jump-op/jn= 1tag))
       ((j>  1tag) (m68-lap-jbcc jump-op/j>  1tag))
       ((j>= 1tag) (m68-lap-jbcc jump-op/j>= 1tag))
       ((j<  1tag) (m68-lap-jbcc jump-op/j<  1tag))
       ((j<= 1tag) (m68-lap-jbcc jump-op/j<= 1tag))
       ((uj>  1tag) (m68-lap-jbcc jump-op/uj>  1tag))
       ((uj>= 1tag) (m68-lap-jbcc jump-op/uj>= 1tag))
       ((uj<  1tag) (m68-lap-jbcc jump-op/uj<  1tag))
       ((uj<= 1tag) (m68-lap-jbcc jump-op/uj<= 1tag))
       ((jneg 1tag) (m68-lap-jbcc jump-op/negative 1tag))
       ((jpos 1tag) (m68-lap-jbcc jump-op/not_negative 1tag))
       ((jbr 1tag) (m68-lap-jbcc jump-op/jabs 1tag))
     )
    (pseudos-alist
       ((space number)    (m68/space number))
       ;; loser
       ((jump . args)     (apply emit-jump args))       ; -- state machine, ibs
       ;; loser
;       ((template . args) (apply %emit-template args))  ; -- state machine, ib
       ((byte number) (m68/byte number))
       ((word number) (m68/word number))
       ((long number) (m68/long number))
       ((block . forms) (eval `(block ,@forms) (machine-lap-env *m68-machine*)))
       ((equate id form) (*define-lap-m68 id (eval form (machine-lap-env *m68-machine*))))
       )))

(define (m68-lap-jbcc jump-op 1tag)
  (let ((next-tag (generate-symbol 'm68-lap-jbcc)))
    (cond ((not (symbol? 1tag))
           (error "j pseudo ops expect a symbol")))
    (emit-jump-to-ib *current-ib* jump-op 1tag next-tag)
    (emit-tag next-tag)))

(*define-lap-m68 'number make-as-number)
    
;;; Losing near-parameterizations.

(define (losing-m68-emit opcode-fg . operands)
  (emit-to-ib *current-ib* (apply opcode-fg operands))
  (flush-delayed-comments))

(define (m68emit fg)
  (emit-to-ib *current-ib* fg)
  (flush-delayed-comments))

(define %emit losing-m68-emit)

(set *pretty-print-tag* pp-ib-as-name-or-hash)
(set *current-machine* *m68-machine*) 

;;; Set machine parameters.

;;; These are in M68IS.
;(set (machine-template-emitter *m68-machine*) emit-m68-template)
;(set (machine-cond-branch      *m68-machine*) m68/jbcc)
;(set (machine-uncond-branch    *m68-machine*) m68/jbra)         

(set (machine-clump-size       *m68-machine*) 16)
(set (machine-maximum-clumps   *m68-machine*) 5)
(set (machine-clump-writer     *m68-machine*) m68/write-clumps)
(set (machine-pseudo-ops       *m68-machine*) *m68-pseudo-ops*)
(set (machine-pseudo-operands  *m68-machine*) *m68-pseudo-operands*)

;;; Handy items for lap env

(walk *define-lap-m68
      '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15)
      '( 0  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15)) 

(walk *define-lap-m68
      '(.d0 .d1 .d2 .d3 .d4 .d5 .d6 .d7 .a0 .a1 .a2 .a3 .a4 .a5 .a6 .a7)
      '(0    1   2   3   4   5    6  7   8   9  10  11  12  13  14  15))
                                     
(walk *define-lap-m68
      '(S0  S1  S2  S3  S4  S5 SCRATCH  nil-reg P   A1  A2  A3  AN  TP  TASK  SP)
      '(0    1   2   3   4   5       6    7     8   9  10  11  12  13    14  15))

(walk *define-lap-m68
      '(.b .w .l)
      '(b w l))

(define .b 'b)
(define .w 'w)
(define .l 'l)

