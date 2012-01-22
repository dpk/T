(herald (assembler as_vax t 0)
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


;;; VAX machine information
                         
(define *vax-machine* (make-machine))

;;; Create VAX lap environment under orbit.

(new-lap-env *vax-machine* orbit-env '*vax-lap-env*)

(define (*define-lap-vax sym val) 
   (*define-lap *vax-machine* sym val))

;;; Lap interface for the compiler

(define (losing-vax-process-lap-list items)
    (if (null? *current-ib*) (emit-tag (generate-symbol 'lap-entry)))
    (process-lap-list items *vax-machine*))

(define lap-transduce losing-vax-process-lap-list)

;;; For testing lap

(define-syntax (vaxlap . items)  
  `(test-lap ',items *vax-machine*))

;;; Some quick definitions for what follows

(define *vax-pseudo-operands* 
   (pseudos-alist 
      ((label tag)    (vax/label tag))   
      ((template tag) (vax/label tag))   
      ((to tag)       (data-current-label tag))
      ((static id)    (static id))
      ))

(define *vax-pseudo-ops* 
   (pseudos-alist 
       ((j=  1tag) (vax-lap-jbcc jump-op/j=  1tag))
       ((jn= 1tag) (vax-lap-jbcc jump-op/jn= 1tag))
       ((j>  1tag) (vax-lap-jbcc jump-op/j>  1tag))
       ((j>= 1tag) (vax-lap-jbcc jump-op/j>= 1tag))
       ((j<  1tag) (vax-lap-jbcc jump-op/j<  1tag))
       ((j<= 1tag) (vax-lap-jbcc jump-op/j<= 1tag))
       ((jbr 1tag) (vax-lap-jbcc jump-op/jabs 1tag))

       ((space number)    (vax/space number))
       ;; loser
       ((jump . args)     (apply emit-jump args))       ; -- state machine, ibs
       ;; loser
       ((template . args) (apply %emit-template args))  ; -- state machine, ib
       ((byte number) (vax/byte number))
       ((word number) (vax/word number))
       ((long number) (vax/long number))
       ((block . forms) (eval `(block ,@forms) (machine-lap-env *vax-machine*)))
       ((equate id form) (*define-lap-vax id (eval form (machine-lap-env *vax-machine*))))
       ))

(define (vax-lap-jbcc jump-op 1tag)
  (let ((next-tag (generate-symbol 'vax-lap-jbcc)))
    (emit-jump-to-ib *current-ib* jump-op 1tag next-tag)
    (emit-tag next-tag)))

(*define-lap-vax 'number make-as-number)

;;; Losing near-parameterizations.

(define (losing-vax-emit opcode-fg . operands)
  (emit-to-ib *current-ib* (apply opcode-fg operands))
  (flush-delayed-comments))

(define (vaxemit fg)
  (emit-to-ib *current-ib* fg)
  (flush-delayed-comments))

(define %emit losing-vax-emit)

(set *pretty-print-tag* pp-ib-as-name-or-hash)
(set *current-machine* *vax-machine*) 

;;; Set machine parameters 
 
;;; These are in VAXI
;(set (machine-template-emitter *vax-machine*) emit-vax-template)
;(set (machine-cond-branch      *vax-machine*) vax/jcc)
;(set (machine-uncond-branch    *vax-machine*) vax/jbr)         

(set (machine-clump-size       *vax-machine*) 8)
(set (machine-maximum-clumps   *vax-machine*) 4)
(set (machine-clump-writer     *vax-machine*) vax/write-clumps)
(set (machine-pseudo-ops       *vax-machine*) *vax-pseudo-ops*)
(set (machine-pseudo-operands  *vax-machine*) *vax-pseudo-operands*)

;;; Handy items for lap env

(walk *define-lap-vax
      '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15)
      '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 
                                     
(walk *define-lap-vax
      '(S0 S1 S2 S3 NARGS P A1 A2 A3 A4 AN AN-1 TP nil-reg AP TASK FP SP)
      '(0  1   2 3  3     4 5  6  7  8  9  8    10 11      12 12   13 14))
