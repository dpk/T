(herald (assembler as_utils t 3)
        (env t (assembler ib)))  ;  get-value needs

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

;;; Runtime support for the assembler (and maybe descriptions).

(define (walk-backwards proc list)
  (cond ((null? list) 'done)
        (else (walk-backwards proc (cdr list))
              (proc (car list)))))

(define (fixnum-mod x y)
  (cond ((fx< x 0)
         (fx- (fx- y 1) (fixnum-remainder (fx- -1 x) y)))
        (else (fixnum-remainder x y))))

;(define bref (*value *t-implementation-env* 'bref))
;(define make-bytev (*value *t-implementation-env* 'make-bytev))

;;; GET-VALUE is called from FG and from BITS.
;;;    DESTRUCTURE-FG, CONTEXT-FG, BITS-FG
;;; GET-FIXED-VALUE is called only from FG
;;;    COMPRESS-FG

(define (get-fixed-value vop voc1 vars vals)
  (xselect vop
    ((vop/const) (vref vals voc1))
    ((vop/var)   (let ((expr (vref vars voc1)))
                   (cond ((fixnum? expr) expr)
                         (else 
                          (no-op (error "assembler expecting fixed value, got ~s"
                                        expr))))))
    ((vop/proc)  ((vref vals voc1) vars))))

(define (get-value vop voc1 vars vals)
  (xselect vop
    ((vop/const) (vref vals voc1))
    ((vop/var)   (let ((expr (vref vars voc1)))
                   (cond ((fixnum? expr) expr)  
                         ((procedure? expr) (expr vars))
                         (else expr))))
    ((vop/proc)  ((vref vals voc1) vars))
    ))
    
;;; Called from expressions in machine decriptions that use DISP or FROM.

(define-integrable (expr-compute-disp vars mark-index dest-expr)
  (let ((mark-address (vref *mark-addresses* mark-index)))
     (cond ((ib? dest-expr) 
            (fx- (ib-address dest-expr) mark-address))
           (else                           
            (no-op 
              (error "bad arguments to EXPR-COMPUTE-DISP - DISP and FROM expect a mark and a tag"))))))

;;; This is provided to be used from machine decriptions expressions.

(define (mark-address mark-index)
  (vref *mark-addresses* mark-index))

;;; Enumerated types

;;; Branches - this is hacked as numbers to make branch reversal fast.
;;; This is probably not necessary.
;;; Used by assembler, as clients, as client compiler.

;;; carry set   is the same as uj<
;;; carry clear is the same as uj>=

(define-constant jump-op/jabs 0)
(define-constant jump-op/jn=  1) (define-constant jump-op/j=   -1)
(define-constant jump-op/j>   2) (define-constant jump-op/j<=  -2)
(define-constant jump-op/j>=  3) (define-constant jump-op/j<   -3)
(define-constant jump-op/uj>  4) (define-constant jump-op/uj<= -4) 
(define-constant jump-op/uj>= 5) (define-constant jump-op/uj<  -5)
(define-constant jump-op/not_negative 6) (define-constant jump-op/negative -6)
(define-constant jump-op/no_overflow  7) (define-constant jump-op/overflow -7) 
                                                                   

;;; For listings & other output.

(define (jump-op-name op)
    (xcond ((and (fx>= op 0) (fx<= op 7))
            (vref '#("abs" "neq" "gt " "ge " "gtu" "geu" "pos" "vc") op))
           ((and (fx>= op -7) (fx< op 0))
            (vref '#("abs" "eq " "le " "lt " "leu" "ltu" "neg" "vs") (fx- 0 op)))))

(define reverse-jump fixnum-negate)

;;; This is stuff the assembler uses all over

;;; AS internal enumerations

(define-constant vop/const 0)
(define-constant vop/var   1)
(define-constant vop/proc  2)

;;; Compute 1 time
(define-constant wop/fix          0)
(define-constant wop/@fix         1)
(define-constant wop/proc         2)

(define-constant wop/subfield-ic  3)

;;; Recompute
(define-constant wop/var          4)
(define-constant wop/depending-on 5)
(define-constant wop/d-o          wop/depending-on)

(define-constant wop/mark         6)

;;; Field size stuff (more in AS_OPEN)

(define (lessp x y z)
  (and (<= x y) (< y z)))

(define (32bit? n)
  (lessp #x-80000000 n #x80000000))

(define (32bit-u? n)
  (lessp -1 n #x100000000))

