(herald vax_bignum (env tsys))

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

(define (set-bignum-length! bignum length)
  (lap ()  
    (ashl ($ -6) (d@r A1 -2) S0)   ; length in bytes
    (bicb2 ($ #b00000011) S0)
    (subl2 A2 S0)                 ; size of bogus bytev including header
    (subl2 ($ 4) S0)              ; bytev length
    (ashl ($ 8) S0 S0)
    (movb ($ header/bytev) S0)  ; bogus bytev header
    (ashl ($ -2) A2 S1)           ; new length
    (movl S0 (index (d@r A1 2) S1))
    (ashl ($ 8) S1 S0)
    (movb (d@r A1 -2) S0)
    (movl S0 (d@r A1 -2))
    (mnegl ($ 2) NARGS)
    (movl (@r sp) tp)
    (jmp (@r tp))))

(define-constant (bignum-positive? bignum)   ; if bit 7 of header is on
  (fx= (mref-8-u bignum -4) 
       (fixnum-add header/bignum 128)))

(define-constant bignum-negate!
  (primop bignum-negate! ()
    ((primop.side-effects? self) t)
    ((primop.generate self node)                               
     (let ((reg (->register 'pointer node (leaf-value ((call-arg 2) node)) '*)))
       (emit vax/xorb2 (machine-num #b10000000) (reg-offset reg -2))))
    ((primop.type self node)
     '#[type (proc #f (proc #f top) bignum)])))

(define (%digit-divide x1 x0 y)   ; Divide x1x0 by y with x1 < (* 2 y)
  (lap ()
    (rotl ($ -2) a1 s1)          ; == Logical shift right by 2
    (movl a2 s0)                 ; Dividend in S1,S0
    (ashq ($ -2) s0 s0)          ; Remove dividend tag
    (rotl ($ -2) a3 s2)          ; Divisor in S2 (without tag)

    (ediv s2 s0 a1 a2)           ; Boom

    (ashl ($ 2) a1 a1)           ; Fixnumize quotient
    (ashl ($ 2) a2 a2)           ;   and remainder
    (mnegl ($ 3) nargs)          ; Two values returned
    (movl (@r sp) tp)
    (jmp (@r tp))))








