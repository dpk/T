(herald (assembler as_open t 0))

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


;;; FIELD SIZE UTILITIES
;;; The assembler itself uses these, but machine descriptions
;;; may also make use of them.

(define-integrable (fx-lessp x y z)
  (and (fixnum? y) (fx<= x y) (fx< y z)))

(define-integrable (8bit? n)
  (fx-lessp -128 n 128))

;;; n is an address in bits
(define-integrable (8bit-in-bits? n)
  (fx-lessp -1024 n 1024))

(define-integrable (16bit? n)
  (fx-lessp #x-8000 n #x8000))

;;; n is an address in bits
(define-integrable (16bit-in-bits? n)
  (fx-lessp #x-40000 n #x40000))

(define-integrable (8bit-u? n)
  (fx-lessp -1 n #x100))

(define-integrable (16bit-u? n)
  (fx-lessp -1 n #x10000))

;;; Used in listing, & in bits.

(define-integrable (fixnum-floor x y)
  (fx- x (fixnum-mod x y)))

(define-integrable (fixnum-ceiling x y)
  (fixnum-floor (fx+ x (fx- y 1)) y))

(define-integrable (fixnum-maximum x y)
  (if (fx> x y) x y))



