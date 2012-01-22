(herald dump_codes
        (env t))

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

;;;
;;; codes used for encoding and decoding
;;;

;;; Unstructured codes
(define-constant dump/null               0)       ;;; the empty list
(define-constant dump/char               1)       ;;; character
(define-constant dump/true               2)       ;;; '#t
(define-constant dump/begin-object       3)       ;;; put before each object
(define-constant dump/end-of-file        4)       ;;; put at end of file

;;; Codes that are shared or unshared
;;;   Low order bit zero = 0, not shared
;;;                      = 1, shared
(define-constant dump/pair              16)       ;;; cons pair
(define-constant dump/coded             18)       ;;; Coded by external proc.

;;; Codes that have size bytes
;;;   Number of bytes = (+ 1 (remainder code 4))
(define-constant dump/object-ref        32)       ;;; an already stored object
(define-constant dump/positive-fixnum   36)       ;;; positive fixnum
(define-constant dump/negative-fixnum   40)       ;;; negative fixnum

;;; Codes that are shared or unshared and have size bytes
;;;   Low order bit zero = 0, not shared
;;;                      = 1, shared
;;;   Number of bytes = (+ 1 (quotient (remainder code 8) 2))
(define-constant dump/string            64)       ;;; old string method
(define-constant dump/symbol            72)       ;;; symbol
(define-constant dump/vector            80)       ;;; vector
(define-constant dump/byte-vector       88)       ;;; byte vector
(define-constant dump/positive-bignum   96)       ;;; positive bignum
(define-constant dump/negative-bignum  104)       ;;; negative bignum
(define-constant dump/double-flonum    112)       ;;; two word flonum
