(herald aliases (env tsys (t3_primops open)))

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

;;; Standard name abbreviations

(define-constant char     string-head)
(define-constant chdr     string-tail)
(define-constant chdr!    string-tail!)
(define-constant nthchdr  string-nthtail)
(define-constant nthchdr! string-nthtail!)
(define-constant nthchar  string-elt)

(define-constant fx+   fixnum-add)
(define-constant fx-   fixnum-subtract)
(define-constant fx*   fixnum-multiply)
(define-constant fx/   fixnum-divide)
(define-constant fx1+  fixnum-add1)
(define-constant fx-1+ fixnum-subtract1)
(define-constant fx=   fixnum-equal?)
(define-constant fx<   fixnum-less?)
(define-constant fx>   fixnum-greater?)
(define-constant fxn=  fixnum-not-equal?)
(define-constant fx>=  fixnum-not-less?)
(define-constant fx<=  fixnum-not-greater?)

(define-constant fx-zero?  fixnum-zero?)
(define-constant fx-abs    fixnum-abs)
(define-constant fx-rem    fixnum-remainder)
(define-constant fx-negate fixnum-negate)
(define-constant fx-ior    fixnum-logior)
(define-constant fx-and    fixnum-logand)
(define-constant fx-xor    fixnum-logxor)
(define-constant fx-not    fixnum-lognot)
(define-constant fx-ash    fixnum-ash)
(define-constant fx-ashl   fixnum-ashl)
(define-constant fx-ashr   fixnum-ashr)

(define-constant vref    vector-elt)
(define-constant bref-8-u  mref-8-u)
(define-constant bref-8-s  mref-8-s)
(define-constant bref-16-u  mref-16-u)
(define-constant bref-16-s  mref-16-s)
(define-constant bref    bref-8-u)
(define-constant bref-8  bref-8-s)
(define-constant bref-16 bref-16-s)
(define-constant bref-32 mref-integer)
;++(define-constant bref-pointer mref-32)
(define-constant extend-elt extend-pointer-elt)

;;; Non value returns

(define-integrable (no-value) (return))

(define-constant proclaim enforce)

