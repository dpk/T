(herald predicates
  (env (make-empty-early-binding-locale nil) primops arith locations low))

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

;;; Really basic type predicates, not primitive to the compiler.

(define-constant (null? x)
  (eq? x '()))

(define-constant (atom? x)
  (if (pair? x) '#f '#t))

(define-constant (pair? x)
  (and (list? x)
       (if (null? x) '#f '#t)))

(define-local-syntax (define-extend-predicate type . header-type)
  (let ((header-type (if (atom? header-type) type (car header-type))))
    `(define-constant (,(concatenate-symbol type '?) x)
       (and (extend? x)
            (,(concatenate-symbol header-type '-header?) (extend-header x))))))

(define-extend-predicate symbol)
(define-extend-predicate vector general-vector)
(define-extend-predicate bytev)
(define-extend-predicate text)
(define-extend-predicate string)
(define-extend-predicate foreign)
(define-extend-predicate template)
(define-extend-predicate vcell)
(define-extend-predicate unit)
(define-extend-predicate bignum)
(define-extend-predicate weak-set)
(define-extend-predicate weak-alist)
(define-extend-predicate weak-table)
(define-extend-predicate weak-cell)
(define-extend-predicate vframe)
(define-extend-predicate double-float)

