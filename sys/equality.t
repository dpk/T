(herald equality (env tsys))

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

;;; Exact
;++ Where should this be defined?

(define (exact? n)
  (cond ((fixnum?   n) '#t)   ; avoid the procedure call to integer
        ((bignum?   n) '#t)
        ((rational? n) '#t)
        (else '#f)))

;;; Various equality predicates.

;++ fix for generic numbers
(define (equiv? a b)
    (or (eq? a b)
	(cond ((float? a) (and (float? b) (fl= a b)))
	      ((bignum? a) (and (bignum? b) (bignum-equal? a b)))
	      ((hacked-ratio? a) (and (hacked-ratio? b) (rational-equal? a b)))
	      ((string? a) (and (string? b) (string-equal? a b)))
	      (else '#f))))

(define hacked-ratio?
  (let ((tem (extend-header (ratio 1 2))))
    (lambda (x)
      (and (extend? x) (eq? (extend-header x) tem)))))
  

(define (eqv? a b)
  (or (eq? a b)
      (and (char? a) (char? b) (char= a b))  ;++ make sure of this!
      (and (fixnum? a) (fixnum? b) (fx= a b))
      (and (exact? a) (exact? b) (= a b))))

(define-recursive (equal? a b)
  (or (eq? a b)
      (and (number? a)  (number? b)  (= a b))
      (and (string? a) (string? b) (%string-equal? a b))
      (and (pair? a)   (pair? b)   (alikeq? a b))
      (and (vector? a)
           (vector? b)
           (fx= (vector-length a) (vector-length b))
           (let ((len (vector-length a)))
             (iterate loop ((i 0))
               (cond ((fx>= i len) '#t)
                     ((equal? (vref a i) (vref b i))
                      (loop (fx+ i 1)))
                     (else '#f)))))
      ;++ add arrays later
      ))

(define-integrable (not-equal? a b) (not (equal? a b)))

(define (alike? pred exp1 exp2)
  (iterate loop ((exp1 exp1) (exp2 exp2))
    (cond ((eq? exp1 exp2) t)             ; speed hack
          ((atom? exp1)
           (if (atom? exp2) (pred exp1 exp2) nil))
          ((atom? exp2) nil)
          ((loop (car exp1) (car exp2))
           (loop (cdr exp1) (cdr exp2)))
          (else nil))))

(define-recursive (alikeq? exp1 exp2)  ; i like q, 2
  (cond ((eq? exp1 exp2) t)             ; speed hack
        ((atom? exp1)
         (if (atom? exp2) (eq? exp1 exp2) nil))
        ((atom? exp2) nil)
        ((alikeq? (car exp1) (car exp2))
         (alikeq? (cdr exp1) (cdr exp2)))
        (else nil)))

(define-recursive (alikev? exp1 exp2)
  (cond ((eq? exp1 exp2) t)             ; speed hack
        ((atom? exp1)
         (if (atom? exp2) (equiv? exp1 exp2) nil))
        ((atom? exp2) nil)
        ((alikev? (car exp1) (car exp2))
         (alikev? (cdr exp1) (cdr exp2)))
        (else nil)))
