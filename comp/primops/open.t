(herald open (env tsys))

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

;;; The following variables are free in this file: 
;;; ( FIXNUM-NEGATE CHOPY MEM EOF NO-MORE-COND-CLAUSES NTH )

;;;; Standard constant procedures

;;; Open-coded routines, defined as constant procedures.

;;; System constants

(define-constant t    '#t)
(define-constant else '#t)
(define-constant nil  '#f)

;++ move this stuff
(define-constant null-char #\null)
(define-constant number-of-char-codes 256)
(define-constant string-length-limit 8388607)   ;inclusive limit (2^23 - 1)

;;; Boolean stuff

(define-constant (not x) (if x nil t))
(define-constant (false? x) (if x nil t))

(define-constant (boolean? x) (or (eq? x t) (eq? x nil)))

;;; Combinator stuff

(define-constant (always v) (lambda x (ignore x) v))

(define-constant (proj0 x . rest) (ignore rest) x)
(define-constant (proj1 x y . rest) (ignore rest x) y)
(define-constant (proj2 x y z . rest) (ignore rest x y) z)
(define-constant (proj3 x y z w . rest) (ignore rest x y z) w)

(define-constant (projn n) (lambda arglist (nth arglist n)))

(define-constant (identity x) x)

;;; Macro support for compiler

(define-constant (cond-=>-aux p f a) (if p ((f) p) (a)))

(define-constant (or-aux p r) (if p p (r)))

;;; Used by the compiler for n-ary LETs

(define (%list . l) l)  ; Not DEFINE-CONSTANT (that's the whole point).

;;; Type checking

(define-constant (enforce type obj)
  (if (type obj) obj (*enforce type obj)))

(define-constant (check-arg type obj where)
  (ignore where)
  (if (type obj) obj (*enforce type obj)))

;;; Various predicates

(define-constant (null-list? x)
  (cond ((null? x) t)
        ((list? x) nil)
        (else (undefined-effect "NULL-LIST? got an atom"))))

(define-constant (eof? x)
  (eq? x eof))

(define-constant (newline? c) (eq? c #\newline))

(define-constant (mem? pred obj list)
  (if (mem pred obj list) t nil))

(define-constant (memq? obj list)
  (if (memq obj list) t nil))

(define-constant (neq? x y) (not (eq? x y)))

(define-constant (nonnegative-fixnum? x)
  (and (fixnum? x) (fixnum-not-negative? x)))

;;; Totally random stuff

(define-constant (env-lookup env identifier local? create?)
  (env identifier local? create?))

(define-constant (cons x y)
  (let ((p (%make-pair)))
    (set (car p) x)
    (set (cdr p) y)
    p))

;;; Making closed forms check their arguments

(define-local-syntax (define-safe form . stuff)
  (let ((name (if (atom? form) form (car form))))
    `(block
       (define-constant ,form . ,stuff)
       (declare type-safe-closed-form ,name))))

(define-safe (vset vec i val)
  (set (vector-elt vec i) val))

;;; String stuff

(define-safe (non-empty-string? x)
  (and (string? x) (not (string-empty? x))))
                              
(define-safe (string-empty? x)
  (fixnum-equal? (string-length x) 0))

(define-safe (string-tail s)
  (string-tail! (chopy s)))
                          
(define-safe (string-tail! string)
  (string-nthtail! string 1))

(define-safe (string-nthtail s n)
  (string-nthtail! (chopy s) n))
      
(define-safe (string-nthtail! s n)
  (%chdr s n)
  s)

;;; The LETs in STRING-ELT forces STRING-TEXT to be done before MREF-INTEGER.
;;; Then in the type-safe version the STRING? test is done on X before the
;;; MREF-INTEGER call.  The problem is that MREF-INTEGER has no prefered type.

(define-safe string-elt
  (object (lambda (x i)
            (let ((s (string-text x)))
              (text-elt s (fixnum-add (mref-integer x 4) i))))
    ((setter self)
     (lambda (x i v)
       (let ((s (string-text x)))
         (set (text-elt s (fixnum-add (mref-integer x 4) i))
              v))))))

(define-safe string-head 
  (object (lambda (x) (string-elt x 0))
    ((setter self)
     (lambda (x v)
       (set (string-elt x 0) v)))))
                                 
(define-constant string-offset
  (object (lambda (s)
            (let ((s (enforce string? s)))
              (mref-integer s 4)))
    ((setter self)
     (lambda (self i)
       (let ((s (enforce string? self))
             (i (enforce fixnum? i)))
         (set (mref-integer s 4) i))))
    ((identification self) 'string-offset)))

(define-safe (max-string-length s)
  (text-length (string-text s)))
                
;;; Fixnum stuff

(define-safe (fixnum-greater? x y)
  (fixnum-less? y x))

(define-safe (fixnum-not-equal? x y)
  (not (fixnum-equal? x y)))

(define-safe (fixnum-not-less? x y)
  (not (fixnum-less? x y)))

(define-safe (fixnum-not-greater? x y)
  (not (fixnum-less? y x)))

(define-safe (fixnum-abs n)
  (if (fixnum-less? n 0) (fixnum-negate n) n))

(define-safe (fixnum-min x y)
  (if (fixnum-less? x y) x y))

(define-safe (fixnum-max x y)
  (if (fixnum-greater? x y) x y))

(define-safe (fixnum-positive?     x) (fixnum-greater?     x 0))
(define-safe (fixnum-negative?     x) (fixnum-less?        x 0))
(define-safe (fixnum-zero?         x) (fixnum-equal?       x 0))
(define-safe (fixnum-not-positive? x) (fixnum-not-greater? x 0))
(define-safe (fixnum-not-negative? x) (fixnum-not-less?    x 0))
(define-safe (fixnum-not-zero?     x) (fixnum-not-equal?   x 0))
(define-safe (fixnum-add1          x) (fixnum-add          x 1))
(define-safe (fixnum-subtract1     x) (fixnum-subtract     x 1))
                            
(define-safe (fixnum-odd? x)
  (bit-test x 0))

(define-safe (fixnum-even? x)
  (not (fixnum-odd? x)))

;;; Character comparison stuff

(define-safe (char> x y)
  (char< y x))

(define-safe (charn= x y)
  (not (char= x y)))

(define-safe (char>= x y)
  (not (char< x y)))

(define-safe (char<= x y)
  (not (char< y x)))


;;; Weaks

(define-integrable (weak-semaphore-set? weak)
  (not (alt-bit-set? weak)))

(define-integrable (set-weak-semaphore weak)
  (cond ((weak-semaphore-set? weak)
         (error "simultaneous access on weak ~S" weak))
        (else
         (clear-alt-bit! weak))))

(define-integrable (clear-weak-semaphore weak)
  (set-alt-bit! weak)
  0)

(define-constant (test-and-set-semaphore weak)
  (defer-interrupts (cond ((weak-semaphore-set? weak)
                           t)
                          (else
                           (clear-alt-bit! weak)
                           nil))))
