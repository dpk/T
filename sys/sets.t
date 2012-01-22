(herald sets)

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

;;; Utility operations on sets: ADJOIN, UNION, INTERSECTION, REMQ, SETDIFF

;;; The empty set.

(define (empty-set)
  '())

;;; Is member an element of set.                    

(define (member-of-set? set member)
  (memq? set member))

(define (set-member? x s)                       ;defunct
  (memq? x s))

;;; Add an element X to a set S.

(define (adjoin x s)                            ;defunct
  (if (memq? x s) s (cons x s)))

(define (add-to-set set member)
  (if (memq? member set) set (cons member set)))

;;; Union of two sets.

(define (union x y)
  (if (null? x)
      y
      (do ((y y (cdr y))
           (r x (if (memq? (car y) r)
                    r
                    (cons (car y) r))))
          ((null? y) r))))

;;; Intersection of two sets.

(define (intersection x y)
  (if (null? y)
      '()
      (iterate loop ((x x) (res '()))
        (cond ((null? x) res)
              ((memq? (car x) y)
               (loop (cdr x) (cons (car x) res)))
              (else
               (loop (cdr x) res))))))

;;; Is the intersection of X and Y nonempty.

(define (intersection? x y)
  (if (null? y)
      nil
      (iterate loop ((x x))
        (cond ((null? x)
               nil)
              ((memq? (car x) y)
               t)
              (else
               (loop (cdr x)))))))

;;; Remove an element X from a set S, non-destructively.  The result shares
;;; storage with S.

(define (setremq x s)
  (cond ((null? s) s)
        ((eq? (car s) x)
         (cdr s))
        (else
         (let ((y (setremq x (cdr s))))
           (if (eq? y (cdr s))
               s
               (cons (car s) y))))))

(define (remove-from-set set member)
  (cond ((null? set) set)
        ((eq? (car set) member)
         (cdr set))
        (else
         (let ((elt (remove-from-set (cdr set) member)))
           (if (eq? elt (cdr set))
               set
               (cons (car set) elt))))))



;;; Difference of two sets: (SETDIFF A B) = everything in A that is not in B.

(define (setdiff x y)
  (do ((x x (cdr x))
       (r '() (if (memq? (car x) y)
                  r
                  (cons (car x) r))))
      ((null? x) r)))

(define set-difference setdiff)

;;; Mapping down a set

(define (map-set f s)
  (map f s))

(define (walk-set f s)
  (walk f s))
