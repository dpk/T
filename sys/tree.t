(herald tree (env tsys))

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

;;;; Tree stuff.

;++ fix for generic numbers
;++ is the microhack still needed?

(define (subst pred new old exp)
  (iterate loop ((new new) (old old) (exp exp))
    (cond ((eq? old exp) new)             ;++ Pessimal microhack
          ((pair? exp)
           (cons (loop new old (car exp))
                 (loop new old (cdr exp))))
          ((pred old exp) new)
          (else exp))))

(define-recursive (substq new old exp)
  (cond ((eq? old exp) new)             ;++ Pessimal microhack
        ((pair? exp)
         (cons (substq new old (car exp))
               (substq new old (cdr exp))))
        ((eq? old exp) new)
        (else exp)))

(define-recursive (substv new old exp)
  (cond ((eq? old exp) new)             ;++ Pessimal microhack
        ((pair? exp)
         (cons (substv new old (car exp))
               (substv new old (cdr exp))))
        ((equiv? old exp) new)
        (else exp)))

(define-integrable (copy-tree x) (substq nil nil x))


;;; TREE-HASH.  Maybe this should be an operation.

(define-recursive (tree-hash tree)
  (cond ((pair? tree)
         (fixnum-abs
          (fx+ (tree-hash (car tree))
               (fixnum-ashl (tree-hash (cdr tree)) 1))))
        ((symbol? tree)
         (symbol-hash tree))
        ((string? tree)
         (string-hash tree))
        ((null? tree) 31415926)
        ((char? tree)
         (char->ascii tree))
        ((fixnum? tree)
         (fixnum-abs tree))
        (else (tree-hash (error "unhashable leaf~%  (~s ~s)"
                                'tree-hash
                                tree)))))


(define (make-tree-table . maybe-id)
  (create-%table (if maybe-id (car maybe-id) nil)
                 0 t true tree-hash alikev?))

(define (make-tree-table-with-size start-size . maybe-id)
  (create-%table (if maybe-id (car maybe-id) nil)
                 start-size t true tree-hash alikev?))

(define (tree-table? x)
  (and (%table? x)
       (eq? (%table-hash x) tree-hash)))
