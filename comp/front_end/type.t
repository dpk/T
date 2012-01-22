(herald (front_end type))

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

;;; An extremely minimal type system
;;;
;;; A type is one of NIL, 'OBJECT, 'LITERAL '(PROC <n-ary?> <nargs>)

;;;   Get the type of node for a definition.

(define (get-node-definition-type node)
  (cond ((primop-node? node)
         (fix-primop-type (primop.type (primop-value node) node)))
        ((reference-node? node)
         nil)
        ((literal-node? node)
         'literal)
        ((lambda-node? node)
         `(proc ,(if (lambda-rest-var node) t nil)
                ,(length (lambda-variables node))))
        ((object-node? node)
         'object)
        (else nil)))

;;; Converts primop types into those used here.

(define (fix-primop-type old-type)
  (cond ; ((object-type? old-type)
        ;  'object)
        ((proc-type? old-type)
         `(proc ,(proc-type-n-ary? old-type) ,(proc-type-nargs old-type)))
        (else
         '(proc #t 0))))

;;;   Check that a primop's type matches a call to it.  This only checks
;;; the number of arguments.

(define (primop-type-check primop call)
  (let ((type (primop.type primop call))
        (nargs (length (call-args call))))
    (or (eq? type type/top)
        (and (proc-type? type)
             (or (fx= nargs (proc-type-nargs type))
                 (and (proc-type-n-ary? type)
                      (fx>= nargs (proc-type-nargs type))))))))

(define (callable-type? type)
  (or (eq? type 'object)
      (and (pair? type)
           (eq? (car type) 'proc))))




