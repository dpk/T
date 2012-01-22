(herald (assembler fg_expr t 11))

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

;;; This "compiles" "expressions" taken from an FG description.

(lset *fg-expr-refs* '())

(define *fg-expr-syntax-table*
  (make-syntax-table standard-syntax-table '*fg-expr-syntax-table*))
                 
;;; ? references variables in the FG
(set (syntax-table-entry *fg-expr-syntax-table* '?)
     (macro-expander (? var)
       (if (not (memq var *fg-expr-refs*)) (push *fg-expr-refs* var))
       var))

(lset *env-parameter-name* nil)

(set (syntax-table-entry *fg-expr-syntax-table* 'from)
     (macro-expander (from mark-var dest-var)
       (if (not (memq mark-var *fg-expr-refs*)) (push *fg-expr-refs* mark-var))
       (if (not (memq dest-var *fg-expr-refs*)) (push *fg-expr-refs* dest-var))
       `(expr-compute-disp ,*env-parameter-name* 
                           ,mark-var
                           ,dest-var)
       ))

;;; Returns s-expr for a procedure, which expects its first argument
;;; to be the VARS vector.

(define (compile-expr expr runenv-shape)
  (let ((env-parameter-name (generate-symbol 'expr-env))
        )
    (bind ((*fg-expr-refs* '())
           (*env-parameter-name* env-parameter-name)
           )
      (let ((code (tas-expand expr *fg-expr-syntax-table*)))
        `(lambda (,env-parameter-name)
           (let ,(map (lambda (v)
                        `(,v (vref ,env-parameter-name
                                   ,(vars-ref runenv-shape v))
                              ))
                      *fg-expr-refs*)
             ,code))))))
                                           
                 
;;; Returns s-expr for a printer for an FG.

(define (compile-print-expr expr parameters runenv-shape)
  (bind ((*fg-expr-refs* '()))
    (let ((env-parameter-name (generate-symbol 'expr-env)))
      (let ((code (tas-expand expr *fg-expr-syntax-table*)))
        `(lambda ,(cons env-parameter-name parameters)
           (let ,(map (lambda (v)
                        `(,v (vref ,env-parameter-name
                                   ,(vars-ref runenv-shape v))
                             ))
                      *fg-expr-refs*)
             ,code))))))
