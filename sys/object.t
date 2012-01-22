(herald object (env tsys))

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

;;;; object related macros
                                 
(define-syntax (fix-up-default pat body)
  (cond ((atom? body) 'nil)
        (else
          (destructure (((name . args) pat))
            (let ((ignore-list (iterate loop ((args args) (l '()))
                                 (cond ((null? args) l)
                                       ((atom? args) (cons args l))
                                       (else
                                        (loop (cdr args) (cons (car args) l)))))))
              `(named-lambda ,name ,args
                  (ignorable ,@ignore-list)
                  ,@body))))))


(define-syntax (operation default . clauses)
  `(%operation (%massage-default ,default)
               nil
               (join-methods handle-operation
               ,@clauses)))

(define-syntax (define-operation pat . body)
  (let ((pat (if (pair? pat) pat (cons pat '()))))
    `(define ,(car pat)
       (%operation  
         (fix-up-default ,pat ,body)
         ',(car pat)
         handle-operation))))

(define-syntax (define-settable-operation pat . body)
  (let ((pat (if (pair? pat) pat (cons pat '()))))
    `(define ,(car pat)
       (%settable-operation (fix-up-default ,pat ,body)
          	            ',(car pat)))))

(define-syntax (define-predicate id)
  `(define ,id (%predicate ',id)))  ; Hair this up later

(define-syntax (join-methods handler . clauses)
  (if (null? clauses)
      handler
     `(join (object nil ,@clauses) ,handler)))

(define-syntax (define-methods handler . clauses)
  `(set ,handler (join (object nil ,@clauses) ,handler)))

(define (expand-object-form form)
  (destructure (((proc . clauses) form))
    (let ((op (generate-symbol 'op)))
      `(*object ,proc (lambda (,op)
                        (select ,op
                          ,@(map construct-method clauses)
                          (else nil)))))))

(define (construct-method clause)
  (cond ((pair? (car clause))     
         (destructure ((((op state . vars) . body) clause))
           (if (atom? state)        ; old form
               `((,op) (lambda (,state #f ,@vars)
                           (ignorable ,state)
                           ,@body))
               (destructure (((self obj) state))
                 `((,op) (lambda (,self ,obj ,@vars)
                             ,@body))))))
        (else
         `((,(car clause)) ,@(cdr clause)))))
           
