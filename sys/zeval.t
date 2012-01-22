(herald zeval
        (env tsys (osys kernel) (osys list)))

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

;;; Evaluator

;++ Should the Z system handle syntax?

(define (z-eval exp env)
  (cond ((atom? exp)
         (cond ((symbol? exp) (z-evalue exp env))
               (else exp)))
        (else
         (let ((head (car exp)))
           (cond ((pair? head)
                  (z-eval-call (z-eval head env) (cdr exp) env))
                 ((symbol? head)
                  (z-eval-symbol-form head exp env))
                 ((and (syntax-present?) (syntax-descriptor? head))
                  (z-eval-special-form head exp env))
                 (else
                  (z-eval-call (z-eval head env) (cdr exp) env)))))))

(define (z-eval-symbol-form head exp env)
  (case head
        ((quote) (cadr exp))
        ((if)
         (if (z-eval (cadr exp) env)
             (z-eval (caddr exp) env)
             (z-eval (cadddr exp) env)))
        ((block)
         (z-eval-rest (cdr exp) env))
        ((lambda)
         (z-eval-lambda (cdr exp) env))
        ((named-lambda)
         (z-eval-lambda (cddr exp) env))
        ((set-variable-value)
         (cond ((atom? (cadr exp))
                (z-set-evalue (cadr exp) env (z-eval (caddr exp) env) nil nil))
               (else
                (z-eval `((setter ,(caadr exp)) ,@(cdadr exp),@(cddr exp)) env))))
        ((define-variable-value)
         (z-set-evalue (cadr exp) env (z-eval (caddr exp) env) t t))
        (else                                       
         (cond ((and (syntax-present?)
                     (syntax-table-entry standard-syntax-table head))
                => (lambda (descr)
                     (z-eval-special-form descr exp env)))
               (else
                (z-eval-call (z-evalue head env) (cdr exp) env))))))

(define (z-eval-special-form descr exp env)
  (let ((new-exp (check-special-form-syntax descr exp)))
    (cond ((neq? exp new-exp)
           ;; An error was reported, and luser gave us a new form.
           (z-eval new-exp env))
          (else
           ;; Non-primitive syntax; assume it's a macro.
           (z-eval (expand-macro-form descr exp standard-syntax-table) env)))))

(define (z-eval-call proc args env)
  (cond ((null-list? args)
         (proc))
        (else
         (let ((arglist (cons (z-eval (car args) env) '())))
           (do ((z arglist (cdr z))
                (args (cdr args) (cdr args)))
               ((null-list? args)
                (apply proc arglist))
             (set (cdr z) (cons (z-eval (car args) env) '())))))))


(define (z-eval-lambda params+body env)
  (let ((params (car params+body))
        (body (cdr params+body)))
    (cond ((null? params)               ; trivial pessimization
               (lambda ()
                 (z-eval-rest body env)))
              (else
               (lambda args
                 (z-eval-rest body
                         (cons env (cons params args))))))))

(define (z-eval-rest exps env)
  (cond ((atom? (cdr exps)) (z-eval (car exps) env))
        (else (z-eval (car exps) env)
                  (z-eval-rest (cdr exps) env))))

(define (z-evalue id env)
  (cond ((not (pair? env))
         (let ((probe (env id nil nil)))
           (cond (probe (vcell-contents probe))
                 (else (error "~s is unbound" id)))))
        (else (iterate loop ((ids (cadr env)) (vals (cddr env)))
                        (cond ((atom? ids)
                               (cond ((eq? id ids) vals)
                                         (else (z-evalue id (car env)))))
                              ((eq? id (car ids))
                               (car vals))
                              (else     
                               (loop (cdr ids) (cdr vals))))))))

(define (z-set-evalue id env val local? def?)
;++ why not LOCALE? or ENVIRONMENT?
  (cond ((not (pair? env))
         (*set-value env id val)
         (no-value))
        (else
         (iterate loop ((ids (cadr env)) (vals (cddr env)))
           (cond ((atom? ids)
                  (cond ((eq? id ids)
                         (error "~s: can't set value of a rest-arg!" id))
                        (else (z-set-evalue id (car env) val local? def?))))
                 ((eq? id (car ids))
                  (set (car vals) val)
                  val)
                 (else
                  (loop (cdr ids) (cdr vals))))))))
