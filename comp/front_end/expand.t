(herald (front_end expand))

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

;;;              Macro expansion of top level forms.
;;;============================================================================
;;;   EXPAND-FORMS expands all of the EXPS that are macros.  A list of
;;; (<expression> <syntax-table>) lists is returned where <expression> is not
;;; a macro and <syntax-table> contains the syntax that is to be used in
;;; compiling <expression>.
;;;
;;;   EXPAND-FORMS deals with macros and the special-forms DEFINE-LOCAL-SYNTAX,
;;; LET-SYNTAX, and BLOCK.

(define (expand-forms exps syntax)
  (reverse! (real-expand-forms exps syntax)))

;;; Loop down the list EXPS calling EXPAND on each expression in turn.  If the
;;; expression expands in to (BLOCK . <exps>) then the <exps> are expanded
;;; by a recursive call.

(define (real-expand-forms exps syntax)
  (iterate loop ((exps exps) (syntax syntax) (res '()))
    (cond ((null? exps)
           res)
          (else
           (receive (exp local-syntax new-syntax)
                    (expand (car exps) syntax)
             (cond ((and (pair? exp)
                         (eq? (car exp) syntax/block))
                    (loop (cdr exps)
                          new-syntax
                          (append! (real-expand-forms (cdr exp) local-syntax)
                                   res)))
                   (else
                    (loop (cdr exps)
                          new-syntax
                          `((,exp ,local-syntax ,(car exps)) . ,res)))))))))

;;; Expand EXP using syntax table SYNTAX.  Just checks to see if it is a macro.

(define (expand exp syntax)
  (cond ((atom? exp)
         (return exp syntax syntax))
        ((symbol? (car exp))
         (let ((probe (syntax-table-entry syntax (car exp))))
           (cond ((null? probe)
                  (return exp syntax syntax))
                 (else
                  (expand-special-form probe exp syntax)))))
        ((syntax-descriptor? (car exp))
         (expand-special-form (car exp) exp syntax))
        (else
         (return exp syntax syntax))))

;;; Does syntax error checking, primitive syntax dispatch, and macro 
;;; expansion.  The error checking is done by CHECK-SPECIAL-FORM, a T system
;;; procedure.

(define (expand-special-form descr exp syntax)
  (let ((proc (table-entry primitive-handler-table descr))
        (new-exp (check-special-form-syntax descr exp)))
    (cond ((neq? exp new-exp)
           ;; An error was reported, and luser gave us a new form.
           (expand new-exp syntax))
          ((eq? descr syntax/let-syntax)
           (expand-let-syntax exp syntax))
          ((eq? descr syntax/define-local-syntax)
           (expand-local-syntax exp syntax))
          (proc
           (return (cons descr (cdr exp)) syntax syntax))
          ((macro-expander? descr)
           (expand (expand-macro-form descr exp syntax) syntax))
          (else
           (syntax-error '"special form unknown to this compiler~%  ~S" exp)))))

;;; DEFINE-LOCAL-SYNTAX
;;;  Returns an updated syntax table to be used in expanding the expressions
;;; that follow.

(define (expand-local-syntax exp syntax)
  (let ((new-syntax (make-syntax-table syntax nil)))
    (set-local-syntax new-syntax (cdr exp))
    (return `(,syntax/block) new-syntax new-syntax)))

;;; LET-SYNTAX
;;;  Returns an updated syntax table to be used in expanding the expressions
;;; in the body and the old syntax table for expanding the ones that follow.

(define (expand-let-syntax exp syntax)
  (destructure (((#f specs . body) exp))
    (let ((new-syntax (make-syntax-table syntax nil)))
      (walk (lambda (spec)
              (set-local-syntax new-syntax spec))
            specs)
      (return `(,syntax/block . ,body) new-syntax syntax))))


