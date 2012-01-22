(herald syntax (env tsys))

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

;;;; Syntax tables & syntax descriptors

(define-integrable syntax-table-entry
  (object (lambda (table sym) (table sym))
          ((setter self) set-syntax-table-entry)))

(define-operation (set-syntax-table-entry table sym descr))

;++ shouldn't syntax tables be eliminated and let
;++ the syntax-descriptor into regular environments?

(define (make-syntax-table super . maybe-id)
  (let* ((id (car maybe-id))            ; (car '()) => #f
         (table (make-table id))
         (env nil)
         (atomex nil))
    (object (lambda (sym)
              (let ((probe (table-entry table sym)))
                (cond (probe (if (eq? probe '*filtered*) nil probe))
                      (super (super sym))
                      (else nil))))
      ((set-syntax-table-entry self sym descr)
       (cond ((table-entry table sym)
              (env-warning "Redefining syntax" sym))
             ((and super (super sym))
              (env-warning "Shadowing syntax" sym)))
       (set (table-entry table sym)
            (if descr
                (enforce syntax-descriptor? descr)
                '*filtered*)))
      ((env-for-syntax-definition self)
       (cond (env)
             (else
              (set env (if super 
                           (env-for-syntax-definition super)
                           (make-locale standard-env 'env-for-syntax-definition)))
              env)))
      ((set-env-for-syntax-definition self new-env)
       (set env (enforce environment? new-env)))
      ((atom-expander self)
       (cond (atomex)
             (super (atom-expander super))
             (else default-atom-expander)))
      ((set-atom-expander self new-atomex)
       (set atomex (enforce procedure? new-atomex)))
      ((syntax-table? self) '#t)
      ((identification self) id)
      ((set-identification self val)
       (if (not id) (set id val)))
      ((print-type-string self) "Syntax-table"))))
                                
(define-predicate syntax-table?)
(define-settable-operation (env-for-syntax-definition table))
(define set-env-for-syntax-definition (setter env-for-syntax-definition))

(define-settable-operation (atom-expander table))
(define set-atom-expander (setter atom-expander))

(define (default-atom-expander atom)
  (cond ((symbol? atom)
         `(,(t-syntax 'variable-value) ,atom))   ;randomness
        ((self-evaluating? atom)
         `(,(t-syntax 'quote) ,atom))           ;more randomness
        ((null? atom)
         (warning "~S in evaluated position~%" atom)
         `(,(t-syntax 'quote) ,atom))
        (else
         (syntax-error "unevaluable datum - ~S" atom))))

;++ what about #!true #!false ...
(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (char? exp)))

(define (make-empty-syntax-table id)
  (make-syntax-table nil id))

(define-operation (syntax-descriptor? obj) (macro-expander? obj))
(define-predicate macro-expander?)
(define-operation (expand-macro-form desc exp table))
(define-operation (syntax-check-predicate obj) true)

;;; Called from expansion of MACRO-EXPANDER.
;;; EXPANDER is a procedure of one argument.
;;; Someday allow for tracing macro expansions?

(define (make-macro-descriptor expander pred id)
  (let ((pred (if (pair? pred) true pred)))
    (object nil
      ((expand-macro-form self exp table)
       (ignore table)
       (expander exp))
      ((macro-expander? self) t)
      ((syntax-check-predicate self) pred)
      ((print self stream)
       (print-syntax-descriptor self stream))
      ((identification self) id)
      ((disclose self)
       (disclose-macro-expander expander))
      ((get-loaded-file self)
       (get-loaded-file expander)))))

;;; Sample nonstandard macro expander:
;;;   (OBJECT NIL
;;;           ((EXPAND-MACRO-FORM SELF EXP TABLE)
;;;            ... use TABLE ...)
;;;           ((MACRO-EXPANDER? SELF) T))

;;; Basic syntax check for special forms.

(define (check-special-form-syntax desc exp)
  (cond (((syntax-check-predicate desc) (cdr exp))
         exp)
        (else
         (check-special-form-syntax desc
                                    (syntax-error
                                     "bad syntax for special form~%  ~S"
                                     exp)))))

;;; This is no longer used in this file, but it is needed elsewhere.

(define (compatible-with-argspectrum? l spect)
  (iterate loop ((l l) (n 0))
    (cond ((fx>= n (car spect))
           (cond ((null? (cdr spect))
                  (if (null? l) n nil))         ;Must match exactly
                 ((fixnum? (cdr spect))
                  (iterate loop ((l l) (n n))
                    (cond ((or (fx>= n (cdr spect)) (atom? l))
                           (if (null? l) n nil))
                          (else (loop (cdr l) (fx+ n 1))))))
                 (else n)))     ;Enough args, and no limit
          ((atom? l) nil)       ;Too few args
          (else (loop (cdr l) (fx+ n 1))))))

;;; ---------- end of important stuff.

;;; Random debugging thing called by CRAWL.

(define (macro-expand form table)
  (cond ((atom? form) form)
        ((symbol? (car form))
         (let ((probe (syntax-table-entry table (car form))))
           (if (and probe (macro-expander? probe))
               (expand-macro-form probe form table)
               form)))
        ((macro-expander? (car form))
         (expand-macro-form (car form) form table))
        (else form)))

;;; Incredibly kludgey procedure.  Assumes that the form of DISCLOSE
;;; of the expansion procedure is of the form
;;;  (NAMED-LAMBDA ,SYMBOL (,Z)
;;;    (DESTRUCTURE (((() . ,ARGS) ,Z))
;;;      . ,REST))

(define (disclose-macro-expander proc)
  (let ((f (lambda (name body)
             (cond ((and (pair? body) (eq? (car body) (t-syntax 'destructure)))
                    (destructure ((   (#f (((#f . args) #f)) . body)   body))
                      `(macro-expander (,name . ,args) . ,body)))
                   (else nil)))))
    (cond ((disclose proc)
            => (lambda (lexp)
                 (cond ((pair? lexp)
                        (case (car lexp)
                          ((named-lambda) (f (cadr lexp) (cadddr lexp)))
                          ((lambda)       (f nil         (caddr lexp)))
                          (else nil)))
                       (else nil))))
          (else nil))))

(define (*define-syntax env symbol descr)
  (set (syntax-table-entry (env-syntax-table env) symbol) descr))

(define (t-syntax name)         ;Handy abbreviation
  (syntax-table-entry standard-syntax-table name))

(set (syntax-present?) '#t)




