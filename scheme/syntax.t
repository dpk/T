(herald syntax (env tsys))

;;; Copyright (c) 1985, 1987 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, K Pitman, J Rees.
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
;;; 4. Yale has made no warranty or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;

;;; Modified by Ashwin Ram, July 1985

;;; Further modifications for R^RS conformity by J Rees, December 1987

(define scheme-syntax-table
   (env-syntax-table scheme-env))

(define (t-syntax sym)
   (syntax-table-entry standard-syntax-table sym))

(define (scheme-syntax sym)
   (syntax-table-entry scheme-syntax-table sym))

(define (scheme-atom-expander atom)
  (cond ((symbol? atom)
         `(,(t-syntax 'variable-value) ,atom))   
        ((or (number?  atom)         ; self evaluating
             (string?  atom)
             (char?    atom)
             (boolean? atom))
         `(,(t-syntax 'quote) ,atom))           
        ((null? atom)
         (warning "~S in evaluated position~%" atom)
         `(,(t-syntax 'quote) ,atom))
        (else
         (syntax-error "unevaluable datum - ~S" atom))))

(set ((*value t-implementation-env 'atom-expander) scheme-syntax-table) 
     scheme-atom-expander) 

(define (definition? exp)
   (and (pair? exp) (eq? (car exp) 'define)))

(define (parse-define pat body)
   (cond ((atom? pat)
          (return pat (car body)))
         (else
          (return (car pat)
                  `(,(t-syntax 'named-lambda)
		    ,(car pat)
		    ,(cdr pat)
		    ,@(process-body body))))))

(define (parse-top-level-define pat body k)
  (receive (var val) (parse-define pat body) (k var val)))

; Peel definitions off front of body, and assemble an appropriate LABELS
; expression if any are found.

(define (process-body exp-list)
  (let ((assemble (lambda (e d) 
		    (if (null? d)
			e
			`((,(t-syntax 'labels) ,(reverse d) ,@e))))))
    (iterate -loop- ((e exp-list)
		     (d '()))
      (cond ((null? e)
	     ;; Not a particularly useful diagnostic, but...
	     (syntax-error "null body - ~S" exp-list)
	     (assemble '(0) d))
	    (else
	     (let ((exp (car e)))
	       (cond ((definition? exp)
		      (receive (name val) (parse-define (cadr exp) (cddr exp))
			(-loop- (cdr e)
				(cons `(,name ,val) d))))
		     (else (assemble e d)))))))))



(define-local-syntax (define-scheme-syntax pat . body)
  (let ((foo (lambda (name val)
                `(set (syntax-table-entry scheme-syntax-table ',name)
                      ,val))))
     (cond ((atom? pat)
            (foo pat (car body)))
           (else
            (foo (car pat) `(macro-expander ,pat . ,body))))))

;; collect  --??

(define-scheme-syntax (cons-stream hd tl)
  `(cons ,hd (,(t-syntax 'delay) ,tl)))

;; This isn't quite right; if a DEFINE is found in a weird place, an
;; error should be signalled.

;; Also, DEFINE should check its syntax, so that (DEFINE A B C) is an
;; error, not the same as (DEFINE A B) as it is now.


(define-scheme-syntax (define pat . body)
   (parse-top-level-define
      pat body
          (lambda (name val)
             `(,(t-syntax 'block)
		(,(t-syntax 'lset-variable-value) ,name ,val)
		',name))))

(define-scheme-syntax (lambda vars . body)
  `(,(t-syntax 'lambda) ,vars ,@(process-body body)))

;;; "Named" LET 

(define-scheme-syntax (let specs . body)
  (if (or (pair? specs) (null? specs))
      `(,(t-syntax 'let) ,specs ,@(process-body body))
      (let ((tag specs)
            (specs (car body))
            (body (cdr body)))
        `((,(t-syntax 'labels) ((,tag (,(t-syntax 'lambda) ,(map car specs)
					  ,@(process-body body))))
            ,tag)
          ,@(map cadr specs)))))

(define-scheme-syntax (let* specs . body)
  `(,(t-syntax 'let*) ,specs ,@(process-body body)))

(define-scheme-syntax (letrec specs . body)
  `(,(t-syntax 'labels) ,specs ,@(process-body body)))

;;; Is LOCALE no longer supported in T3?  If so, flush this or redefine
;;; it in terms of MAKE-LOCALE and EVAL!

(define-scheme-syntax (make-environment . body)  ;Yow!
  (let ((name (generate-symbol 'make-environment)))
    `(,(t-syntax 'locale) ,name ,@body ,name)))

;;; Permit PC-Scheme use of SET! like T's SET (e.g. (SET! (CAR X) Y)).

(define-scheme-syntax (set! var val)
  `(,(t-syntax 'set) ,var ,val))

(define-scheme-syntax sequence (t-syntax 'block))
(define-scheme-syntax begin    (t-syntax 'block))

(walk (lambda (sym)
         (set (syntax-table-entry scheme-syntax-table sym)
              (syntax-table-entry standard-syntax-table sym)))
      '(quote
        quasiquote
        cond
        if
        and
        or
        do
        case
        delay
        block              ;; T2.8's internal macros use BLOCK, not #[Syntax BLOCK]. Sigh.
        pp
        trace
        untrace
        locative           ;; for TRACE
        var-locative       ;; for TRACE
        ignore
        ignorable
	time
        select
	define-syntax
	define-local-syntax
	define-constant
	))           ;; OBJECT (for PP hack) (probably no longer needed - JAR)

(define-scheme-syntax (access var env)
 `(environment-ref ,env ',var))


;;****************************************************************************
'SCHEME_SYNTAX
