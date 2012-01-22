(herald system (env tsys))

;;; Copyright (c) 1985 Yale University
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

(import t-implementation-env source-file-extension)

(define (scheme-breakpoint)
  (bind (((repl-prompt) scheme-prompt)
	 ((source-file-extension) 'scm)
         ((repl-print)  scheme-repl-print))
    (breakpoint "Scheme" scheme-env)))

(define (scheme-top-level)
  (bind (((repl-prompt) scheme-prompt)
         ((repl-print)  scheme-repl-print)
	 ((source-file-extension) 'scm)
         ((repl-env)    scheme-env))
    (breakpoint "Scheme Top Level" scheme-env)))

(define (scheme-reset)
  (set (*value t-implementation-env '*top-level*) scheme-top-level)
  ((*value t-implementation-env '**reset**) nil))

(*define scheme-env   'reset (*value scheme-internal-env 'scheme-reset))
(*define standard-env 'scheme-reset (*value scheme-internal-env 'scheme-reset))
(*define scheme-env   't-reset (*value t-implementation-env 't-reset))

(define scheme-prompt
   (lambda (n)
     (if (<= n 0)
         "==> "
         (format nil "[~s]=> " n))))

;; Ignore the fact that the MIT dialect does newline-before-print instead of
;; newline-after-print.  Instead, try to put a blank line between the print
;; and the next prompt, so that it looks more like the output in SICP.

(define (scheme-repl-print obj port)
   (print obj port)
   (fresh-line port)
   (newline port))

(define (scheme-repl-eval exp env)              ;; not currently used
  (eval (scheme-preprocess-form exp) env))

(define (scheme-preprocess-form exp)
   (cond ((definition? exp)
          (receive (name val) (parse-define (cadr exp) (cddr exp))
             `(,(t-syntax 'define) ,name ,val)))
         (else exp)))

;;****************************************************************************
'SCHEME_SYSTEM

