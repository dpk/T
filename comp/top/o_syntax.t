(herald orbit_syntax)

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

;;; Syntax changes from T2 to T3.

(define-syntax (lap vars . instructions)
  `(,primop/lap ,(create-primop-node instructions) ,@vars))

(define-syntax (lap-template  . instructions)
  `(,primop/lap-template ,(create-primop-node instructions)))

(define-syntax (define-foreign name name-params value)
  (let* ((vars (do ((i (length name-params) (- i 1))
                   (vars '() (cons (generate-symbol 'foreign) vars)))
                  ((= i 1) vars)))
	 (f-name (if (symbol? (car name-params))
		     (string->symbol
		      (string-append "_" (string-downcase! (symbol->string
							    (car name-params)))))
		     (string->symbol (car name-params))))
        (fo (create-literal-node (object nil 
                                   ((foreign-name self) f-name))))
        (reps (create-primop-node (reverse (cdr name-params))))
        (value-rep (create-primop-node value)))
    `(define ,name
       (lambda ,vars
         (call-foreign ,fo ,reps ,value-rep ,@vars)))))

(set (syntax-table-entry (env-syntax-table t-implementation-env) 'lap)
     (syntax-table-entry (env-syntax-table orbit-env) 'lap))

(set (syntax-table-entry (env-syntax-table t-implementation-env) 'lap-template)
     (syntax-table-entry (env-syntax-table orbit-env) 'lap-template))

(set (syntax-table-entry (env-syntax-table t-implementation-env) 'define-foreign)
     (syntax-table-entry (env-syntax-table orbit-env) 'define-foreign))
                                                                          
(set (syntax-table-entry standard-syntax-table 'define-foreign)
     (syntax-table-entry (env-syntax-table orbit-env) 'define-foreign))
                                                                          
                           
