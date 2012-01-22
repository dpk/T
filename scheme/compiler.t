(herald compiler)

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

(*define scheme-env 'compile-file
 (lambda (spec)
   ((*value orbit-env 'totally-comfile)
    spec
    spec
    standard-read-table
    scheme-syntax-table
    scheme-early-binding-env)))

(define (create-scheme-support . files)
  (let ((env ((*value orbit-env 'make-empty-early-binding-locale)
              'scheme-early-binding-env)))
    (walk (lambda (n)
            (cond ((standard-early-binding-env n)
                   => (lambda (d)
                        (set (env n) d)))))
          scheme-from-t)
    (walk (lambda (p)
            (cond ((standard-early-binding-env (cadr p))
                   => (lambda (d)
                        (set (env (car p)) d)))))
          scheme-aliased-from-t)
    (walk (lambda (f)
            ((*value orbit-env 'load-early-bindings) f env))
          files)
    env))
    
(define scheme-early-binding-env
  (create-scheme-support
   (list 'tscheme 'runtime (*value orbit-env '*information-file-extension*))))

