(herald scheme (env tsys))

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

;; Contents:
;; ---------
;; Emulator for Scheme in T.
;; Make sure this is compiled before putting it in production use.
;;
;; Implements a mix of MIT Scheme and R3R Scheme (in T).
;; Intended to run the examples from S&ICP, the problem sets from 6.001, 
;; and MacScheme programs.

;; Using the interpreter:
;; ----------------------
;; (SCHEME-BREAKPOINT) -- Enters Scheme temporarily; <EOF> takes you back to T.
;; (SCHEME-TOP-LEVEL)  -- Enters Scheme permanently (at top-level).
;; (T-RESET)           -- Takes you back to T.
;; You might, for example, set up an INIT.T file for the students that
;; automatically did (SCHEME-TOP-LEVEL).

;; Report bugs to T3-BUGS.
;; ---------------------------------

;;****************************************************************************
;;                              ENVIRONMENT
;;****************************************************************************

(let ((scheme-internal-env (make-locale standard-env 'scheme-internal-env))
      (scheme-env (make-locale nil 'scheme-env)))

   (*define scheme-env 'scheme-internal-env scheme-internal-env)
   (*define scheme-internal-env 'scheme-env scheme-env)
   (*define standard-env 'scheme-internal-env scheme-internal-env)
   (*define standard-env 'scheme-env scheme-env)

   (load '(tscheme syntax) scheme-internal-env)
   (load '(tscheme system) scheme-internal-env)
   (load '(tscheme runtime) scheme-internal-env)
   (load '(tscheme compiler) scheme-internal-env)

   (*define standard-env 'scheme-breakpoint
       (*value scheme-internal-env 'scheme-breakpoint))
   (*define standard-env 'scheme-reset
       (*value scheme-internal-env 'scheme-reset))
   (*define standard-env 'scheme-top-level
       (*value scheme-internal-env 'scheme-top-level))

)

;;****************************************************************************
'SCHEME
