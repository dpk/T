(herald untime (env tsys))

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

;*** Internal Time Constants (Implementation Dependent)
;*** ============================================================
(define-constant *internal-time-units-per-second* 1)
(define-constant *internal-base-date* "1 January 1970 00:00:00 GMT")

;*** Internal Time and Date routines (Implementation Dependent)
;*** ============================================================

(define get-time
  (let ((itime (unix-gettimeofday (make-unix-time)
                                  (make-unix-time-zone))))
        (dtime nil))
    (object nil
      ((decode-time self) ...)
      ((time->string self fmt) ...)
      ((print self)...)



;*** Timer stuff

;*** (USLEEP N)
;*** =========================================================================
;*** Sleep for N micro-secs.
;***
(define (usleep n)
  (if (pointer-less? (xcall *usleep-xenoid* 0 (fixnum->pointer n))
	             (fixnum->pointer 0))
      (unix-error)
      T))
