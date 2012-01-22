(herald unix_timer (env tsys))

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


(define %%itimer_real    0)
(define %%itimer_virtual 1)
(define %%itimer_prof    2)

(define-foreign getitimer (getitimer (in rep/integer) (in rep/extend))
  rep/integer)

(define-foreign setitimer (setitimer (in rep/integer)
                                     (in rep/extend)
				     (in rep/extend))
  rep/integer)

(define %time (make-bytev 16))
(define %otime (make-bytev 16))
(define %stop (make-bytev 16))

(define-syntax (time expr . n)
  (let ((n (if n (car n) 1)))
     `((*value t-implementation-env '%monitor) ,n (lambda () ,expr))))


;** (%METER FN ITERATIONS TIME-OUT)
;** ===============================================================
;** Time INTERATIONS calls to FN.  The procedure should terminate
;** within TIME-OUT seconds or an interrupt will occur.

(define (%monitor repetitions thunk)
  (set (mref-integer %time 8) time-out)
  (set (mref-integer %time 12) 0)
  (setitimer %%itimer_virtual %time %otime)
  (receive vals (do ((i 1 (fx+ i 1)))
		    ((fx>= i repetitions) (thunk))
		  (thunk))
	   (getitimer %%itimer_virtual %time)
	   (let* ((usec (fx+ (fx* 1000000 (fx- (fx- time-out 1)
					       (mref-integer %time 8)))
			     (fx- 1000000 (mref-integer %time 12)))))
	     (format t "~&virtual time = ~s seconds~%"
		     (fl/ (->float usec) 1000000.0))
	     (setitimer %%itimer_virtual %stop %otime)
	     (apply return vals))))


(define time-out 1000000)