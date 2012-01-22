(herald unix_fault (env tsys))

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

;;; To do:
;;;     exception frame handle
;;;     interrupt frame handle
;;;     interrupt-queue
;;;     heap-guard-handler
;;;     stack-guard-handler


;;; Fault frame hacking
;;;  fault-frame-header/number of slots
;;;  foreign-call-cont

;; otherwise:

;;;  fault-frame-header/number of slots
;;;  number of pointers on stack at fault
;;;  hack top of stack if PC is in kernel, else 0
;;;  PC     
;;;  P
;;;  A1     
;;;  A2     
;;;  A3
;;;  ..   
;;;  AN     
;;;  TP
;;; extra scratch
;;; extra pointer
;;;   .
;;; pointer temps
;;; scratch-temps
;;; ... ...
;;; ... ...
;;; ... ...
;;;  T stack begins here    

(define-constant foreign-fault-frame? alt-bit-set?)
                                      
(define (fault-frame-slots frame)
  (cond ((foreign-fault-frame? frame)
         (bytev-length frame))
        (else
         (fx+ (bytev-length frame)                ; size
              (if (eq? (extend-elt frame 1) 0)
                  (extend-elt frame 0)            ; pointers on top
                  1)))))                          ; hack

(define-handler fault-frame
  (object nil
      ((frame-previous self) 
       (make-pointer self (fault-frame-slots self)))
      ((crawl-exhibit self) (crawl-exhibit-fault-frame self))
      ((print-type-string self) "Fault-frame")))
                                                                  
(define (print-register frame name index)
  (let ((out (crawl-output)))
    (format out " ~s = " name)
    (print-one-line (extend-elt frame index) out)
    (newline out)))


(define (make-error-handler msg)
  (lambda (signal frame)
    (ignore signal frame)
    (enable-signals)
    (error msg)))

(define (make-NC-error-handler msg)
  (lambda (signal frame)
    (ignore signal frame)
    (enable-signals)
    (non-continuable-error msg)))


;;; Unix signal handler

(define-operation (get-handler obj type))

;++ this should be doing arg checking

(define signal-handler
  (let ((handler-vector (vector-fill
                         (make-vector (fx+ number-of-signals 1))
                         'default)))
    (object (lambda (signal frame)
              (unwind-protect 
               ((vref handler-vector signal) signal frame)
               (enable-signals)))
      ((setter self)
       (lambda (signal class handler)
         (set (vref handler-vector signal) handler)
         (if (fx= (set-signal signal class) -1)
             (error "call to sigvec failed for signal ~d" signal))))
       ((get-handler self type)
        (vref handler-vector type)))))

(define set-signal
  (let ((sigvec-struct (make-bytev 12))
        (return-struct (make-bytev 12)))
    (set (mref-integer sigvec-struct 8) 0)
    (set (mref-integer sigvec-struct 4) %%SIGINT)   ; sigint disabled    
    (lambda (type class)
      (set (mref-integer sigvec-struct 0)
           (xcase class
             ((A) (system-global slink/interrupt-handler))
             ((E) (system-global slink/interrupt-handler))
             ((D) 0)
             ((I) 1)))
      (unix-sigvec type sigvec-struct return-struct))))

(define-foreign unix-sigvec (sigvec (in rep/integer)
                                    (in rep/extend)
                                    (in rep/extend))
                rep/integer)

(define-integrable (set-mask-bit mask n)
  (fixnum-logior mask (fixnum-ashl 1 (fx- n 1))))

(define-integrable (clear-mask-bit mask n)
  (fixnum-logand (fixnum-lognot (fixnum-ashl 1 (fx- n 1))) mask))

(define (disable-signal sig)
  (unix-sigblock (set-mask-bit 0 sig)))

(define (enable-signals)
  (unix-sigsetmask 0))                                     

(define (enable-signal sig)
  (unix-sigsetmask (clear-mask-bit (unix-sigblock 0) sig)))

(define-foreign unix-sigsetmask (sigsetmask (in rep/integer))
                rep/integer)

(define-foreign unix-sigblock (sigblock (in rep/integer))
                rep/integer)

;;; Standard signal handlers.
;++ these should take type and frame as arguments

(define (sigint-handler signal frame)
  (enable-signals) 
  (if (not (foreign-fault-frame? frame))
      (breakpoint "Interrupt")
      (let ((stamp (gc-stamp)))
        (breakpoint "Interrupt")
        (if (fxn= (gc-stamp) stamp)
            (non-continuable-error "Interrupted code can't continue due to GC")))))


(define (sigquit-handler signal frame)
  (ignore signal)
  (enable-signals)
  (if (not (foreign-fault-frame? frame))
      (z-breakpoint)
      (let ((stamp (gc-stamp)))
        (z-breakpoint)
        (if (fxn= (gc-stamp) stamp)
            (non-continuable-error "Interrupted code can't continue due to GC")))))


;;; Initialize the condition system.  This procedure must be called
;;; to enable the T error system.  It should be called as soon as
;;; possible during the startup sequence.

(define (initialize-condition-system)
  (do ((l *signals* (cdr l)))
      ((null? l)
       (set (signal-handler %%SIGINT 'A)  sigint-handler)
       (set (signal-handler %%SIGQUIT 'A) sigquit-handler))
    (destructure (((type class handler msg) (car l)))
      (set (signal-handler type class)
           (case handler
                 ((error)
                  (make-error-handler msg))
                 ((non-continuable)
                  (make-NC-error-handler msg))
                 ((default) 'default)
                 ((ignore)  'ignore)
                 (else handler))))))

;;; Exit from T, optionally setting the return code

(lset exit-agenda (make-agenda 'exit-agenda))

(define (exit . arg)
  (exit-agenda)
  (unix-exit (if (null? arg) 0 (car arg))))

(define-foreign unix-exit (exit (in rep/integer))
                rep/undefined)

;;; Local OS error handling

(define-integrable (check-status status)
  (if (fx< 0 status) (local-os-error status)))

(define (local-os-error STATUS)
  (error "~&** VM Unix error - ~a" (local-os-error-message status)))

(define (local-os-error-message status)
  (ignore status)
  (let ((msg (get-string-buffer-of-size 128)))
    (set (string-length msg) 128)
    (unix-error msg 128)
    (set (string-length msg) (string-posq #\null msg))
    (let ((msg1 (copy-string msg)))
      (release-string-buffer msg)
      msg1)))

(define-foreign unix-error (get_unix_error_msg (in rep/string)
                                               (in rep/integer))
                rep/undefined)
