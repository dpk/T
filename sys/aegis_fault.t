(herald aegis_fault (env tsys))

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

;;; Fault frame hacking

;;; Fault frame: (see also start_t.asm, & aem68kernel.t)
;;; out of foreign-code:

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
;;;  AN     
;;;  TP
;;; extra scratch
;;; extra pointer
;;;   .
;;; pointer temps
;;; scratch-temps
;;;  .
;;; offset from this slot to diagnotic frame
;;; old SP
;;; fault SP (redundant for interrupt dispatcher ease)
;;; some number of bytes for alignment
;;; ...
;;; ... dc.w $DFDF          (apollo diagnostic frame begins here)   
;;; ... dc.l fault status
;;; ... D0
;;; ... ...
;;; ... A7
;;; ... ...
;;;  T stack begins here    

;;; FF = fault frame        (a T concept)
;;; DF = diagnostic frame   (an Aegis concept)

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

;;; Fault handler


(define-constant fault_$stop                   #x120018)
(define-constant fault_$process_interrupt      #x12001f)
(define-constant fault_$quit                   #x120010)
(define-constant mst_$guard_fault              #x4000a)
(define-constant fault_$guard                  mst_$guard_fault)
(define-constant bat_$disk_full                #x10002)
(define-constant time_$itimer_real             #xD0007)
(define-constant fault_$hangup                 #xb000e)

;;; We arrive here whenever we get a T-asynchronous fault.
;;; Be careful not to do any consing, because the disk might be full.

(define (re-enable-faults)
    (pfm_$enable)
    0)

(define-foreign pfm_$enable ("PFM_$ENABLE") ignore)

;;; FAULT-ENTRY is called from the lap code in aem68kernel.
;++ known bug fix later
;++ pfm_cleanup 
;++ pfm_signal
;++ valid frame

(define (fault-entry status frame)
  (re-enable-faults)
  (select status
    ((fault_$stop fault_$hangup) (exit))
    ((fault_$guard)  
     (check-continue frame (lambda () (non-continuable-error "Stack overflow"))))
    ((fault_$process_interrupt) 
     (check-continue frame (lambda () (z-breakpoint "Z system interrupt"))))
    ((fault_$quit) 
     (check-continue frame (lambda () (breakpoint "Interrupt"))))
    ((time_$itimer_real) 
     (check-continue frame timer-interrupt))
    (else
     (non-continuable-error "~a (status ~x)" 
                            (local-os-error-message status) 
                            status))))

(define (check-continue frame thunk) 
  (if (not (foreign-fault-frame? frame))
      (thunk)
      (let ((stamp (gc-stamp)))
        (thunk)
        (if (fxn= (gc-stamp) stamp)
            (non-continuable-error "Interrupted code can't continue due to GC")))))

;;; Initialize the condition system.  This procedure must be called
;;; to enable the T error system.  It should be called as soon as
;;; possible during the startup sequence.

(define (initialize-condition-system) 
    (pfm_$establish_fault_handler 0
                                  4
                                  (vref (system-global slink/boot-args)
					3)   ; boot/interrupt-xenoid
                                  nil))
(define-foreign pfm_$establish_fault_handler
    ("PFM_$ESTABLISH_FAULT_HANDLER" (in rep/integer) 
                                  (in rep/integer-16-u) 
                                  (in rep/extend)
                                  (out rep/integer))
        rep/integer)


;;; Exit from T, optionally setting the return code.

(define-foreign pgm_$exit ("PGM_$EXIT") ignore)

(define-foreign pgm_$set_severity
  ("PGM_$SET_SEVERITY" (in rep/integer-16-u)) ignore)

(lset exit-agenda (make-agenda 'exit-agenda))

(define (exit . arg)
  (let ((severity (if (null? (car arg)) 0 (car arg))))
    (exit-agenda)
    (unwind-to-state nil)
    (pgm_$set_severity severity)
    (exit-and-dheap)))


;;; Local OS error handling
;++ apollo error text implementation
;++ nothing has been hacked for faults during GC (see t2)


(define-foreign error_$get_text
    ("ERROR_$GET_TEXT" (in     rep/integer)
                     (ignore rep/extend)
                     (out    rep/integer-16-u)
                     (ignore rep/extend)
                     (out    rep/integer-16-u)
                     (ignore rep/extend)
                     (out    rep/integer-16-u) )
    ignore)

;;; OS interface error checking utilities

(define-constant (CHECK-STATUS STATUS)
  (if (fxN= status 0) (local-os-error status)))

(define (local-os-error STATUS)
  (error "~&** VM Aegis error - ~a" (local-os-error-message status)))

(define (local-os-error-message status)
  (let ((subsys (make-string 80))
        (module (make-string 80))
        (code   (make-string 80)))
    (receive (sl ml cl)
             (error_$get_text status (string-text subsys) 0
                                     (string-text module) 0
                                     (string-text code)   0)
      (cond (*z?*
             (set (string-length code) cl)
             code)
            (else
             (set (string-length subsys) sl)
             (set (string-length module) ml)
             (set (string-length code)   cl)
             (format nil "~a [~a/~a]" code subsys module))))))
                              




