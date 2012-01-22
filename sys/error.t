(herald error (env tsys))

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

;++ Someday we should include an explanitory index of error messages
;++ in the manual.  In order to do this the errors should have
;++ reasonably short explanitory names.
;++
;++ Errors really want to be printed with a prefix format.  We really
;++ need something like Water's PP.

(define internal-error-notice
  #.(format nil "~%~a~%~a~%~a~%~a~%"
      "****"
      "****    This is an internal error. Please inform the"
      "****    implementors by sending mail to T3-BUGS@YALE."
      "****"))

;;; VM-ERROR is called only inside the Virtual Machine.  It indicates
;;; that something is seriously amiss.  If the Z-SYSTEM is present
;;; we try to use it to give an error message.  If an error is
;;; encountered while the GUARD is set or if the Z-SYSTEM isn't
;;; present we punt to FATAL-ERROR which does whatever the local-os
;;; allows.


(define vm-error
  (let ((guard  nil)
        (notice? '#t))
    (lambda (type fmt . args)
      (cond ((and (z-system-present?) (not guard))
             (bind ((*z?*  t)
                    (guard t))
               (let ((out (error-output)))
                 (z-format out "~%** VM Error (~a): " type)
                 (apply z-format out fmt args)
                 (if notice? (vm-write-string out internal-error-notice))
                 (vm-force-output out)))
             (bind ((notice? '#f))
               (z-breakpoint)))
            (else
             ;; punt to the machine debugger
             (let ((out (error-output)))
               (vm-newline out)
               (vm-write-string out "** VM Error while reporting error!")
               (vm-write-string out internal-error-notice)
               (fatal-error)))))))

;++ Move this to the local os hardware exception module.

;++ When the system is more robust (VM-ERROR-OUTPUT) should be
;++ a broadcast port which writes both to (ERROR-OUTPUT) and to
;++ (VM-ERROR-LOG) a file in the (THE-T-SYSTEM-DIRECTORY).
;++ A log entry should consist of (VM-VERSION), (DATE&TIME),
;++ and any arguments to the call to VM-LOG.

;(define (vm-log . args)
;  (apply vm-write (vm-error-log) (vm-version) (date&time) args))

;;; Fatal error
;++ This routine should go to the machine debugger if it can.
;++ Someday maybe it will do a core dump (and/or checkpoint).

(define (fatal-error) (exit))

;;; This error is called if a hardware exception occurs while control
;;; is inside the critical region of the hardware exception handler.
;;; See the local os hardware exception module.

;;;  Errors detected by ICALL 

(define (icall-bad-proc p args)
  (let* ((proc (or (identification p) p))
         (fmt  (cond ((not (reasonable? proc))
                      "attempt to call a corrupt datum~%**~10t~s")
                     ((symbol? proc)               ; Cater to the confused
                      "attempt to call a symbol or nonvalue~%**~10t~s")
                     (else 
                      "attempt to call a non-procedure~%**~10t~s"))))
    (apply (error fmt (cons proc args)) args)))

(define (icall-wrong-nargs p args)
  (let* ((n     (car (argspectrum p)))
         (nary? (cdr (argspectrum p)))
         (id    (cond ((identification p))
                      (else
                       (format nil "#{object internal to ~a}"
                               (get-proc-name (extend-header p)))))))
    (error (list "wrong number of arguments to procedure -~%"
                 "**~10t~a~%**~10t~a takes~a ~a argument~p.~%")
           (cons id args)
           id
           (if nary? " at least" "")
           n
           n)))

(define (cont-wrong-nargs p . args)
  (let* ((m     (length args))
         (n     (car (argspectrum p)))
         (nary? (cdr (argspectrum p))))
    (error "returned ~a value~p when~a ~a ~a expected -~%**~10t~s~%"
           m            
           m
           (if nary? " at least" "")
           n
           (if (fx= n 1) "was" "were")
           (cons (or (identification p) p) args))))

(define (apply-too-many-args proc) 
  (nc-error "exceeded maximum number of arguments while applying ~a"
            proc))

(define (handle-undefined-effect string template)
  (nc-error "undefined effect - ~a ~%**~10tin procedure ~s~%"
            string
            (or (get-proc-name template) 'anonymous)))

(define (heap-overflow-error)
  (nc-error "heap overflow"))

(define (undefined-effect . stuff)
  (error "call to ~s~%  ~s" 'undefined-effect `(undefined-effect . ,stuff)))

(define (error fmt . args)
  (if (not *z?*)
      (signal-error *unspecific-error-type* fmt args)
      (apply vm-error 'Z fmt args)))

(define (non-continuable-error fmt . args)
  (if (not *z?*)
      (signal-error *non-continuable-error-type* fmt args)
      (apply vm-error 'ZNC fmt args))
  (not-continuable))

(define nc-error non-continuable-error)

(define (not-continuable)
  (error "The error you encountered is not continuable.")
  (breakpoint)
  (not-continuable))


;;; Warnings.

(define (warning fmt . args)
  (let* ((flag (warn))
         (out  (cond ((false? flag) (null-port))
                     (else          (error-output)))))
    (format out "~&;** Warning: ")
    (apply format out fmt args)
    (fresh-line out)
    (if (eq? flag 'break) (breakpoint) (no-value))))

;;; Three settings true, false, or 'BREAK.

;++ need a better name, maybe break-on-warning
(define-simple-switch warn
  (lambda (val)
    (or (eq? val '#f) (eq? val '#t) (eq? val 'break)))
  '#t)

;;; Language level errors.

(define (losing-xcond)
  (error "no clause selected in ~s expression" 'xcond))

(define (losing-xcase)
  (error "no clause selected in ~s expression" 'xcase))

(define (losing-xselect)
  (error "no clause selected in ~s expression" 'xselect))


;;; Undefined values

(define (undefined-value . stuff)
  (cond ((null? stuff)
         ;; Don't close over STUFF
         (object nil
                 ((print self port)
                  (format port "#{Undefined-value~_~a}"
                          (object-hash self)))))
        (else
         (object nil
                 ((print self port)
                  (format port "#{Undefined-value~_~a"
                          (object-hash self))
                  (walk (lambda (x) (format port "~_~a" x))
                        stuff)
                  (write-char port #\}))))))


(define undefined-if-value      (undefined-value "undefined IF value"))
(define unbound-label           (undefined-value "unbound label"))
(define let-missing-initializer (undefined-value "LET missing initializer"))
(define no-more-cond-clauses    (undefined-value "no more COND clauses"))
(define case-fell-off-end       (undefined-value "CASE fell off end"))
(define select-fell-off-end     (undefined-value "SELECT fell off end"))
