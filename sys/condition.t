(herald condition (env tsys))

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

;;;; Signals and errors
;;;  These are probably still too heavy weight!

;;;  A condition is signaled by being called.

(define (make-condition-type default-handler string id continuable?)
  (let ((handler default-handler))
    (object (lambda (fmt args)
              (labels ((instance
                        (object (lambda ()
                                  (receive vals (handler instance)
                                    (if continuable?
                                        (apply return vals)
                                        (not-continuable))))
                          ((print-signal self port)
                           (format port "~&** ~A: " string)
                           (apply format port fmt args)
                           (fresh-line port))
                          ((print-type-string self) string))))
                (instance)))
      ((condition-handler self) handler)
      ((set-condition-handler self val) (set handler val))
      ((identification self) id)
      ((print-type-string self) "Condition"))))

;;; Operations on conditions

(define-settable-operation (condition-handler condition-type))
(define set-condition-handler (setter condition-handler))

;;; Operation on condition instance

(define-operation (print-signal instance port))


;;; Utility for CONDITION-BIND (?)

(define (cons-condition-handler proc type)
  (let ((proc (enforce procedure? proc))
        (punt (condition-handler type)))
    (lambda (err)
      (proc err punt))))

;;; ---------- Error conditions.

;;; Error conditions in general

(lset *the-error* nil)
(lset *reporting-error?* nil)
(lset *abort-error-report* nil)

(define (make-error-type string id)
  (make-condition-type standard-error string id t))

(define (make-non-continuable-error-type string id)
  (make-condition-type standard-error string id nil))

;++ if the format statement doesn't have enough args
;++ you get a misleading error.

(define (standard-error err)
  (catch error-point
    (bind ((*the-error* (cons err error-point)))
      (catch abort
        (bind ((*reporting-error?* t)
               (*abort-error-report* abort))
          (let ((out (error-output)))
            ;; don't use ~2& - Z-FORMAT can't cope
;++            (format out "~&~%")
            (print-signal err out))))
      (breakpoint))))

;++ user versus system errors
;; the error system needs
;; format, i/o

(define (signal-error error-type f-string f-args)
  (cond ((not *reporting-error?*)
         (error-type f-string f-args))
        ((neq? *reporting-error?* '*reporting-error?*)
         (bind ((*reporting-error?* '*reporting-error?*)) 
           (format (error-output) "~&**** Error while reporting error!~%")
           (*abort-error-report* nil)))
        (else
         (apply vm-error 'signal f-string f-args))))

;;; Particular error conditions.

(define *unspecific-error-type*
  (make-error-type "Error" '*unspecific-error-type*))

(define *non-continuable-error-type*
  (make-non-continuable-error-type "Error" '*non-continuable-error-type*))

(define (syntax-error f-string . f-args)
  (signal-error *syntax-error-type* f-string f-args))

(define *syntax-error-type*
  (make-error-type "Syntax error" '*syntax-error-type*))

(define (read-error port f-string . f-args)
;++ flush  (clear-input port) ;++ why is this needed??
  (signal-error *read-error-type*
                (append (cond ((pair? f-string) f-string)
                              (else (list f-string)))
                        '("~&  (line ~S of port ~S)"))
                (append f-args (list (vpos port) port))))

(define *read-error-type*
  (make-error-type "Read error" '*read-error-type*))
