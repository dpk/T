(herald locative (env tsys))

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

(define-settable-operation (contents loc))

(define set-contents (setter contents))

(define-operation (define-contents loc value))

(define-predicate locative?)

;;; Other locatives are made using the MAKE-LOCATIVE operation.

(define-operation (make-locative proc . args)
  (object nil
          ((contents self) (apply proc args))
          ((set-contents self val)
           (apply (setter proc) (append args (list val)))) ; choke!
          ((locative? self) t)
          ((print-type-string self) "Locative")))



;;; Delays

(define-predicate delay?)

(define-operation (force obj) obj)

;;; This value is local to the module

(define %unforced (undefined-value 'unforced-delay))

(define (make-delay proc)
  (let* ((value %unforced)
         (get-value  (lambda ()
                       (if (eq? value %unforced) (set value (proc)))
                       value)))
    (object (lambda () (get-value))
      ((force self) (get-value))
      ((delay? self) t)
      ((print-type-string self)
       (if (eq? value %unforced) "Delayed" "Forced")))))
