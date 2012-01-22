(herald debug (env tsys))

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

;;;; random collection of debugging stuff

;;; backtrace


(define (*backtrace frame)
  (format (debug-output) "~& continue into~25t module~40t code~%")
  (do ((frame frame (previous-continuation frame)))
      ((null? frame) repl-wont-print)
    (frame-print-synopsis frame (debug-output))))
                                          
(define (previous-continuation frame)
  (cond ((supreme-frame? frame)
         (get-next-frame frame))
        (else
         (iterate loop ((next (get-next-frame frame)))
           (cond ((null? next) nil)
                 ((supreme-frame? next)
                  (get-next-frame next))
                 (else (loop (get-next-frame next))))))))
             
(define (get-next-frame frame)
  (iterate loop ((frame (frame-previous frame)))
   (cond ((null? frame) nil)
         ((frame? frame) frame)
         (else
          (loop (frame-previous frame))))))

(define-operation (frame-print-synopsis frame port)
  (let ((thing (cond ((interpreter-frame? frame)
                      (interpreter-frame-code frame))
                     (else
                      (extend-header frame)))))
    (let ((proc-name (get-proc-name thing))
          (loaded-file (get-loaded-file thing))
          (disclosed (disclose thing)))
      (cond (proc-name (format port " ~s" proc-name))
            (else      (format port " (anonymous)")))
      (cond (loaded-file (format port "~25t ~s" (identification loaded-file)))
            (else        (format port "~25t (none)")))
      (cond (disclosed
             (set (hpos port) 40)
             (write-spaces port 1)
             (print-one-line disclosed port)))
      (newline port))))

(define (frame-disclose frame)      ; used by crawl.
  (and (frame? frame)
       (interpreter-frame? frame)
       (disclose (interpreter-frame-code frame))))

(define-operation (get-loaded-file obj)
  (cond ((bogus-entity? obj)
         (get-loaded-file (bogus-entity-handler obj)))
        ((template? obj) (template-unit obj))
        ((closure? obj)
         (get-loaded-file (extend-header obj)))
        (else nil)))

(define-operation (get-environment obj)
  (cond ((and (frame? obj)
              (interpreter-frame? obj))
         (interpreter-frame-env obj))
        ((bogus-entity? obj)
         (get-environment (bogus-entity-handler obj)))
        ((closure? obj)
         (let ((probe (unit-env (template-unit (extend-header obj)))))
           (cond ((environment? probe) probe)
                 (else nil))))
        (else nil)))

(define-operation (disclose obj) nil)

(define-operation (where-defined proc)
  (cond ((get-loaded-file proc) => loaded-file-source)
        (else nil)))
