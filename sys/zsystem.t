(herald zsystem
        (env tsys
             (osys kernel)
             (osys list)
             (osys vector)
             (osys vm_port)))

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

;;; Z system object, read-eval-print loop, etc.

;;; We herein explicitly avoid any dependence on entities, operations,
;;; or format.  we must not throw away the scaffolding!  at some
;;; point the "real" printer will take over and we can just stop
;;; loading this one, and let it lie dormant until the next
;;; transportation effort.  Ha!

;;; *z-repl-env* is set to a real environment later in the bootstrap.

(lset *z-repl-env* bootstrap-env)

(define (z-read-eval-print-loop)
  (iterate loop ()
    (let ((out standard-output)
          (fmt (if (fx= *break-level* 0) "~&> " "~&~s(Z): ")))
      (z-prompt (out) fmt *break-level*)
      (let ((form (z-read (standard-input))))
        (cond ((eof? form) form)
              (else
               (receive vals 
                        (z-eval form *z-repl-env*)
                 (cond ((null? vals)
                        (format (out) "~&;no values.")
                        (loop))
                       ((not (null? (cdr vals)))
                        (format (out) ";multiple values:")
                        (do ((l vals (cdr l))
                             (i 0 (fx+ i 1)))
                            ((null? l) (loop))
                          (format (out) "~% [~s] ~s" i (car l))))
                       (else
                        ;; single value
                        (z-print (car vals) (out))
                        (loop))))))))))

(define (z-top-level)
  (z-breakpoint "~&Z top level"))

(define (z-reset)
  (set *top-level* z-top-level)
  (**reset** nil))
                                      
;;; We assume that catch works.

(define (z-breakpoint . args)
  (let* ((z     *z?*)
         (cont  **ret**)
         (level *break-level*))
    (receive vals (catch ret
                    (set *z?*          '#t)
                    (set **ret**       ret)
                    (set *break-level* (fx+ *break-level* 1))
                    (if args (apply z-prompt (standard-output) args))
                    (z-read-eval-print-loop)
                    (cont))
      (set *z?*          z)
      (set **ret**       cont)
      (set *break-level* level)
      (apply return vals))))

(define z-system
  (create-system 'Z-system 1 0 3 true true true t-copyright-notice '()))

(set *top-level* z-top-level)
(set (z-system-present?) '#t)
(set *z-repl-env* tvm-env)
