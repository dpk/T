(herald zload
        (env tsys (osys kernel) (osys vm_port)))

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

;;; z-load.

(lset *z-print-load-message?* t)

(define (z-load path env)
  (z-load-file path env nil))

(define (z-load-file path env bin?)
  (if *z-print-load-message?* 
      (z-format (standard-output) "~&~%;loading ~s~%" path))
  (cond (bin?
         (let ((path (if (symbol? path)
                         (string-append (symbol->string path) ".MO")
                         path)))
           (with-open-ports ((port (open-port path 'in)))
             (load-comex port env))))
        (else
         (let ((path (if (symbol? path)
                         (string-append (symbol->string path) ".T")
                         path)))
           (with-open-ports ((port (open-port path 'in)))
             (z-load-source port env))))))

(define (z-load-source port env)
  (let ((firstfrob (let ((frob (z-read port)))
                     (cond ((and (pair? frob)
                                 (eq? (car frob) 'herald))
                            (z-read port))
                           (else frob)))))
    (iterate loop ((frob firstfrob) (val nil))
      (cond ((eof? frob) val)
            (else
             (let ((val (z-eval frob env)))
               (loop (z-read port) val)))))))


(comment ;this will replace the above
  (define (z-load-file path env)
    (with-open-ports ((port (open-port path 'in)))
      (if *z-print-load-message?* 
          (z-format (standard-output) "~&;loading ~s~%" path))
      (let ((firstfrob (let ((frob (z-read port)))
                         (cond ((and (pair? frob)
                                     (eq? (car frob) 'herald))
                                (z-read port))
                               (else frob)))))
        (cond ((comex? firstfrob)
               (receive (unit code) (install-comex firstfrob env)
                 (set (weak-table-entry code-unit-table code) unit)
                 (add-to-population code-population code)
                 ;; run top-level forms
                 ((unit-top-level-forms unit))))
              (else
               (iterate loop ((frob firstfrob) (val nil))
                 (cond ((eof? frob) val)
                       (else
                        (let ((val (z-eval frob env)))
                          (loop (z-read port) val))))))))))
)

(define (z-load-system)
  (z-load-file "~osys/syntax.mo" tvm-env t)
  (walk (lambda (file) (z-load-file file tvm-env t))
        '(ratio
          aegis_float
          arith
          m68_bignum
          bignum_fixnum
          bignum_util
          bignum
          bignum_arith
          dispatch
          ;    pfloat
          equality
          ;(osys single_float)
          ;(osys double-float)
          ;(osys complex)

          ;; i/o system
          port
          aegis_port
          sort
          herald
          load
          infinite_vector
          dump
          dump_comex

          ;; Syntax system
          macros       ; ENV calls MAKE-TABLE which calls GC_STAMP
          aegis_macros ; macro's for foreign calls
          cond
          let
          backquote
          object       ; object macro
          modify


          ;; REPL
          recognize
          readtable
          read
          format
          eval
          repl
          aegis
          fs_parse
          fs
          tree
          crawl
          debug
          trace
          pp
          exports
          obsolete
          tsystem
          )))
