(herald zprint
        (env tsys
             (osys kernel)
             (osys character)
             (osys string)
             (osys buffer)
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


;;; Z system printer
;;; Externel Entries: Z-PRINT, Z-WRITE, Z-FORMAT, Z-PROMPT

(lset *z-output-radix* 10)

(define-integrable (z-print form iob)  (z-write iob form))

(define (z-write iob form)
  (xcond ((not (reasonable? form))
          (z-write-random iob "Unreasonable" form))
         ((string? form)
          (vm-write-char iob #\")
          (vm-write-string iob form)
          (vm-write-char iob #\"))
         ((fixnum? form)
          (vm-write-fixnum iob form *z-output-radix*))
         ((null? form) (vm-write-string iob "()"))
         ((pair? form)
          (z-write-pair iob form))
         ((symbol? form)
          (plain-write-symbol iob form))
         ((extend? form)
          (z-write-extend iob form))
         ((true-header? form)
          (vm-write-string iob "#T"))
         ((char? form)
          (cond ((char= form #\rubout) (vm-write-string iob "#\\rubout"))
                ((char= form #\space) (vm-write-string  iob "#\\space"))
                ((char< form #\space)
                 (vm-write-string iob "#^")
                 (vm-write-char iob (ascii->char (fx+ (char->ascii form)
                                                     #o100))))
                ((char> form #\rubout)
                 (vm-write-string iob "#{Char ")
                 (vm-write-fixnum iob form *z-output-radix*)
                 (vm-write-char iob #\}))
                (else (vm-write-string iob "#\\")
                      (vm-write-char iob form))))
         ((nonvalue? form)
          (vm-write-string iob "{Has no value}"))))

(define (z-write-pair iob l)
  (cond ((null? l) (vm-write-string iob "()"))
        (else
         (vm-write-char iob #\()
         (iterate loop ((l l))
           (z-write iob (car l))
           (cond ((atom? (cdr l))
                  (cond ((not (null? (cdr l)))
                         (vm-write-space iob)
                         (vm-write-char iob #\.)
                         (vm-write-space iob)
                         (z-write iob (cdr l))))
                  (vm-write-char iob #\)))
                 (else
                  (vm-write-space iob)
                  (loop (cdr l))))))))

(define (z-write-extend iob form)
  (cond ((text?   form)
         (vm-write-string iob "#{Text ")
         (vm-write-text  iob form (text-length form))
         (vm-write-string iob "}"))
        ((vector? form) 
         (z-write-object iob "Vector"  (vector-length form)))
        ((vcell? form)
         (z-write-object iob "Vcell"   (vcell-id form)))
        ((foreign? form)
         ;; write the low 30 bits!
         (z-write-object iob "Foreign" (mref-integer form 0)))
        (else
         (z-write-random iob (cond ((closure?   form) "Closure")
                                   ((template? form) "Template")
                              ;++   ((float?    form) "Float")
                                   ((bytev?    form) "Bytev")
                                   ((unit?     form) "Unit")
                                   (else             "Extend"))
                          form))))


(define (z-write-random iob type-string obj)
  (z-write-object iob type-string (descriptor->fixnum obj)))

(define (z-write-object iob type-string id)
  (vm-write-string iob "#{")
  (vm-write-string iob type-string)
  (vm-write-space iob)
  (z-write iob id)
  (vm-write-char iob #\} ))

;;; Z-FORMAT

(define (z-format iob fmt . args)
  (z-format-aux iob fmt args '#t))  ; always force output in z system

(define (z-prompt iob fmt . args)
  (z-format-aux iob fmt args '#t))

(define (z-format-aux iob fmt args force?)
  (let ((fmt (chopy (cond ((string? fmt) fmt)
                          ((and (pair? fmt)
                                (string? (car fmt)))
                           (car fmt))
                          (else
                           (error "losing z-format string!")))))
        (iob (if (buffer? iob) 
                 iob
                 (error "losing z-format port - ~s" iob))))
    (iterate loop ((args args))
      (cond ((string-empty? fmt)
             (if force? (vm-force-output iob))
             (no-value))
            ((char= (char fmt) #\~)
             (string-tail! fmt)
             (let ((op (char-upcase (string-head fmt))))
               (string-tail! fmt)
               (case op
                 ((#\A) ((cond ((string? (car args)) vm-write-string)
                               ((char? (car args))   vm-write-char)
                               (else                 z-write))
                         iob
                         (car args))
                        (loop (cdr args)))
                 ((#\B) (vm-write-fixnum iob (car args) 2)
                        (loop (cdr args)))
                 ((#\C) (vm-write-char iob (car args))
                        (loop (cdr args)))
                 ((#\D) (vm-write-fixnum iob (car args) 10)
                        (loop (cdr args)))
                 ((#\O) (vm-write-fixnum iob (car args) 8)
                        (loop (cdr args)))
                 ((#\P) (if (fx> (car args) 1) (vm-write-char iob #\s))
                        (loop (cdr args)))
                 ((#\S) (z-write iob (car args))
                        (loop (cdr args)))
                 ((#\X) (vm-write-fixnum iob (car args) 16)
                        (loop (cdr args)))
                 ((#\%) (vm-newline iob)
                        (loop args))
                 ((#\&) (if (fx> (iob-h iob) 0) (vm-newline iob))
                        (loop args))
                 ((#\~) (vm-write-char iob #\~)
                        (loop args))
                 ((#\_) (vm-write-space iob)
                        (loop args))
                 ((#\T) (vm-write-space iob)
                        (loop args))
                 (else
                  (cond ((digit? op 10)
                         (z-skip-digits fmt)
                         (loop args))
                        ((whitespace? op)
                         (z-skip-whitespace fmt)
                         (loop args))
                        (else
                         (z-format iob "~~~c" op)))))))
            (else
             (vm-write-char iob (string-head fmt))
             (string-tail! fmt)
             (loop args))))))


(define (z-skip-digits str)
  (cond ((digit? (char str) 10)
         (z-skip-digits (string-tail! str)))
        (else str)))

(define (z-skip-whitespace str)
  (cond ((whitespace? (char str))
         (z-skip-whitespace (string-tail! str)))
        (else str)))
