(herald handler (env tsys))


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

;;; Generic handlers  needs frame.t


(define *handlers*
  (vector-fill (make-vector 32) handle-unused-tag))
 
(define handle-unused-tag
  (object nil
    ((print self port)
     (format port "bad header type, addr = #x~x"
             (descriptor->fixnum self)))))

(define handle-immediate
  (object nil
    ((print self port)
     (format port "{Immediate_~s}"
             (fixnum-ashl (descriptor->fixnum self) 2)))))                                              

(define handle-nonvalue
  (object nil
    ((print self port) (format port "#{Nonvalue}"))))

(define-simple-switch bytev-elision fixnum? 8)

(define-handler bytev
  (let ((writer (lambda (port bytev count)
                  (let ((len (cond ((null? count) (bytev-length bytev))
                                   ((fx< count (bytev-length bytev))
                                    count)
                                   (else (bytev-length bytev))))
                        (writec (if (iob? port) vm-write-char write-char)))
                    (if count
                        (if (fx< len (bytev-length bytev))
                            (format port "#{Bytev (~a) " (object-hash bytev))
;++                            (format port "#[Bytev (~a) " (object-hash bytev))))
                            (format port "#{Bytev (~a) " (object-hash bytev))))
                    (do ((i 0 (fx+ i 1)))
                        ((fx>= i len) (no-value))
                      (let ((byte (bref bytev i)))
                        (if (fx= (fx-rem i 2) 0) (space port))
                        (writec port (digit->char (fx-ashr byte 4) 16))  
                        (writec port (digit->char (fx-and byte 15) 16))))
                    (if count
                        (if (fx< len (bytev-length bytev))
                            (write-string port " ... }")
;++                            (writec port #\])))
                            (writec port #\})))
                    (no-value)))))
    (object nil
      ((display self port)
       (writer port self nil))
      ((print self port)
       (writer port self (bytev-elision)))
      ((crawl-exhibit self)
       (writer (standard-output) self (bytev-length self))))))

(define (make-bytev-for-read key port rt)
  (let* ((l     (read-to-right-bracket port #\] (rt-with-radix rt 16)))
         (l     (cdr l))
         (bytev (make-bytev (fx-ashl (length l) 1))))
    (format t "~x~%" l)
    (iterate loop ((i 0) (l l))
      (cond ((null? l) bytev)
            (else
             (set (bref-16 bytev i) (car l))
             (loop (fx+ i 2) (cdr l)))))))

(define-handler foreign
  (object nil
    ((print self port)
     (format port "#{Foreign~_~s~_~x}"
             (foreign-name self)
             (mref-integer self 4)))))

(define-handler template
  (object nil
    ((print-type-string obj) "Template")))

(define-handler vcell
  (object nil
     ((contents self)
      (let ((z (vcell-contents self)))
        (cond ((nonvalue? z)
               (error "bound variable ~s has no value"
                      (vcell-id self)))
              (else z)))) 
      ((set-contents self value)
       (check-rebinding self nil set-contents)
       (*set self value))
      ((define-contents self value)
         ;; kludge to allow (define x x)
       (check-rebinding self t define-contents)
       (*set self value))
      ((locative? self) t)
      ((identification self) (vcell-id self))
      ((crawl-exhibit self)
       (exhibit-standard-extend self %%vcell-size 0 0))
      ((maybe-crawl-component self command)
       (cond ((and (nonnegative-fixnum? command)
                   (fx< command %%vcell-size))
              (crawl-push (extend-pointer-elt self command)))
             (else nil)))
      ((print-type-string self) "Value-cell")))
                                                              
(define-handler unit
  (object nil                          
    ((compiled-code? self) '#t)
    ((crawl-exhibit self)
     (exhibit-standard-extend self (unit-length self) 0 0))
    ((maybe-crawl-component self command)
     (cond ((and (nonnegative-fixnum? command)
                 (fx< command (unit-length self)))
            (crawl-push (extend-pointer-elt self command)))
           (else nil)))
    ((get-loaded-file self) self)
    ((loaded-file-herald self) (unit-herald self))
    ((loaded-file-source self) (unit-source-filename self))
    ((run-compiled-code self env) 
     (ignore env)  ;++ should there be an env arg?
     ((unit-top-level-forms self)))
    ((identification self) 
     (filename-name (herald-filename (unit-herald self))))
    ((print-type-string self) "Unit")))

(define-handler true
  (object nil
    ((print obj port) (write-string port "#T"))))

(define-handler cell                                                   
 (object nil
    ((contents self) (cell-value self))
    ((set-contents self value)
     (set (cell-value self) value))
    ((locative? self) t)
    ((print self port) (format port "#{Cell~_~s}" (cell-value self)))))
     
;;; Initialization

;(set (vref *handlers* (fixnum-ashr header/template       2)) handle-template)
;(set (vref *handlers* (fixnum-ashr header/slice          2)) handle-slice)
;(set (vref *handlers* (fixnum-ashr header/symbol         2)) handle-symbol)
;(set (vref *handlers* (fixnum-ashr header/vcell          2)) handle-vcell)
;(set (vref *handlers* (fixnum-ashr header/general-vector 2)) handle-vector)
;(set (vref *handlers* (fixnum-ashr header/bytev          2)) handle-bytev)
;(set (vref *handlers* (fixnum-ashr header/text           2)) handle-text)
;(set (vref *handlers* (fixnum-ashr header/foreign        2)) handle-foreign)
;(set (vref *handlers* (fixnum-ashr header/cell           2)) handle-cell)
;(set (vref *handlers* (fixnum-ashr header/vframe         2)) handle-vframe)
;(set (vref *handlers* (fixnum-ashr header/unit           2)) handle-unit)
(set (vref *handlers* (fixnum-ashr header/stack          2)) handle-stack)
;(set (vref *handlers* (fixnum-ashr header/weak-set       2)) handle-weak-set)
;(set (vref *handlers* (fixnum-ashr header/weak-alist     2)) handle-weak-alist)
;(set (vref *handlers* (fixnum-ashr header/weak-table     2)) handle-weak-table)
;(set (vref *handlers* (fixnum-ashr header/weak-cell      2)) handle-weak-cell)
