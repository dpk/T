(herald readtable (env tsys))

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

;;;; read-table manipulation

;;; read tables consist of data which drives read and print.

;;; output control:


;;; for now, the read table is a vector with one entry for each ascii
;;; character.  each such entry can be either a procedure, in which case the
;;; character is a procedure for a read macro, or (the value of) one of:
;;;   %%whitespace
;;;   %%ignored
;;;   %%constituent
;;;   %%escape-char
;;;   %%undefined
;;; thanks to common lisp (i.e. steele) for the general shape of the thing.

(define-constant %%whitespace  0)
(define-constant %%ignored     1)
(define-constant %%constituent 2)
(define-constant %%escape-char 3)
(define-constant %%undefined   4)

(define-integrable (read-macro? syn) (not (fixnum? syn)))       ; hack!

(define-integrable (read-table? obj) (rt? obj))

;;; the read table structure type.

(define-structure-type rt
  id                ; identification
  mutable?          ; true if writable
  vector            ; vector with one entry per ascii code
  translator        ; consituent translation function for atom scan
  string->symbol    ; thing to call if there are slashified chars  (???)
  radix             ; input radix
  recognizer        ; atom recognation functional  (???)
  keyword-table     ; keyword table for #[...]
  (((set-immutable rt)  (set (rt-mutable? rt) nil))
   ((mutable? rt)       (rt-mutable? rt))
   ((identification rt) (rt-id rt))
   ((print rt port)
    (format port "#{Read-table~_~s~_~s}" (object-hash rt) (rt-id rt)))))

(define-integrable (char-syntax rt ch)
  (vref (rt-vector rt) (char->ascii ch)))

(define vanilla-read-table
  (let ((rt (make-rt)))
    (set (rt-id rt)         'vanilla-read-table)
    (set (rt-mutable? rt)   nil)
    (set (rt-vector rt)
         (let ((v (vector-fill (make-vector number-of-char-codes)
                               %%undefined)))
           (do ((i 0 (fx+ i 1)))
               ((fx>= i number-of-char-codes))
             (cond ((graphic? (ascii->char i))  ; careful - don't be circular
                    (set (vref v i) %%constituent))))
           (walk (lambda (ch)
                   (set (vref v (char->ascii ch)) %%whitespace))
                 '(#\space #\return #\linefeed #\tab #\form))
           (set (vref v (char->ascii #\rubout)) %%ignored)
           (set (vref v (char->ascii #\escape)) %%constituent) ; pacify mit?
           v))
    (set (rt-translator rt)     %char-upcase)
    (set (rt-string->symbol rt) string->symbol)
    (set (rt-radix rt)          10)
    (set (rt-recognizer rt)     recognize-atom)
    rt))

(define (make-read-table super id)
  (let ((rt (copy-structure (enforce read-table? super)))
        (new (make-vector number-of-char-codes)))
    (set (rt-id rt) id)
    (set (rt-mutable? rt) t)
    (do ((i 0 (fx+ i 1)))
        ((fx>= i number-of-char-codes)
         (set (rt-vector rt) new)
         rt)
      (let ((z (vref (rt-vector rt) i)))
        (set (vref new i)
             (cond ((read-macro? z) (copy-read-table-entry z))
                   (else z)))))))

(define-operation (copy-read-table-entry syn) syn)

(define read-table-entry
  (object (lambda (rt ch)
            (char-syntax rt (enforce char? ch)))
          ((setter self)
           (lambda (rt ch val)
             (cond ((rt-mutable? rt)
                    (set (vref (rt-vector rt) (char->ascii (enforce char? ch)))
                         val))
                   (else
                    (error "attempt to alter an immutable read-table~%  ~s"
                           `(set (read-table-entry ,self ,ch) ,val))))))))

(define set-read-table-entry (setter read-table-entry))

;++ This seems like a wierd default method.
(define-operation (establish-read-table-entry val ch)
  (cond ((and (eq? val %%escape-char) (null? *escape-char*))
         (set *escape-char* ch))))

(define-predicate delimiting-read-macro?)

(define (constituent-syntax? e)     ; used by print - can go away some day
  (cond ((fixnum? e)
         (fx= e %%constituent))
        (else
         (not (delimiting-read-macro? e)))))

;;; hack for reading/printing in different radices.
;;; if consing is a worry, pool these things.

(define (rt-with-radix rt radix)
  (let ((new-rt (copy-structure rt)))
    (set (rt-radix new-rt) radix)
    new-rt))
