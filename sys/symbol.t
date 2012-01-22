(herald symbol
  (env tsys))

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

;;; general symbol table stuff.

(lset *symbol-delimiter* '#f) ;++ should we initialize it?

(define-constant %%symbol-text-offset 4)

(define-integrable (symbol-print-length sym)
  (fx- (symbol-length sym) %%symbol-text-offset))

(define the-symbols                   ; local
  (vector-fill (make-vector 2039) '()))    ; 2039 is prime


(define (compare-string-to-symbol string symbol)
  (let ((strlen (string-length string))
        (symlen (symbol-length symbol)))
    (if (fx= strlen (fx- symlen %%symbol-text-offset))
        (iterate loop ((i 0) (j %%symbol-text-offset))
          (cond ((fx>= i strlen) '#t)
                ((charN= (string-elt string i) (symbol-elt symbol j))
                 '#f)
                (else
                 (loop (fx+ i 1) (fx+ j 1)))))
        '#f)))

;;; intern, v.t. to confine or impound, esp. during a war.  (webster.)

;;; Is interned really useful?

(define (interned obj)
  (let ((string (check-arg
                 (lambda (obj)
                   (cond ((symbol? obj) (symbol->string obj)) ;++ can't happen
                         ((string? obj) obj)
                         (else nil)))
                 obj
                 'interned)))
    (intern-1 string '#f)))

;;; integrable because only used in one place.

(define-integrable (%make-symbol string hash)
  (let* ((len  (fx+ %%symbol-text-offset (string-length string)))
         (xlen (fx-ashr (fx+ len 3) 2))
         (sym  (make-vector-extend header/symbol len xlen)))
    (iterate loop ((i 0) (j %%symbol-text-offset))
      (cond ((fx< j len)
             (set (symbol-elt sym j) (string-elt string i))
             (loop (fx+ i 1) (fx+ j 1)))
            (else
             (set (symbol-hash sym) hash)
             sym)))))

;++ the fx-rem is slow it should be changed to fx-and
;++ or maybe use rk's table package

(define (intern-1 string create?)
  (let* ((hash   (string-hash string))
         (index (fx-rem hash (vector-length the-symbols)))
         (bucket (vref the-symbols index)))
    (iterate loop ((l bucket))
      (cond ((null? l)
             (if create?
                 (let ((symbol (%make-symbol string hash)))
                   (set (vref the-symbols index)
                        (cons symbol bucket))
                   symbol)
                 '#f))
            ((compare-string-to-symbol string (car l))
             (car l))
            (else (loop (cdr l)))))))

;;; string->symbol uses one (global) symbol table in particular.
                         
(define (string->symbol string)
  (let ((string (enforce string? string)))
    (intern-1 string '#t)))

(define (symbol->string symbol)
  (let* ((symbol (enforce symbol? symbol))
         (len    (symbol-length symbol))
         (string (make-string (fx- len 4)))     ; subtract the hash slot
         (text   (string-text string)))
    (iterate loop ((i %%symbol-text-offset) (j 0))
      (cond ((fx>= i len) string)
            (else
             (set (text-elt text j) (symbol-elt symbol i))
             (loop (fx+ i 1) (fx+ j 1)))))))


;;; Other stuff

(define (increment-generator-count)
  (defer-interrupts
    (set (system-global slink/symbol-generator-count)
         (fx+ (system-global slink/symbol-generator-count) 1))
    (system-global slink/symbol-generator-count)))

;;; Generates a new (not previously interned) symbol using prefix
;;; which must be a string.

(define (generate-symbol prefix)
  (let ((buf (get-buffer)))
    (display prefix buf)  
    (vm-write-char buf #\.)
    (vm-write-fixnum buf (increment-generator-count) 10)
    (let ((str (buffer->string! buf)))
      (cond ((intern-1 str '#f)
             (release-buffer buf)
             (generate-symbol prefix))
            (else
             (let ((val (intern-1 str '#t)))
               (release-buffer buf)
               val))))))

;;; Random utility used by system macros.  Buffers must be available
;;; in order to use this.

(define (concatenate-symbol . things)
  (with-buffers ((buf))
    (do ((z things (cdr z)))
        ((null? z)
         (string->symbol (buffer->string! buf)))
      (display (car z) buf))))

(define (walk-symbols proc)
  (walk-vector (lambda (bucket) (walk proc bucket)) the-symbols))

;;; Symbol printing

(lset *write-symbol* plain-write-symbol)

(define-handler symbol
  (object nil
    ((hash self) (symbol-hash self))
    ((print symbol port)
     (*write-symbol* port symbol))
    ((display symbol port)
     (plain-write-symbol port symbol))))

(lset *translate-constituent-inverse* '#f)

;;; Write a vanilla symbol

(define (plain-write-symbol port symbol)
  (let ((len    (symbol-length symbol))
        (writec (if (iob? port) vm-write-char write-char)))
    (iterate loop ((i %%symbol-text-offset))
      (cond ((fx>= i len) (no-value))
            (else
             (writec port 
                     (if *translate-constituent-inverse*
                         (*translate-constituent-inverse* (symbol-elt symbol i))
                         (symbol-elt symbol i)))
             (loop (fx+ i 1)))))))

;;; Build the symbol table
;++ This should move to boot someday,
(initialize-symbol-table)
