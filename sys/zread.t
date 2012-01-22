(herald zread
  (env tsys
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

;;; Z system reader

(define-constant %%close-paren (cons '**close-paren** '()))
(define-constant %%dot         (cons '**dot** '()))
(define-constant %%escape-character  #\\)

(lset *z-input-radix* 10)

(define (z-read iob)
  (let ((form (z-sub-read iob)))
    (cond ((eq? form %%dot)
           (error "\" . \" in illegal context."))
          ((eq? form %%close-paren)
           (z-read iob))
          (else form))))

(define (z-sub-read iob)
  (let ((c (vm-read-char iob)))
    (cond ((eof? c)   c)
          ((whitespace? c) (z-sub-read iob))
          ((char= c #\()    (z-read-list iob))
          ((char= c #\))    %%close-paren)
          ((char= c #\.)    %%dot)
          ((char= c #\')    (list 'quote (z-sub-read iob)))
          ((char= c #\`)    (list 'quasiquote (z-sub-read iob)))
          ((char= c #\,)    (list (cond ((char= (vm-peek-char iob) #\@)
                                         (vm-read-char iob)
                                         'unquote-splicing)
                                        (else 'unquote))
                                  (z-sub-read iob)))
          ((char= c #\#)    (z-sharpsign-read-macro iob))
          ((char= c #\")    (z-read-string iob))
          ((char= c #\;)    (z-read-comment iob))
          ((char= c #\-)
           (if (digit (vm-peek-char iob) *z-input-radix*)
               (z-read-signed-number iob -1 *z-input-radix*)
               (z-read-symbol iob c)))
          ((char= c #\+)
           (if (digit (vm-peek-char iob) *z-input-radix*)
               (z-read-signed-number iob 1 *z-input-radix*)
               (z-read-symbol iob c)))
          ((digit c *z-input-radix*)
           => (lambda (c)
                (z-read-number iob c *z-input-radix*)))
          (else
           (z-read-symbol iob c)))))


(define (z-sharpsign-read-macro iob)
  (let ((c (vm-read-char iob)))
    (cond ((or (char= c #\o) (char= c #\O))
           (z-read-number iob (vm-read-char iob) 8))
          ((or (char= c #\x) (char= c #\X))
           (z-read-number iob (vm-read-char iob) 16))
          ((or (char= c #\b) (char= c #\B))
           (z-read-number iob (vm-read-char iob) 2))
          ((char= c #\\)
           (vm-read-char iob))
          ((char= c #\.)
           (z-eval (z-sub-read iob) *z-repl-env*))
          ((char= c #\[)
           (let ((key (z-read-symbol iob (vm-read-char iob))))
             (case key
              ((comex)
               (read-comex iob)
               (vm-read-char iob)) ; #\]  ;++ check for errors?
              (else
               (error "unknown syntax #[~a ...]." key)))))
          (else
           (error "unknown sharpsign read macro - ~a~&" c)))))


(define (z-read-list iob)
  (let ((form (z-sub-read iob)))
    (cond ((eof? form) form)
          ((eq? form %%close-paren) '())
          ((eq? form %%dot) (z-read-tail iob))
          (else (cons form (z-read-list iob))))))

(define (z-read-tail iob)
  (let ((last-form (z-sub-read iob)))
    (cond ((eof? last-form)              last-form)
          ((eq? last-form %%close-paren) '())
          ((eq? last-form %%dot)         (z-read-tail iob))
          (else
           (let ((another-form (z-sub-read iob)))
             (cond ((eq? another-form %%close-paren) last-form)
                   ((eq? another-form %%dot) (cons last-form
                                                   (z-read-tail iob)))
                   (else (cons last-form (z-read-list iob)))))))))

(define (z-read-signed-number iob sign radix)
  ;++ loses on %%minimum-fixnum.
  (fx* sign (z-read-number iob (digit (vm-read-char iob) radix) radix)))

(define-recursive (z-read-number iob n radix)
  (let* ((c (vm-peek-char iob))
         (v (digit c radix)))
    (cond (v
           ;; accept the character
           (vm-read-char iob)
           (z-read-number iob (fx+ v (fx* n radix)) radix))
          ;; eat trailing dots on numbers
          ((and (char= c #\.)
                (vm-read-char iob)
                (not (whitespace? (vm-peek-char iob))))
           (error "floating point numbers are not implemented in the Z system."))
          (else n))))

(define (z-read-string iob)
  (iterate loop ((l '()))
    (let ((c (vm-read-char iob)))
      (cond ((eof? c)
             (error "end of file within a string."))
            ((char= c %%escape-character)
             (loop (cons (z-read-escaped-char iob) l)))
            ((char= c #\") (backwards-list->string l))
            (else (loop (cons c l)))))))

(define (z-read-escaped-char iob)
  (let ((c (vm-read-char iob)))
    (cond ((or (char= c #\\) (char= c #\")) c)
          (else
           (error "invalid escaped character - ~a~&" c)))))

(define (z-read-symbol iob c)
  (iterate loop ((l (cons (char-upcase c) '())))
    (cond ((z-break-char? (vm-peek-char iob))
           (string->symbol (backwards-list->string l)))
          (else
           (loop (cons (char-upcase (vm-read-char iob)) l))))))

(define (backwards-list->string l)
    (let* ((len (length l))
           (str (make-string len))
           (text (string-text str)))
        (do ((i (fx- len 1) (fx- i 1))
             (l l (cdr l)))
            ((fx< i 0) str)
          (set (text-elt text i) (car l)))))


(define (z-read-comment iob)
  (let ((c (vm-read-char iob)))
    (cond ((eof? c) c)                     ; no test with conditions
          ((char= c #\newline) (z-sub-read iob))
          (else (z-read-comment iob)))))

(define (z-break-char? c)
  (or (char= c #\()
      (char= c #\))
    ;  (char= c #\.)  ;++ symbols can have dots
      (char= c #\;)
      (whitespace? c)))

(comment
    ;++ flush this stuff
    ;;; random auxiliaries (actually duplicates of character and string
    ;;; code!)

    (define-constant %%alpha     (char->ascii #\a))
    (define-constant %%cap-alpha (char->ascii #\A))
    (define-constant %%zed       (char->ascii #\z))
    (define-constant %%zero      (char->ascii #\0))
    (define-constant %%nine      (char->ascii #\9))

    (define (z-whitespace? c)
      (cond ((or (char= c #\space)
                 (char= c #\tab)
                 (char= c #\newline))
             t)
             (else nil)))

    (define (z-char-upcase c)
      (let ((c (char->ascii c)))
        (ascii->char (cond ((and (fx>= c %%alpha)
                                 (fx<= c %%zed))
                            (fx- c (fx- %%alpha %%cap-alpha)))
                           (else c)))))

    (define (z-digit c radix)
      (let ((c (char->ascii (z-char-upcase c))))
        (cond ((and (fx>= c %%zero)
                    (fx<= c (fixnum-min (fx+ radix %%zero) (fx+ %%nine 1))))
               (fx- c %%zero))
              ((and (fx> radix 10)
                    (fx>= c %%cap-alpha)
                    (fx< c (fx+ %%cap-alpha (fx- radix 10))))
               (fx+ (fx- c %%cap-alpha) 10))
              (else nil))))

 )
