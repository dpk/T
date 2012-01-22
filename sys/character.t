(herald  character (env tsys))

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

;;;; Character-related routines

;++ What should be integrated?

;;; Very gross names. Think of better ones.  Figure out what to
;;; do about integrability.  These shouldn't be integrable if
;;; CHAR-UPCASE isn't integrable.

;;; released procedures
;;; char<,char=,char>,charN=,char<=,char>= are primops

(lset *escape-char*            #\backslash)
(lset *control-char-delimiter* #\^)
(lset *dispatch-char*          #\#)

(define-integrable (lowercase? c)
  (and (char>= c #\a) (char<= c #\z)))

(define-integrable (uppercase? c)
  (and (char>= c #\A) (char<= c #\Z)))

(define-integrable (%char-upcase c)
  (if (lowercase? c)
      (ascii->char (fx- (char->ascii c) #o40))
      c))

(define (char-upcase c)
  (%char-upcase (enforce char? c)))

(define-integrable (%char-downcase c)
  (if (uppercase? c)
      (ascii->char (fx+ (char->ascii c) #o40))
      c))

(define (char-downcase c)
  (%char-downcase (enforce char? c)))

(define-integrable (%char-invert-case c)
  (cond ((uppercase? c)
         (ascii->char (fx+ (char->ascii c) #o40)))
        ((lowercase? c)
         (ascii->char (fx- (char->ascii c) #o40)))
        (else
         c)))

(define (char-invert-case c)
  (%char-invert-case (enforce char? c)))

;;; case insensitive versions of character relations

(define (char=ic c1 c2)
  (char= (char-upcase c1) (char-upcase c2)))

(define (char<ic c1 c2)
  (char< (char-upcase c1) (char-upcase c2)))

(define (char>ic c1 c2)
  (char> (char-upcase c1) (char-upcase c2)))

(define (charn=ic c1 c2)
  (charn= (char-upcase c1) (char-upcase c2)))

(define (char>=ic c1 c2)
  (char>= (char-upcase c1) (char-upcase c2)))

(define (char<=ic c1 c2)
  (char<= (char-upcase c1) (char-upcase c2)))

;;; character classes, these are all predicates

(define-integrable (%alphabetic? c)
  (or (lowercase? c) (uppercase? c)))

(define (alphabetic? c)
  (%alphabetic? (enforce char? c)))

(define-integrable (%graphic? c)
  (and (char>= c #\space) (char< c #\rubout)))

(define (graphic? c)
  (%graphic? (enforce char? c)))

(define-integrable (%control? c)
  (or (char< c #\space) (char= c #\rubout)))

(define (control? c)
  (%control? (enforce char? c)))

(define-integrable (%numeric? c radix)
  (true? (%digit c radix)))

(define (numeric? c radix)
  (%numeric? c radix))

(define-integrable (%alphanumeric? c)
  (or (%alphabetic? c) (%digit? c 10)))

(define (alphanumeric? c)
  (%alphanumeric? (enforce char? c)))

(define (whitespace? c)
;++ Make this should be using a breakmask?
  (or (char= #\space    c)
      (char= #\newline  c)
      (char= #\return   c)
      (char= #\tab      c)
      (char= #\form     c)
      (char= #\linefeed c)))

;;; random utilities

(define-integrable (%controlify c)
  (ascii->char (fixnum-logand (char->ascii c) #o37)))

(define (controlify c)
  (%controlify (enforce char? c)))

(define-integrable (%uncontrolify c)
  (ascii->char (fx+ (char->ascii c) #o100)))

(define (uncontrolify c)
  (%uncontrolify (enforce char? c)))

(define-integrable (acceptable-radix? radix)
  (and (fixnum? radix) (fx> radix 0) (fx<= radix 36)))

(define (digit c radix)
  (%digit (enforce char? c)
          (enforce acceptable-radix? radix)))

;++ wouldn't digit be fast enough?
(define (%digit c radix)                ; for entry from reader.
  (cond ((fx<= radix 10)
         (cond ((and (char< c (ascii->char (fx+ (char->ascii #\0)
                                                radix)))
                     (char>= c #\0))
                (fx- (char->ascii c) (char->ascii #\0)))
               (else nil)))
        ((and (char<= c #\9)
              (char>= c #\0))
         (fx- (char->ascii c) (char->ascii #\0)))
        (else
         (let ((cc (%char-upcase c)))
           (cond ((and (char>= cc #\A)
                       (char< cc (ascii->char (fx+ (char->ascii #\A)
                                                   (fx- radix 10)))))
                  (fx+ 10 (fx- (char->ascii cc) (char->ascii #\A))))
                 (else nil))))))

(define %digit? %digit)

(define (digit? c radix) (true? (digit c radix)))

(define (char->digit c radix)
  (or (digit c radix)
      (char->digit (error "argument isn't a digit in given radix.~%  ~s"
                          `(char->digit ,c ,radix))
                   radix)))

(define %char->digit %digit)

;;; Common Lisp calls this DIGIT-CHAR.

(define (digit->char n radix)
  (let ((n     (enforce nonnegative-fixnum? n))
        (radix (enforce acceptable-radix?   radix)))
    (cond ((fx> n radix)
           (error "argument doesn't correspond to a digit.~%  ~s"
                  `(digit->char ,n ,radix)))
          ((fx< n 10)
           (ascii->char (fx+ (char->ascii #\0) n)))
          (else
           (ascii->char (fx+ (char->ascii #\A) (fx- n 10)))))))

;;; This looks circular.  It is.

(define *symbolic-character-table*
  '(
    ;; System dependent options

    (newline    . #\newline)

    ;; Distinguished ASCII codes on any system

    (null       . #\null)
    (bell       . #\bell)
    (backspace  . #\backspace)
    (tab        . #\tab)
    (linefeed   . #\linefeed)
    (form       . #\form)
    (formfeed   . #\form)
    (return     . #\return)
    (escape     . #\alt)
    (altmode    . #\alt)
    (alt        . #\alt)
    (space      . #\space)
    (rubout     . #\rubout)

    (left-paren    . #\()
    (right-paren   . #\))
    (star          . #\*)
    (plus-sign     . #\+)
    (minus-sign    . #\-)
    (left-bracket  . #\[)
    (right-bracket . #\])
    (left-brace    . #\{)
    (right-brace   . #\})
    (left-angle    . #\<)
    (right-angle   . #\>)
    (slash         . #\/)
    (backslash     . #\\)
    (quote         . #\')
    (backquote     . #\`)
    (doublequote   . #\")
    (comma         . #\,)
    (dot           . #\.)
    (semicolon     . #\;)
    
    ))

(define (char-name ch)
  (car (rassq ch *symbolic-character-table*)))

(define (name-char symbol)
  (cdr (assq symbol *symbolic-character-table*)))

;;; The character handler

(define-handler char
  (object nil
    ((hash self) (char->ascii self))
    ((display obj port) (write-char port obj))
    ((print obj port)
     (cond ((char= obj #\space)
            (write-string port "#\\space"))
           ((graphic? obj)
            (write-string port "#\\")
            (write-char port obj))
           ((char-name obj)
            => (lambda (name) (format port "#\\~s" name)))
           ((control? obj)
            (write-string port "#^")
            (write-char port (uncontrolify obj)))
           (else
            (format port "#[Ascii~_~d]" (char->ascii obj)))))
    ((crawl-exhibit ch)
     (let ((n (char->ascii ch)))
       (format (terminal-output) " ascii: decimal ~d, hex ~x, octal ~o~%"
               n n n)))))
                        
