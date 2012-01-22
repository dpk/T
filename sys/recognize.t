(herald recognize
  (env tsys (osys readtable)))

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

;;;; recognizers for numbers

;;; This is used by the reader, to decide whether something's a
;;; number or not, and by the printer, to decide whether it needs
;;; to slashify a symbol.

;;; El hacko versions to use until continuation-passing is really
;;; cheap, or until we have a good regular-expression package, or
;;; whatever.  this is not intended to be clean or final in any
;;; sense.

;;; what about recognizing 15./29. for ratios?  hmm.  later.

;;; main entry point:

(define (recognize-atom s rt)                ; ad hac
  (let ((c (char s))
        (radix (rt-radix rt)))
    (or (cond ((char= c dot-char)
               (cond ((fx= (string-length s) 1) parses-as-dot)
                     (else (recognize-fraction (chdr s)))))
              ((%digit? c (if (fx< radix 10) 10 radix))
               (recognize-number (chopy s) rt))
              ((fx= (string-length s) 1) nil)   ; + and - aren't numbers
              ((sign-char? c)
               (recognize-signed-number (chdr s) rt))
              (else nil))
        parses-as-symbol)))

(define (recognize-signed-number s rt)
  ;; sign has been gobbled.  dispatch on next character.
  (let ((c (char s)))
    (cond ((char= c dot-char)
           (recognize-fraction (chdr s)))
          ((%digit? c (let ((radix (rt-radix rt)))
                        (if (fx< radix 10) 10 radix)))
           (recognize-number s rt))
          (else nil))))

(define (recognize-number s rt)
  ;; determine radix: if there are e's or dots, then base 10
  ;; else given radix.
  (let ((radix (cond ((or (string-posq #\. s)
                          (and (fx< (rt-radix rt) 15)
                               (or (string-posq #\E s)
                                   (string-posq #\e s))))
                      10)
                     (else (rt-radix rt)))))
    ;; scan over initial digits.
    (iterate loop ()
      (cond ((string-empty? s) parses-as-integer)
            (else
             (let ((c (char s)))
               (chdr! s)
               (cond ((%digit? c radix)
                      (loop))
                     ((char= c #\.)
                      (cond ((string-empty? s)
                             parses-as-decimal-integer)
                            (else
                             (recognize-optional-fraction s))))
                     ((exponent-introducer? c)
                      (recognize-float-exponent s))
                     ((char= c ratio-char)
                      (if (recognize-integer s radix)
                          parses-as-ratio nil))
                     (else nil))))))))

(define (recognize-fraction s)
  ;; dot has already been gobbled.  digits must follow.
  (cond ((%digit? (char s) 10)
         (recognize-optional-fraction s))
        (else nil)))

(define (recognize-optional-fraction s)
  (iterate loop ()
    (cond ((string-empty? s) parses-as-float)
          (else
           (let ((c (char s)))
             (chdr! s)
             (cond ((%digit? c 10) (loop))
                   ((exponent-introducer? c) (recognize-float-exponent s))
                   (else nil)))))))

(define (recognize-float-exponent s)
  ;; e has already been gobbled.
  (cond ((string-empty? s) parses-as-float)
        (else (if (sign-char? (char s)) (chdr! s))
              (if (recognize-integer s 10) parses-as-float nil))))

(define (recognize-integer s radix)
  (cond ((string-empty? s) nil)
        (else
         (iterate loop ()
           (let ((c (char s)))
             (chdr! s)
             (cond ((not (%digit? c radix)) nil)
                   ((string-empty? s) parses-as-integer)
                   (else (loop))))))))

(define parses-as-dot
  (lambda (s rt) (ignore s rt) dot-token))

(define parses-as-symbol
  (lambda (s rt)
    ((rt-string->symbol rt) s)))

(define parses-as-integer
  (lambda (s rt)
    (string->integer s (rt-radix rt))))      ; defined in bignum module

(define parses-as-decimal-integer
  (lambda (s rt)
    (ignore rt)
    (let ((s (chopy s)))
      (set (string-length s) (fx- (string-length s) 1))
      (string->integer s 10))))

(define parses-as-float
  (lambda (s rt)
    (ignore rt)
    (string->flonum s)))

(define parses-as-ratio
  (lambda (s rt)
    (let ((s1 (chopy s))
          (q (string-posq ratio-char s))
          (radix (rt-radix rt)))
      (set (string-length s1) q)
      (ratio (string->integer s1                    radix)
             (string->integer (nthchdr s (fx+ q 1)) radix)))))
