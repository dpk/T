(herald string (env tsys))

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

(lset *string-delimiter* #\doublequote)

(define (make-string size)
  (let ((size (enforce nonnegative-fixnum? size)))
    (let ((string (make-vector-extend header/slice size 2)))
      (set (string-text string) (make-text size))
      string)))

;;; string header manipulation

(define (chopy string)
  (let ((string (enforce string? string)))
    (let ((new (%make-extend (extend-header string) %%slice-size)))
      (set (string-text new) (string-text string))
      (set (mref-integer new 4) (mref-integer string 4))
      new)))

(define (chopy! dest source)
  (let ((dest (enforce string? dest))
        (source (enforce string? source)))
    (set (extend-header dest) (extend-header source))
    (set (string-text dest) (string-text source))
    (set (mref-integer dest 4) (mref-integer source 4))
    dest))

(define (string-replace dest source count)
  (let ((dest (enforce string? dest))
        (source (enforce string? source))
        (count (enforce nonnegative-fixnum? count)))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i count) dest)
      (set (nthchar dest i) (nthchar source i)))))

(define (copy-string string)
  (let ((string (enforce string? string)))
    (let ((len (string-length string)))
      (string-replace (make-string len) string len))))

(define (string-equal? s1 s2)
  (let ((s1 (enforce string? s1))
        (s2 (enforce string? s2)))
    (%string-equal? s1 s2)))
                  
(define (%string-equal? s1 s2)
  (and (eq? (extend-header s1) (extend-header s2))
       (let ((len (string-length s1))
             (s1-text (string-text s1))
             (s2-text (string-text s2)))
         (iterate loop ((i 0) 
                        (s1-i (mref-integer s1 4)) 
                        (s2-i (mref-integer s2 4)))
           (cond ((fx>= i len) t)
                 ((char= (text-elt s1-text s1-i) (text-elt s2-text s2-i)) 
                  (loop (fx+ i 1) (fx+ s1-i 1) (fx+ s2-i 1)))
                 (else nil))))))


                  

(define (list->string l)
  (let ((l (enforce list? l)))
    (let ((len (length l)))
      (let ((str (make-string len)))
        (do ((i 0 (fx+ i 1))
             (l l (cdr l)))
            ((fx= i len) str)
          (set (nthchar str i) (car l)))))))

(define (string->list s)
  (let ((s (enforce string? s)))
    (do ((i (fx- (string-length s) 1) (fx- i 1))
         (l '() (cons (nthchar s i) l)))
        ((fx< i 0) l))))

(define (string-append . strings)
  (do ((l strings (cdr l))
       (n 0 (fx+ n (string-length (enforce string? (car l))))))
      ((null? l)
       (let ((newstring (make-string n)))
         (do ((l strings (cdr l))
              (n (chopy newstring) (nthchdr! n (string-length (car l)))))
             ((null? l) newstring)
           (string-replace n (car l) (string-length (car l))))))))

(define (string-slice string start count)
  (let ((string (enforce string? string))
        (start (enforce nonnegative-fixnum? start))
        (count (enforce nonnegative-fixnum? count)))
    (let ((new-string (nthchdr string start)))
      (cond ((fx>= (string-length new-string) count)
             (set (string-length new-string) count)
             new-string)
            (else
             (error "inconsistent arguments~
                   ~%  (~s ~s ~s ~s)"
                    'string-slice string start count))))))

(define (substring string start count)
  (let ((string (enforce string? string))
        (start (enforce nonnegative-fixnum? start))
        (count (enforce nonnegative-fixnum? count)))
    (let ((new (make-string count)))
      (do ((i start (fx+ i 1))
           (j 0 (fx+ j 1)))
          ((fx= j count) new)
        (set (nthchar new j) (nthchar string i))))))
        
;;; mappers.

(define (walk-string fn string)         ; cf. walk-vector
  (let ((string (enforce string? string)))
    (let ((limit (fx- (string-length string) 1)))
      (cond ((fx>= limit 0)
             (iterate loop ((i 0))
               (cond ((fx>= i limit) 
                      (fn (nthchar string i)))
                     (else
                      (fn (nthchar string i))
                      (loop (fx+ i 1))))))))))

(define (map-string proc string)
  (let ((string (enforce string? string)))
    (let ((len (string-length string)))
      (let ((new-string (make-string len)))
        (do ((i 0 (fx+ i 1)))           ; avoid chonsing
            ((fx>= i len) new-string)
          (set (nthchar new-string i) (proc (nthchar string i))))))))

(define (map-string! fn string)
  (let ((string (enforce string? string)))
    (let ((len (string-length string)))
      (do ((i 0 (fx+ i 1)))
          ((fx>= i len) string)
        (set (nthchar string i) (fn (nthchar string i)))))))

;;; case stuff

(define (string-upcase string)
  (map-string %char-upcase string))

(define (string-downcase string)
  (map-string %char-downcase string))

(define (string-invert-case string)
  (map-string %char-invert-case string))

(define (string-upcase! string)
  (map-string! %char-upcase string))

(define (string-downcase! string)
  (map-string! %char-downcase string))

(define (string-invert-case! string)
  (map-string! %char-invert-case string))

(define (string-fill string ch)
  (let ((string (enforce string? string))
        (ch (enforce char? ch)))
    (let ((size (string-length string)))
      (do ((i 0 (fx+ i 1)))
          ((fx>= i size) string)
        (set (nthchar string i) ch)))))

(define (char->string ch)           
  (let ((ch (enforce char? ch)))
    (let ((s (make-string 1)))
      (set (nthchar s 0) ch)
      s)))

(define (string-find-char string ch)
  (let* ((string (enforce string? string))
         (ch (enforce char? ch))
         (len (string-length string)))
      (iterate loop ((i 0))
        (cond ((fx>= i len) nil)
              ((char= (nthchar string i) ch) i)
              (else (loop (fx+ i 1)))))))

(define (string-posq ch string) (string-find-char string ch))

(define (string-reverse-find-char string ch)
  (let ((string (enforce string? string))
        (ch (enforce char? ch)))
    (iterate loop ((i (string-length string)))
      (let ((i (fx- i 1)))
        (cond ((fx< i 0) nil)
              ((char= (nthchar string i) ch) i)
              (else (loop i)))))))

(define-simple-switch text-elision fixnum? 20)

(define-handler text
  (let ((writer (lambda (port text count)
                  (let ((len (cond ((null? count) (text-length text))
                                   ((fx< count (text-length text))
                                    count)
                                   (else (text-length text))))
                        (writec (if (iob? port) vm-write-char write-char)))
                    (if (fixnum? count)
                        (if (fx< len (text-length text))
                            (format port "#{Text (~a) " (object-hash text))
                            (format port "#[Text (~a) \"" (object-hash text))))
                    (do ((i 0 (fx+ i 1)))
                        ((fx>= i len) (no-value))
                      (writec port (text-elt text i)))
                    (if (fixnum? count)
                        (if (fx< len (text-length text))
                            (write-string port " ... }")
                            (write-string port "\"]")))))))
    (object nil
      ((display self port)
       (writer port self nil))
      ((print self port)
       (writer port self (text-elision)))
      ((crawl-exhibit self)
       (writer (standard-output) self (text-length self))))))

(define-handler slice
  (object nil
    ((hash self) (string-hash self))
    ((display obj port) (write-string port obj))
    ((print obj port)
     (print-delimited-string obj port *string-delimiter*))
    ((crawl-exhibit string)
     (format (terminal-output) 
             " header: addr = #x~x, length = ~d, offset = ~d~%"
             (descriptor->fixnum string)
             (string-length string)
             (mref-integer string 4))
     (format (terminal-output) " text: addr = #x~x, length = ~d,~% '~a'~%"
             (descriptor->fixnum (string-text string))
             (text-length (string-text string))
             (string-text string)))))
     
;;; We should pre-scan the string to decide whether it can be
;;; blatted out with a single write-string.
;++ We should be handling control characters in strings.

(define (print-delimited-string obj port delim)
  (let ((port  (enforce port? port))
        (delim (enforce char? delim)))
    (cond ((not (reasonable? obj))    ; robustness implies hair.  sorry.
           (print-random obj port))
          (else
           (let ((writec (if (iob? port) vm-write-char write-char))
                 (len    (string-length obj)))
             (writec port delim)
             (iterate loop ((i 0))
               (cond  ((fx>= i len)
                       (writec port delim)
                       (no-value))
                      (else
                       (let ((ch (string-elt obj i)))
                         (cond ((char= ch #\newline)
                                (newline port))
                               ((%control? ch)
                                (writec port *escape-char*)
                                (writec port *control-char-delimiter*)
                                (writec port ch))
                               (else
                                (if (or (char= ch delim)
                                        (char= ch *escape-char*))
                                    (writec port *escape-char*))
                                (writec port ch))))
                       (loop (fx+ i 1))))))))))
