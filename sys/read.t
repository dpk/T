(herald read
  (env tsys (osys buffer) (osys readtable)))


;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, K Pitman, J Rees.
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

;;;; external representation parser

;;; the incredible reader.  it is here that the language's external syntax
;;; (such as it is) is definitively defined.

;;; ---------- internal markers and tokens

(define (make-token id)                 
  (object nil
          ((print-type-string self) "Token")
          ((identification self) id)))

;;; class of markers for right brackets

(define close-token-marker (make-token 'close-token-marker))

(define-integrable (make-close-token)
  (cons close-token-marker nil))

(define-integrable (close-token? x)
  (and (pair? x) (eq? (car x) close-token-marker)))

;;; token for dot syntax

(define dot-token (make-token 'dot-token))

(define-integrable (dot-token? x)
  (eq? x dot-token))

;;; nothing-read is the mechanism whereby readmacros can choose to not
;;; return anything.

(define nothing-read (make-token 'nothing-read))

(define-integrable (nothing-read? x)
  (eq? x nothing-read))

;;; ---------- top-level entries into reader

;;; some of the following procedure names can be explained by
;;; imagining that an "object" is either an object read in the normal
;;; way or an eof token, and that a "thing" might include some strange
;;; internal reader token, like a dot or a close-bracket.

;;; read-object is the default method for the read operation.

(define (read-object port read-table)
  (read-object-1 port read-table nil))

;;; recursive entry from readmacros.  this is guaranteed to return a useable
;;; value, no funny tokens of any sort.

(define (read-refusing-eof port)
  (read-object-refusing-eof port (port-read-table port)))

(define (read-object-refusing-eof port rt)
  (read-object-1 port rt t))

(define (read-object-1 port rt refuse-eofs?)
  (iterate loop ()                      ; throw away bogus close tokens.
    (let ((obj (read-thing-refusing-dots port rt)))
      (cond ((eof? obj)
             ;; eof's might or might not be returnable.
             (cond (refuse-eofs?
                    (read-error port "unexpected end-of-file"))
                   (else obj)))
            ((close-token? obj)
             ;; right parentheses might or might not be ignorable.
             (cond ((and (not refuse-eofs?) (interactive? port))
                    (loop))
                   (else
                    (unread-char port)    ; incredible hack.
                    (read-error port "unexpected \"~c\""
                                (read-char port)))))
            (else
             ;; object is neither eof nor close.  return it.
             obj)))))

;;; this is called from the list reader and from the two above routines.

(define (read-thing-refusing-dots port rt)
  (let ((obj (read-thing port rt)))
    (cond ((dot-token? obj)
           (read-error port "\" . \" in illegal context"))
          (else obj))))

;;; ---------- main dispatch for reader

;;; this is the place where the scanning and dispatching actually happens.

(define (read-thing port rt)
  (iterate loop ()
    (let ((ch (read-char port)))
      (cond ((eof? ch) ch)
            (else
             (let ((syn (char-syntax rt ch)))
               (cond ((read-macro? syn)
                      (let ((obj (syn port ch rt)))
                        (if (nothing-read? obj) (loop) obj)))
                     (else
                      (select syn
                              ((%%whitespace %%ignored) (loop))
                              (else (read-atom port rt ch)))))))))))

;;; not readmacro, not whitespace: just a vanilla symbol or number.

(define (read-atom port rt ch)
  (with-buffers ((buf))                 ; ought to bind this somehow
    (iterate loop ((ch ch) (slashes? nil))
      ((lambda (accum done)
         (cond ((eof? ch) (done))
               (else
                (let ((syn (char-syntax rt ch)))
                  (cond ((not (read-macro? syn))
                         (select syn
                           ((%%constituent)
                            (accum ((rt-translator rt) ch)
                                   slashes?))
                           ((%%whitespace)
                            (unread-char port)    ; put back the delimiter
                            (done))
                           ((%%escape-char)
                            (let ((ch (read-char port)))
                              (if (eof? ch)
                                  (read-error port
                                              ;; elaborate on this...
                                              "eof follows escape char")
                                  (accum ch t))))
                           ((%%undefined)
                            (illegal-char-encountered port ch))
                           (else
                            (loop (read-char port) slashes?))))
                        ((not (delimiting-read-macro? syn))
                         (accum ((rt-translator rt) ch)
                                slashes?))
                        (else 
                         (unread-char port)    ; put back the delimiter
                         (done)))))))
       (lambda (ch slashes?)            ; accum
         (vm-write-char buf ch)
         (loop (read-char port) slashes?))
       (lambda ()                       ; done
         (let ((str (buffer->string buf)))
           (cond (slashes?
                  ((rt-string->symbol rt) str))
                 (else
                  (((rt-recognizer rt) str rt) str rt)))))))))

(define (illegal-char-encountered port ch)
  (read-error port "illegal character ~s" ch))

;;; create the standard read table.

(define standard-read-table
  (make-read-table vanilla-read-table 'standard-read-table))

(set *print-table* standard-read-table)

(set-read-table-entry standard-read-table #\backslash %%escape-char)

(walk (lambda (ch)
        (set-read-table-entry standard-read-table ch %%undefined))
      '(#\left-brace #\right-brace #\left-bracket #\right-bracket))

;;; ---------- standard read macros

(define (make-list-reader)
  (let* ((token (make-close-token))
         (right
          (object (lambda (port ch rt)
                    (ignore port ch rt)
                    token)
                  ((delimiting-read-macro? self) t)
                  ((establish-read-table-entry self ch)
                   (if (and (eq? self
                                 (read-table-entry standard-read-table
                                                   #\right-paren))
                            (null? *list-end-char*))
                       (set *list-end-char* ch)))
                  ((print-type-string self) "List-terminator"))))
    (object (lambda (port ch rt)
              (ignore ch)
              (read-delimited-list port token rt))
            ((delimiting-read-macro? self) t)
            ((establish-read-table-entry self ch)
             (if (and (eq? self
                           (read-table-entry standard-read-table
                                             #\left-paren))
                      (null? *list-begin-char*))
                 (set *list-begin-char* ch)))
            ((list-terminator self) right)
            ((print-type-string self) "List-reader"))))

(define-operation (list-terminator syn))

(let ((reader (make-list-reader)))
  (set-read-table-entry standard-read-table
                        #\left-paren
                        reader)
  (set-read-table-entry standard-read-table
                        #\right-paren
                        (list-terminator reader)))

;;; Used when something ends in wrong kind of token.  this is the
;;; place to implement super-brackets, if we ever decide that we
;;; want them.

(define (losing-right-bracket port)
  (read-error port "right bracket doesn't match left bracket")
  nothing-read)

(define (losing-eof port)
  (read-error port "end of file inside list (missing right bracket)")
  nothing-read)

(define (read-delimited-list port token rt)
  (iterate loop ((l '()))
    (let ((obj (read-thing port rt)))
      (cond ((eof? obj) (losing-eof port))
            ((close-token? obj)
             ;; list ends with right paren
             (cond ((neq? obj token)
                    (losing-right-bracket port))
                   (else (reverse! l))))
            ((dot-token? obj)
             (let ((tail (read-object-refusing-eof port rt)))
               (let ((z (read-thing-refusing-dots port rt))) ;expect close.
                 (cond ((eof? z) (losing-eof port))
                       ((eq? z token)
                        ;; list ends <.> <frob> <rparen>
                        (append-reverse! l tail))
                       ((close-token? z)
                        (losing-right-bracket port))
                       (else
                        (read-error port "two objects follow dot in list"))
                       ))))
            (else (loop (cons obj l)))))))

(define read-delimited-string
  (object (lambda (port ch rt)
            (with-buffers ((buffer))
              (read-delimited-string-into-buffer port ch rt buffer)
              (buffer->string buffer)))
          ((establish-read-table-entry self ch)
           (if (null? *string-delimiter*)
               (set *string-delimiter* ch)))))

(define (read-delimited-string-into-buffer port delimiter rt buffer)
  (labels (((error)
            (read-error port
                        "end of file within ~c...~c (missing delimiter)"
                        delimiter
                        delimiter)
            buffer)
           ((read-escaped-char)
            (let ((ch (read-char port)))
              (cond ((eof? ch) (error))
                    ((char= ch *control-char-delimiter*)
                     (let ((ch (read-char port)))
                       (if (eof? ch) (error) (%controlify ch))))
                    ((char= ch #\left-bracket)
                     (read-keyworded port ch 0 rt))
                    (else ch))))
          ((loop)
           (let ((ch (read-char port)))
             (cond ((eof? ch) (error))
                   ((char= ch delimiter) buffer)
                   (else
                    (vm-write-char
                     buffer
                     (if (eq? (char-syntax rt ch) %%escape-char)
                         (read-escaped-char)
                         ch))
                    (loop))))))
    (loop)))

(set-read-table-entry standard-read-table #\doublequote read-delimited-string)

(define read-delimited-symbol
  (object (lambda (port ch rt)
            (with-buffers ((buffer))
              (read-delimited-string-into-buffer port ch rt buffer)
              (string->symbol (buffer->string! buffer))))
          ((establish-read-table-entry self ch)
           (if (null? *symbol-delimiter*) (set *symbol-delimiter* ch)))))

;(set-read-table-entry standard-read-table #\| read-delimited-symbol)

(define read-comment
  (object (lambda (port ch rt)
            (ignore ch rt)
            (let ((obj (read-line port)))
              (if (eof? obj) obj nothing-read)))
    ((delimiting-read-macro? self) t)))

(set-read-table-entry standard-read-table #\semicolon read-comment)

(define (read-quotation port ch rt)
  (ignore ch)
  (list 'quote (read-object-refusing-eof port rt)))

(set-read-table-entry standard-read-table #\quote read-quotation)

(define (read-backquote port ch rt)
  (ignore ch)
  (list 'quasiquote (read-object-refusing-eof port rt)))

(set-read-table-entry standard-read-table #\backquote read-backquote)

(define (read-comma port ch rt)
  (ignore ch)
  (list (cond ((char= (peek-char port) #\@)
               (read-char port)
               'unquote-splicing)
              (else 'unquote))
        (read-object-refusing-eof port rt)))

(set-read-table-entry standard-read-table #\comma read-comma)

;;; ---------- sharpsign

(define (make-dispatch-read-macro)
  (make-dispatch-read-macro-1 (vector-fill (make-vector number-of-char-codes)
                                           nil)))

(define (make-dispatch-read-macro-1 dispatch-table)
  (object (lambda (port ch rt)
            (let ((nextch (read-char port)))
              ;; should read a number here, for #nrfoo.
              (let ((fn (vref dispatch-table (char->ascii nextch))))
                (cond (fn (fn port nextch nil rt))
                      (else (read-error port
                                        "\"~c\" is an unknown ~c dispatch"
                                        nextch ch))))))
          ((dispatch-syntax self ch)
           (vref dispatch-table (char->ascii ch)))
          ((set-dispatch-syntax self ch fn)
           (cond ((lowercase? ch)
                  (set (vref dispatch-table
                             (char->ascii (char-upcase ch)))
                        fn))
                 ((uppercase? ch)
                  (set (vref dispatch-table
                             (char->ascii (char-downcase ch)))
                        fn))
                 ((digit? ch 10.)
                  (error "can't set a digit's dispatch-macro syntax")))
           (set (vref dispatch-table (char->ascii ch)) fn))
          ((establish-read-table-entry self ch)
           (if (null? *dispatch-char*) (set *dispatch-char* ch)))
          ((copy-read-table-entry self)
           (make-dispatch-read-macro-1 (copy-vector dispatch-table)))
          ((dispatcher? self) t)))

(define-settable-operation (dispatch-syntax table ch))
(define set-dispatch-syntax (setter dispatch-syntax))
(define-operation (copy-dispatcher table))
(define-predicate dispatcher?)

(define read-dispatch (make-dispatch-read-macro))

(set-read-table-entry standard-read-table #\# read-dispatch)

;;; #\c, #\foo - funny character reader

(define (read-character port ch n rt)
  (ignore n)
  (let ((q (peek-char port)))
    (cond ((alphabetic? q)
           (let ((probe (read-object port rt)))
             (cond ((not (symbol? probe))
                    (read-error port
                                "utter randomness in read-character - read ~s"
                                probe))
                   ((fx= (symbol-print-length probe) 1) q)
                   ((name-char probe))
                   (else (read-error port "#~c~s: unknown #~c form"
                                     ch probe ch)))))
          (else (read-char port)))))

(set-dispatch-syntax read-dispatch #\\ read-character)

;;; #t, #f - true and false

(set-dispatch-syntax read-dispatch #\t true)
(set-dispatch-syntax read-dispatch #\f false)

;;; canonical true and false

;++ flush READ-DELIMITED-CONSTANT  and associated #!...
(define (read-delimited-constant port ch n rt)
  (ignore ch)
  (ignore n)
  (let* ((ch (readc port))
         (token (read-atom port rt ch)))
    (case token
      ((true)           '#t)
      ((false)          '#f)
      ((null)           '())
      ((quasiquote)     'quasiquote)
      ((unquote)        'unquote)
      ((unquote-splice) 'unquote-splicing)
      (else
       (read-error port "#!~s: unknown constant" token)))))

(set-dispatch-syntax read-dispatch #\! read-delimited-constant)

;;; #b nnn, #o nnn, #x nnn - alternate radices

(define (make-radical-reader radix)
  (lambda (port ch n rt)
    (ignore ch n)
    (read-object-refusing-eof port (rt-with-radix rt radix))))

(set-dispatch-syntax read-dispatch #\b (make-radical-reader 2))

(set-dispatch-syntax read-dispatch #\o (make-radical-reader 8))

(set-dispatch-syntax read-dispatch #\x (make-radical-reader 16))

(set-dispatch-syntax read-dispatch #\r
                     (lambda (port ch n rt)
                       (ignore ch)
                       (read-object-refusing-eof port
                                                 (rt-with-radix rt n))))

;;; #^x - control character

(set-dispatch-syntax read-dispatch *control-char-delimiter*
                     (lambda (port ch n rt)
                       (ignore ch n rt)
                       (controlify (read-char port))))

;;; #(a b c ...) - vector syntax
;;; extremely kludgey definition

(define (read-vector port ch n rt)
  (ignore n)
  (list->vector ((read-table-entry standard-read-table #\left-paren)
                 port ch rt)))

(set-dispatch-syntax read-dispatch #\left-paren read-vector)

;;; #[keyword stuff ...] - general rereadable object.
;;; The entries in the READ-KEY-WORD table are procedures
;;; of 3 arguments, (lambda (key port read-table) ...) 
;;; where KEY is the keyword, PORT is the port being
;;; read, and READ-TABLE is the read-table being used to
;;; read from the port.  STUFF in #[key stuff ...] will
;;; not have been read yet.

(define read-keyword-table (make-table 'read-keyword-table))

(set (rt-keyword-table standard-read-table) read-keyword-table)

(define read-to-right-bracket (make-list-reader))

(set-read-table-entry standard-read-table #\right-bracket
                      (list-terminator read-to-right-bracket))

(define (read-keyworded port ch n rt)
  (ignore n ch)
  (let ((key (read-thing port rt)))
    (cond ((table-entry (rt-keyword-table rt) key)
           => (lambda (proc) (proc key port rt)))
          (else
           (read-error port "unknown #[...] syntax: #[~s ...]" key)))))

(set-dispatch-syntax read-dispatch #\[ read-keyworded)

(define (read-ascii key port rt)
  (let ((error (lambda (n)
                 (read-error port "illegal syntax - #[ascii ~s]" n)))
        (l (read-to-right-bracket port #\] rt)))
    (if (not (null? (cdr l))) (error (cdr l)))
    (let ((n (car l)))
      (cond ((and (symbol? n) (name-char n)))
            ((and (fixnum? n)
                  (not (fx< n 0))
                  (not (fx>= n number-of-char-codes))
                  (ascii->char n)))
            (else (error n))))))

(set (table-entry read-keyword-table 'char)  read-ascii)
(set (table-entry read-keyword-table 'ascii) read-ascii)

;;; #[Symbol ...]

(set (table-entry read-keyword-table 'symbol)
      (lambda (key port rt)
        (ignore key) 
        (string->symbol (car (read-to-right-bracket port #\] rt)))))

;;; #[Text ...]

(set (table-entry read-keyword-table 'text)
      (lambda (key port rt) 
        (ignore key) 
        (let ((l (read-to-right-bracket port #\] rt)))   
          (string-text (cadr l)))))

;;; #[Bytev ...]

(set (table-entry read-keyword-table 'bytev) 
     make-bytev-for-read)

;;; #[Filename ...] 

(set (table-entry read-keyword-table 'filename) 
     make-filename-for-read)

;;; #[Syntax ...] and #[Internal-syntax ...]

(define (read-syntax-descriptor key port rt)
  (let* ((l (read-to-right-bracket port #\] rt))
         (sym (car l)))
    (cond ((or (not (null? (cdr l)))
               (not (symbol? sym)))
           (read-error port "illegal syntax - #[syntax ~s]" sym))
          ((xcase key
                  ((syntax)
                   (syntax-table-entry standard-syntax-table sym))
                  ((internal-syntax)    ;insufficient error checking
                   (*value t-implementation-env sym))))
          (else
           (read-error port
                       "not a standard reserved word - #[Syntax ~s]"
                       sym)))))

(set (table-entry read-keyword-table 'syntax)
     read-syntax-descriptor)

(set (table-entry read-keyword-table 'internal-syntax)
     read-syntax-descriptor)

;;; #[Comex ...]

(set (table-entry read-keyword-table 'comex)
     (lambda (key port rt)
       (ignore key rt)
       (read-comex port)))


;;; #.expression - read-time evaluation

(set-dispatch-syntax read-dispatch #\.  ; bletch!  what to do?
                     (lambda (port ch n rt)
                       (ignore ch n)
                       (eval (read-object-refusing-eof port rt)
                             (make-locale standard-env '\#.))))

;;; #|...|# reads as a comment

(define read-inline-comment
  (let ((level 0))
    (lambda (port ch n rt)
      (ignore ch n rt)
      (set level (fx+ level 1))
      (let ((readc (if (iob? port) vm-read-char read-char)))
        (labels (((error)
                  (read-error port "end of file within #|...|# (missing delimiter)"))
                 ((loop)
                  (let ((ch (readc port)))
                    (cond ((eof? ch) (error))
                          ((char= ch #\|)
                           (let ((ch (readc port)))
                             (cond ((eof? ch) (error))
                                   ((char= ch #\#)
                                    (set level (fx- level 1))
                                    (if (fx= level 0) nothing-read (loop)))
                                   (else
                                    (unread-char port)
                                    (loop)))))
                          ((char= ch #\#)
                           (let ((ch (readc port)))
                             (cond ((eof? ch) (error))
                                   ((char= ch #\|)
                                    (set level (fx+ level 1))
                                    (loop))
                                   (else
                                    (unread-char port)
                                    (loop)))))
                          (else (loop))))))
          (loop))))))

(set-dispatch-syntax read-dispatch #\| read-inline-comment)


;;; ## reads as (car (repl-results)).  experimental feature.

(set-dispatch-syntax read-dispatch #\#
                     (always '(car (repl-results))))

(set-immutable standard-read-table)
