(herald runtime (env tsys))

;;; Copyright (c) 1985, 1988 Yale University
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
;;; 4. Yale has made no warranty or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;

;;; Modified by Ashwin Ram, July 1985

;;; Compilation support environment for Scheme picks up integrable
;;; procedure definitions made in this file.

(define-constant (string-set! string n char)
   (set (string-elt string n) char))

(define-constant (set-car! x y)
   (set (car x) y))

(define-constant (set-cdr! x y)
   (set (cdr x) y))

;;; Define Scheme's READ in terms of T's, etc.

(define-local-syntax (define-scheme pat . body)
   (let ((foo (lambda (name val)
                 `(*define scheme-env ',name ,val))))
      (cond ((atom? pat)
             (foo pat (car body)))
            (else
             (foo (car pat)
                  `(named-lambda ,(car pat) ,(cdr pat) . ,body))))))

(define-scheme (head stream) (car stream))
(define-scheme (tail stream) (force (cdr stream)))
(define-scheme (empty-stream? stream) (null? stream))

(define-scheme user-initial-environment scheme-env)

(define-scheme (error . items)
   (apply error
          (apply string-append
                 "~a"
                 (map (always "~%~10t~s") (cdr items)))
          items))

(define-scheme (explode atom)
   (map! (lambda (char) (string->symbol (char->string char)))
         (string->list (symbol->string (enforce symbol? atom)))))

(define-scheme (implode list)
   (string->symbol (list->string (map (compose char symbol->string) list))))

(define-local-syntax (optional r specs . body)
   (cond ((null? specs) `(block ,@body))
         (else
          (let ((spec (car specs))
                (specs (cdr specs))
                (var (generate-symbol 'rest)))
             `(let* ((,var ,r)
                     (,(car spec)
                      (cond ((null? ,var) ,(or (cadr spec) 'nil))
                            (else (car ,var)))))
                 (optional (cdr ,var) ,specs ,@body))))))

(define-scheme (read . r)
   (optional r ((port (standard-input)))
      (read port)))

(define-scheme (read-char . r)                       ;; for RRRS
   (optional r ((port (standard-input)))
      (read-char port)))

(define-scheme (char-ready? . r)                       ;; for RRRS
   (optional r ((port (standard-input)))
      (char-ready? port)))

(define-scheme (newline . r)
   (optional r ((port (standard-output)))
      (newline port)
      t))

(define-scheme (write-char c . r)                    ;; for RRRS
   (optional r ((port (standard-output)))
      (write-char port c)
      t))

(define-scheme (princ thing . r)
   (optional r ((port (standard-output)))
      (display thing port)
      t))

(*define scheme-env 'display (*value scheme-env 'princ))

(define-scheme (prin1 thing . r)
   (optional r ((port (standard-output)))
      (print thing port)
      t))

(*define scheme-env 'write   (*value scheme-env 'prin1))

(define-scheme (print thing . r)
   (optional r ((port (standard-output)))
      (format port "~&~S~&" thing)                   ;; Sort of.
      t))

(define-scheme (call-with-input-file spec proc)
   (with-open-ports ((port (open spec '(in))))
      (proc port)))

(define-scheme (call-with-output-file spec proc)
   (with-open-ports ((port (open spec '(out))))
      (proc port)))

(define-scheme (memv x l)
   (mem equiv? x l))

(define-scheme (assv x l)
   (ass equiv? x l))

(define-scheme (member x l)
   (mem alikev? x l))

(define-scheme (assoc x l)
   (ass alikev? x l))

(define-scheme random
  (let ((r (make-random 7)))
    (named-lambda random (n)
      (mod (r) n))))

(define-scheme (char-numeric? ch)
  (digit? ch 10))

(define-scheme (string-ci=? string1 string2)
  (string-equal? (string-upcase string1) (string-upcase string2)))


(define-scheme (substring string start end)
  (substring string start (fx+ (fx- end start) 1)))

(define-scheme (number->string n f)
  (ignore f)
  (format nil "~s" n))

(define-scheme (string->number s)
  (read (string->input-port s)))

(define pi 3.141592653589793)
(define pi/2 1.5707963267948966)

;; Different args from T's ATAN.
(define-scheme (atan y . x-option)
  (let ((y (->float y)))
    (if (null? x-option)
	(atan y)
	(let ((x (->float (car x-option))))
	  (if (and (fl= x 0.0) (fl= y 0.0))
	      (error "arctangent of (0,0)")
	      (cond ((fl= y 0.0)
		     (if (fl< x 0.0) pi 0.0))
		    ((fl= x 0.0)
		     (if (fl< y 0.0) (fl- 0.0 pi/2) pi/2))
		    ((fl< x 0.0)
		     (let ((theta (atan (fl/ y x))))
		       (if (fl< y 0.0) (fl- theta pi) (fl+ theta pi))))
		    (else (atan (fl/ y x)))))))))



(define-scheme (vector . l)
   (list->vector l))

(define-scheme (open-input-file filename)
   (open filename 'in))

(define-scheme (open-output-file filename)
   (open filename 'out))

(define-scheme (t-top)
   (t-top))


;; Need -- ceiling floor round runtime

(define scheme-from-t
      '(t
        nil
        else

        string-set!
	set-car!
	set-cdr!  ; what a hack
;; Primitive procedures (see index to A&S)

        procedure?            
        boolean?          
        apply
        atom?
        car cdr caar cadr cdar cddr
        caaar caadr cadar caddr cdaar cdadr cddar cdddr
        caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
        cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
        cons
        eq?
        eval
        list
        max min
        not                        ;; cheat
        null?
        number?
        symbol?
        remainder
	quotient
        + - * /
        = < >
        1+ -1+
   %%add
   %%subtract
   %%multiply
   %%remainder
   %%less?
   %%equal?

;; These things aren't called "primitive," but are used in the
;; book or problem sets

        force
        abs gcd sqrt
        sin cos exp expt
        <= >=
;;      get put
        assq
        memq
        length
   fx+
   fx-
   fx-and
   fx-ior
   fx-xor
   fx-not
   fx-abs
   fx-negate
   fx-odd?  
   fx-even? 
   fx-bit?  
   fx-ashl  
   fx-ashr  
   fx-ash   
   fx-length
   fx-expt  
   fx-zero? 
   fx*
   fx/ 
   fx=
   fx<
   fx>
   fxn=
   fx>=
   fx<=
   fx-rem
   fl+
   fl-
   fl*
   fl/ 
   fl=
   fl<
   fl>
   fln=
   fl>=
   fl<=

        append
        reverse  
        append!  ; needed by expand quasiquote
;;      reverse!  - ???
        char?
        string->symbol
        symbol->string
        pair?
        integer?
        real?
        rational?
        zero?
        positive?
        negative?
        odd?
        even?
        log
        tan
        asin
        acos
;;      atan                         ;; Different from T's ATAN.
        char-upcase
        char-downcase
        string?
        string-length
        string-append
        string->list
        list->string
        vector?
        make-vector
        vector-length
        vector->list
        list->vector
        map
        call-with-current-continuation
        input-port?
        output-port?

;; MacScheme has this, so what the heck.

        peek-char

;; Macro auxiliaries

        unbound-label                ;; labels
        cons*                        ;; backquote
        or-aux                       ;; or
        no-more-cond-clauses         ;; cond (?)
        display-traced-objects       ;; trace
        set-traced                   ;; trace
        set-untraced                 ;; untrace
        untrace-traced-objects       ;; untrace
        undefined-value              ;; (?)
        make-delay                   ;; delay
        repl-env                     ;; pp
        *pp                          ;; pp
        *pp-symbol                   ;; pp
        disclose                     ;; pp
        *object                      ;; object (for PP hack)
;        extend-pointer-elt           ;; object (for PP hack)
        unquote
        unquote-splicing

	*define-syntax
	make-macro-descriptor
	setter
	make-locale
	
;; Other useful stuff for CS221, non-standard but what the heck...

;;      concatenate-symbol           ;; Make them use (string->symbol (string-append (symbol->symbol ...))) instead?
;;      log

;; Debugging musts, etc.

        compile-file
	compile
        load
        exit
        backtrace
        where-defined
        crawl
        debug                        ;; necessary
        repl-results                 ;; for ##
        ret
      transcript-on
      transcript-off
      *value
      t-implementation-env           ;; for time macro
      gc

       ))

(walk (lambda (sym)
         (*define scheme-env sym (*value scheme-internal-env sym)))
      scheme-from-t)

(define scheme-aliased-from-t
      '((mapcar map)
        (mapc walk)
;        (and *and)
;        (or *or)
        (vector-ref vref)
        (vector-set! vset)
        (vector-fill! vector-fill)

;; A&S

        (make-new-symbol generate-symbol)
        (generate-uninterned-symbol generate-symbol)   ;; Good enough

;; RRRS

        (complex? number?)      ;; ??
        (exact? false)          ;; ??
        (inexact? true)         ;; ??
        (=? =)
        (<? <)
        (>? >)
        (<=? <=)
        (>=? >=)
        (modulo mod)            ;; Close enough
        (eqv? equiv?)           ;; Sort of
        (equal? alikev?)
        (list-ref nth)
        (list-tail nthcdr)
        (last-pair lastcdr)
        (char=? char=)
        (char<? char<)
        (char>? char>)
        (char<=? char<=)
        (char>=? char>=)
        (char-ci=? char=ic)
        (char-ci<? char<ic)
        (char-ci>? char>ic)
        (char-ci<=? char<=ic)
        (char-ci>=? char>=ic)
        (char-alphabetic? alphabetic?)
        (char-whitespace? whitespace?)
        (char-upper-case? uppercase?)
        (char-lower-case? lowercase?)
        (char->integer char->ascii)
        (integer->char ascii->char)
        (string-null? string-empty?)
        (string=? string-equal?)
        (string-ref string-elt)
        (string-fill! string-fill)
        (string-copy copy-string)
        (for-each walk)
        (eof-object? eof?)
        (current-input-port  standard-input)
        (current-output-port standard-output)
        (t-standard-env standard-env)
        (environment-bind! *lset)
	(environment-ref *value)
	(environment-set! *set-value)

       ))

(walk (lambda (foo)
         (*define scheme-env (car foo) (*value scheme-internal-env (cadr foo))))
      scheme-aliased-from-t)

(define-scheme (close-input-port port)
  (close port)
  t)

(define-scheme (close-output-port port)
  (close port)
  t)

(define-scheme (substring-fill! string start end ch)
  (let ((string (enforce string? string))
        (ch (enforce char? ch)))
    (let ((size (string-length string)))
      (cond ((or (fx< end start)
		 (fx< start 0)
		 (fx>= end size))
	     (error "Bad index in ~S"
		    `(SUBSTRING-FILL! ,start ,end ,ch)))
	    (else
	     (do ((i start (fx+ i 1)))
		 ((fx> i end) string)
	       (set (nthchar string i) ch)))))))


(define t-reset (*value t-implementation-env 't-reset))

;;****************************************************************************
'SCHEME_RUNTIME
