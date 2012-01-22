(herald macros (env tsys))

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

;;;; Standard macros

(define-safe-syntax (define . x)
                    (| (symbol? #f)
                       ((symbol? . formals-list?) . (+ #f)))
  (cond ((pair? (car x))
         `(,(t-syntax 'define-variable-value) ,(caar x)
            (,(t-syntax 'named-lambda) ,(caar x) ,(cdar x) . ,(cdr x))))
        ((null? (cddr x))
         `(,(t-syntax 'define-variable-value) ,(car x) ,(cadr x)))
        (else
         (syntax-error "illegal definition syntax~%  ~S" `(define . ,x)))))

(define-syntax (define-integrable . x)
  `(,(t-syntax 'define-constant) . ,x))

(define-safe-syntax (define-constant . x)
                    (| (symbol? #f)
                       ((symbol? . formals-list?) . (+ #f)))
  (receive (ok? var val)
           (cond ((pair? (car x))
                  (return t
                          (caar x)
                          `(,(t-syntax 'named-lambda) ,(caar x) ,(cdar x) . ,(cdr x))))
                 ((null? (cddr x))
                  (return t (car x) (cadr x)))
                 (else
                  (return nil nil nil)))
    (if (not ok?)
        (syntax-error "illegal definition syntax~%  ~S" `(define-constant . ,x))
        `(,(t-syntax 'block)
          (,(t-syntax 'declare) constant ,var)
          (,(t-syntax 'define-variable-value) ,var ,val)))))

(define-syntax (define-recursive . x)
  (cond ((pair? (car x))
         `(,(t-syntax 'define-variable-value) ,(caar x)
            (,(t-syntax 'named-lambda) ,(caar x) ,(cdar x)
              (labels ((,(car x)  ,@(cdr x)))
                ,(car x)))))
        ((null? (cddr x))
         ;++ this should probably be an error
         `(,(t-syntax 'define-variable-value) ,(car x)
            (labels ((,(car x) ,(cadr x)))
                ,(car x))))
        (else
         (syntax-error "illegal definition syntax~%  ~S"
                       `(define-recursive . ,x)))))

;;; We use BLOCK here to keep the LSET-VARIABLE-VALUE at top level.
;;; Returning PLACE is done for compatibility with T2.

(define-syntax (lset place value)
  `(,(t-syntax 'block)
     (,(t-syntax 'lset-variable-value) ,place ,value)
       ,place))


(define-syntax (define-handler name . body)
  (let ((type    (concatenate-symbol 'header/ name))
        (handler (concatenate-symbol 'handle- name)))
  `(block
    (define ,handler ,@body)
    (set (vref *handlers* (fx-ashr ,type 2)) ,handler)
    (no-value))))

(define-syntax (set place value)
  (let ((var (generate-symbol 'set)))
    `(let ((,var ,value))
       ,(cond ((atom? place)
               `(,(t-syntax 'set-variable-value) ,place ,var))
              (else
               `((setter ,(car place)) ,@(cdr place) ,var)))
       ,var)))

(define-syntax (locative form)
  (cond ((atom? form)
	 `(,(t-syntax 'var-locative) ,form))
	(else
	 `(make-locative ,@form))))     ; ??

(define-syntax (delay x)
  `(make-delay (,(t-syntax 'lambda) () ,x)))

(define (blockify x)
  (cond ((atom? x) ''#f)
        ((atom? (cdr x)) (car x))
        (else `(,(t-syntax 'block) ,@x))))

(define-safe-syntax (block0 val . body)
                    (+ #f)
  (let ((g (generate-symbol 'block0)))
    `((,(t-syntax 'lambda) (,g) ,@body ,g) ,val)))

(define-safe-syntax (define-structure-type type-id . specs)
                    (symbol? (@ symbol?) . (| null? ((* valid-method-form?))))
  (let ((stype (concatenate-symbol type-id '-stype)))
    (receive (handler specs)
             (let* ((last (last specs))
                    (h (cond ((not (pair? last)) '())
                             (else last))))
               (return `(object nil
                          ,@h
                          ((crawl-exhibit self)
                           (exhibit-structure self))
                          ((print self port)
                           (print-structure self port)) ;??
                          ((structure-type self) ,stype))
                       (if (pair? last)
                           (delq last specs)
                           specs)))
      `(block (define ,stype (make-stype ',type-id ',specs '#f))
              (set (stype-handler ,stype) ,handler)
              (define ,(concatenate-symbol 'make- type-id)
                (stype-constructor ,stype))
              (define ,(concatenate-symbol type-id '?)
                (stype-predicator ,stype))
              ,@(do ((s specs (cdr s))
                     (i 2 (fx+ i 4))
                     (z '()
                        (cons `(define-constant
                                ,(concatenate-symbol type-id '- (car s))
                                (make-structure-accessor ,stype ,i ',(car s)))
                              z)))
                  ((null? s) (reverse! z)))
              ,stype))))


;;; iteration constructs

(comment
(define-syntax (label clause-1 . rest)
  (let ((form-iterate (lambda (name varinits body)
                        `(,(t-syntax 'labels)
                              ((,name (,(t-syntax 'lambda) ,(map car varinits)
                                        . ,body)))
                           (,name . ,(map cadr varinits))))))
    (if (symbol? clause-1)
        (form-iterate clause-1 (car rest) (cdr rest))
      (form-iterate (car clause-1) (cdr clause-1) rest))))
)

;;; Pattern doesn't allow for (ITERATE (FOO (X 1) ...) ...)

(define-safe-syntax (iterate clause-1 . rest)
                    (symbol? (* (symbol? #f)) . (+ #f))
  (let ((form-iterate (lambda (name varinits body)
                        `(,(t-syntax 'labels)
                              ((,name (,(t-syntax 'lambda) ,(map car varinits)
                                        . ,body)))
                           (,name . ,(map cadr varinits))))))
    (if (symbol? clause-1)
        (form-iterate clause-1 (car rest) (cdr rest))
      (form-iterate (car clause-1) (cdr clause-1) rest))))


;;; expand into explicit lambda's in the rhs's of labelses, so as to
;;; increase the information content of backtrace output.  (otherwise the
;;; local procedures handle identification, and you get lots of loop's and
;;; do.137's in the backtrace.)

(define-safe-syntax (do specs end . body)
                    ((* (| (symbol? #f #f) (symbol? #f) (symbol?)))
                     (+ #f)
                     . (* #f))
  (let ((loop (generate-symbol 'do)))
    `(,(t-syntax 'labels)
       ((,loop (,(t-syntax 'lambda) ,(map car specs)
                 (,(t-syntax 'cond) ,end
                   (else ,(blockify
                           `(,@body
                             (,loop
                              ,@(map (lambda (y)
                                       (if (and (cdr y) (cddr y))
                                           (caddr y)
                                           (car y)))
                                     specs)))))))))
       (,loop ,@(map (lambda (y) (if (cdr y) (cadr y) ''#f)) specs)))))
                
;;; (unwind-protect body . unwind-forms)

(define-safe-syntax (unwind-protect body . unwind-forms)
                    ((* #f) . (+ #f))
  `(unwind-protect-handler (,(t-syntax 'lambda) () ,body)
                           (,(t-syntax 'lambda) () . ,unwind-forms)))

;;; (with-output-to-string var . body) binds VAR to an output port
;;; and executes the body.  Anything written to the output port
;;; during the execution of body is accumulated in a string, which
;;; is returned as the value of the with-output-to-string-expression.

;(define-safe-syntax (with-output-to-string pat . body)
;                    (symbol? . (+ #f))
;  (let ((var (if (pair? pat) (car pat) pat)))
;    `(let ((,var (make-output-to-string-stream)))
;       ,@body
;       (close ,var))))

(define-safe-syntax (with-output-to-string pat . body)
                    (symbol? . (+ #f))
  (let ((var (if (pair? pat) (car pat) pat)))
    `(,(t-syntax 'let) ((,var (get-buffer)))
       ,@body
       (let ((val (buffer->string ,var)))
         (release-buffer ,var)
         val))))

(define-safe-syntax (with-input-from-string pat . body)
                    ((symbol? #f) . (+ #f))
  (let ((var (car pat))
        (string (cadr pat)))
    `(,(t-syntax 'let) ((,var (string->input-port ,string)))
       (,(t-syntax 'block0) ,(blockify body)
                            (close ,var)))))

;++ This isn't used or released should we flush it.
;;; (with-output-to-list var . body) is like (string->list
;;; (with-output-to-string var . body)).  It could be implemented
;;; more efficiently but i'm too lazy to do so.

(define-syntax (with-output-to-list var . body)
  `(string->list (with-output-to-string ,var . ,body)))

;;; (WITH-OUTPUT-WIDTH-port VAR . BODY) is like
;;; WITH-OUTPUT-TO-STRING, but instead of accumulating characters
;;; in a string, it counts them.  the value returned is the number
;;; of characters counted.  For an example see the definition
;;; of printwidth.

(define-syntax (with-output-width-port var . body)
  `(let ((,var (make-output-width-port)))
     ,@body
     (close ,var)))

;;; with-open-ports

(define-safe-syntax (with-open-ports specs . body)
                    ((* (symbol? #f)) . (+ #f))
  `(with-open-ports-handler
    (,(t-syntax 'lambda) ,(map car specs)
      . ,body)
    ,@(map (lambda (spec)
             `(,(t-syntax 'lambda) () ,(cadr spec)))
           specs)))

;;; random

(define-syntax (import env . vars)
  (let ((g (generate-symbol 'import)))
    `(,(t-syntax 'let) ((,g ,env))
       ,@(map (lambda (var)
                (let ((var (enforce symbol? var )))
                  `(,(t-syntax 'define) ,var (*value ,g ',var))))
              vars))))

;(define-syntax (export env . vars)
;  (let ((g (generate-symbol 'export)))
;    `(let ((g ,env))
;       ,@(map (lambda (var) `(*define ,g ',var ,var))
;              vars))))

(define-syntax (require name . maybe-path)
  (cond (maybe-path
         `(*require ',name ',(car maybe-path) (,(t-syntax 'the-environment))))
        (else
         `(*require '() ',name (,(t-syntax 'the-environment))))))

(define-syntax (catch var . body)
  `(*catch (,(t-syntax 'lambda) (,var) . ,body)))

(define-syntax (comment . rest)
  (ignore rest)
  ''comment)                            ; a tradition of sorts.

(define-syntax (ignore . vars)
  `(,(t-syntax 'declare) ignore . ,vars))

(define-syntax (ignorable . vars)
  `(,(t-syntax 'declare) ignorable . ,vars))

(define-syntax (herald . rest)
  (syntax-error "herald form in illegal context~%  ~s"
                `(herald . ,rest)))

;;; This is used for critical sections, usually for heap integrety
;;; or other GC related reason.

(define-syntax (defer-interrupts . body) 
  `(,(t-syntax 'block) ,@body))

;(define-syntax (defer-interrupts . body) 
;  `(block (disable-interrupts)
;          (let ((val (block ,@body)))
;            (enable-interrupts)
;            val)))

;;; Free list versions of POP and PUSH

(define-syntax (free-push form thing)
  (let ((fetch (generate-symbol 'fetch))
        (store (generate-symbol 'store)))
    `(,(t-syntax 'modify-location) , form
       (,(t-syntax 'lambda) (,fetch ,store)
         (,store (cons-from-freelist ,thing (,fetch)))))))

(define-syntax (free-pop form)
  `(,(t-syntax 'modify-location) ,form
     (,(t-syntax 'lambda) (fetch store)
       (,(t-syntax 'let*) ((temp (fetch)) (v (car temp)))
         (store (cdr temp))
         (return-to-freelist temp)
         v))))


;*** (WITH-BUFFERS specs . BODY)
;*** =======================================================================
;*** This macro is used to allocate and release one or more buffers.
;*** Specs is a list of (name size) pairs.  If size is omitted then
;*** the minimum buffer size (64 bytes) is used.  With-buffers forces
;*** the release of the named buffers on a throw past the binders.
;***

(define-safe-syntax (with-buffers specs . body)
                    ((* (symbol? . (| null? (#f)))) . (+ #f))
  (cond ((every? valid-spec? specs)
         `(let (,@(map (lambda (spec)
                         (cond ((atom? (cdr spec))
                                `(,(car spec) (get-buffer)))
                               (else
                                `(,(car spec) (get-buffer-of-size
                                               ,(cadr spec))))))
                       specs))
            (receive vals (block ,@body)
              ,@(map (lambda (spec)
                       `(release-buffer ,(car spec)))
                     specs)
               (apply return vals))))    
        (else
         (syntax-error "illegal spec~%  ~S"
                      `(with-buffer ,specs . ,body)))))


;;; Define a simple switch.

(define-syntax (define-simple-switch name type . initial-value)
  `(define ,name (make-simple-switch ',name ,type ,@initial-value)))

;;; Useful for defining unimplemented procedures

(define-syntax (define-unimplemented pat . msg)
  (destructure (((name . args) pat))
    (let ((fmt (if msg
                   "~a is unimplemented~%**~11t~a"
                   "~a is unimplemented")))
      `(define (,name ,@args)
         (error ,fmt (identification ,name) ,@msg)))))

;;; Assert - (assert (fixnum? x) (float? y) (structure? z))

(define-syntax (assert . types)
  `(block
    (,(t-syntax 'declare) assert ,@types)
    (no-value)))

;;; syntax-related stuff

(define-safe-syntax (safe-macro-expander pat pattern . rest)
                    ((symbol? . formals-list?) #f . (+ #f))
  (let* ((pat (enforce pair? pat ))
         (symbol (car pat))
         (args   (cdr pat))
         (z (generate-symbol 'macro)))
    `(make-macro-descriptor (,(t-syntax 'named-lambda) ,symbol (,z)
                              (,(t-syntax 'ignorable) ,z)
                              ;; careful!  look at disclose-macro-expander.
                              (,(t-syntax 'destructure) (((#f . ,args) ,z))
                                . ,rest))
                            (,(t-syntax 'pattern-predicate) ,pattern)
                            ',symbol)))

(define-safe-syntax (define-safe-syntax pat pattern . rest)
                    (| (symbol? #f #f)
                       ((symbol? . formals-list?) #f . (+ #f)))
  (let ((construct
         (lambda (symbol descr)
           (let ((symbol (enforce symbol? symbol)))
             `(*define-syntax (,(t-syntax 'the-environment))
                              ',symbol
                              ,descr)))))
    (cond ((pair? pat)
           (construct (car pat)
                      `(,(t-syntax 'safe-macro-expander)
                        ,pat ,pattern . ,rest)))
          (else
           (construct pat (car rest))))))

;;; Unsafe versions of the above

(define-safe-syntax (macro-expander pat . rest)
                    ((symbol? . formals-list?) . (+ #f))
  (let ((pattern 
         (if (null? (cdr pat))
             'null?
             (iterate loop ((l (cdr pat)) (pat '()))
               (cond ((null? l) pat)
                     ((atom? l) (append! pat 'true))
                     (else
                     (loop (cdr l) (cons '#f pat))))))))
    `(,(t-syntax 'safe-macro-expander) ,pat
                                       ,pattern
                                       . ,rest)))

(define-safe-syntax (define-syntax pat . rest)
                    (| (symbol? #f)
                       ((symbol? . formals-list?) . (+ #f)))
  (let ((construct
         (lambda (symbol descr)
           (let ((symbol (enforce symbol? symbol)))
             `(*define-syntax (,(t-syntax 'the-environment))
                              ',symbol
                              ,descr)))))
    (cond ((pair? pat)
           (construct (car pat)
                      `(,(t-syntax 'macro-expander) ,pat . ,rest)))
          (else
           (cond ((not (null? (cdr rest)))
                  (syntax-error "too many subforms~%  ~s"
                                `(define-syntax ,pat . ,rest))))
           (construct pat (car rest))))))
