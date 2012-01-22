(herald zvm_undefined (env tsys))

(define-local-syntax (define-undefined name)
  `(define (,name . args)
     (error "call to undefined procedure - ~a"
            (cons ',name args))))

;;; Handlers

;;; i/o ports

;;; i/o operations

(define-undefined PRINT-FLONUM)
(define-undefined READ-OBJECT)
(define-undefined MAKE-OUTPUT-WIDTH-PORT)


;;; Filenames

(define-undefined MAKE-FILENAME)
(define-undefined ->FILENAME)
(define-undefined FILENAME-NAME)
(define-undefined FILENAME?)
(define-undefined FILENAME->STRING)
(define-undefined PARSE-HERALD)
(define-undefined HERALD-FILENAME)


;;; T repl

(define-undefined STANDARD-READ-TABLE)

(define-undefined T-FORMAT-AUX)
(define-undefined T-BREAKPOINT)
(define-undefined PREVIOUS-CONTINUATION)
(define-undefined GET-LOADED-FILE)
(define-undefined GET-ENVIRONMENT)

;;; Loader

(define-undefined SET-BIGNUM-SIGN!)
(define-undefined BIGNUM-NEGATE)
(define-undefined CREATE-BIGNUM)
(define-undefined MAKE-DUMP-STATUS)
(define-undefined STRING->FLONUM)


;;; Syntax

(define-undefined ENV-FOR-SYNTAX-DEFINITION)
(define-undefined MAKE-SYNTAX-TABLE)
(define-undefined SYNTAX-DESCRIPTOR?)
(define-undefined CHECK-SPECIAL-FORM-SYNTAX)
(define-undefined STANDARD-SYNTAX-TABLE)
(define-undefined EXPAND-MACRO-FORM)
(define-undefined SYNTAX-TABLE-ENTRY)


;;; Crawl

(define-undefined MAYBE-CRAWL-COMPONENT)
(define-undefined CRAWL-PRINT-COMPONENT)
(define-undefined CRAWL-EXHIBIT)
(define-undefined EXHIBIT-STANDARD-EXTEND)
(define-undefined CRAWL-PUSH)
(define-undefined FRAME-PRINT-SYNOPSIS)
(define-undefined PRINT-ONE-LINE)
(define-undefined PRETTY-PRINT)
(define-undefined PP-LIST)
