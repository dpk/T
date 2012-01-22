(herald symbol_printer 
  (env tsys (osys symbol) 
            (osys readtable)))

;;; Fancy symbol printer.  It handles slashification.  The fancy
;;; symbol printer attempts to print the symbol using minimum number
;;; of characters under the constraint of rereadability.

(define fancy-symbol-printing?
  (let ((val t))
    (object (lambda () val)
      ((setter self)
       (lambda (val)
         (let ((val (enforce boolean? val)))
           (if val
               (set *write-symbol* fancy-write-symbol)
               (set *write-symbol* plain-write-symbol))))))))

(define-constant special-initials "-+!$%&*/:<=>?~_^")

(define (special-initial? ch)
  (string-posq ch special-initials))

(define-integrable (escaped-initial? symbol)
  (let ((ch (symbol-elt symbol %%symbol-text-offset)))
    (not (or (uppercase? ch)
             (special-initial? ch)))))

; scheme doesn't allow "-" or "+" as an initial.
;            (and (fx= (symbol-length symbol)
;                      (fx+ %%symbol-text-offset 1))
;                 (sign-char? ch))))))

(define (fancy-write-symbol port symbol)
  (receive (i escape?)
           (cond ((fx= 0 (symbol-length symbol))
                  (return %%symbol-text-offset t))
                 ((escaped-initial? symbol)
                  (return (fx+ %%symbol-text-offset 1) t))
                 (else
                  (return %%symbol-text-offset nil)))
    (iterate loop ((i i) (escape? escape?) (delimit? '#f))
      (cond ((fx>= i (symbol-length symbol))
             (cond (delimit? (write-delimited-symbol port symbol escape?))
                   (escape?  (write-escaped-symbol   port symbol))
                   (else     (plain-write-symbol  port symbol)))
             (no-value))
            (else
             (let* ((ch (symbol-elt symbol i))
                    (e  (char-syntax *print-table* ch)))
               (select e
                 ((%%constituent)
                  (cond ((neq? ch ((rt-translator *print-table*) ch))
                         (loop (fx+ i 1) '#t delimit?))
                        (else
                         ;; this is the most important case ...
                         (loop (fx+ i 1) escape? delimit?))))
                 ((%%escape-char)
                  (loop (fx+ i 1) '#t delimit?))
                 ((%%whitespace %%ignored %%undefined)
                  (loop (fx+ i 1) escape? '#t))
                 (else
                  ;; read macro.
                  (cond ((or (not (constituent-syntax? e))
                             (fx= i 0))
                         (loop (fx+ i 1) '#t delimit?))
                        (else
                         (loop (fx+ i 1) escape? delimit?)))))))))))


;;; The symbol has no funny characters, but if it were printed using
;;; PLAIN-WRITE-SYMBOL then it would be re-read as a number or
;;; some random thing.

(define (write-delimited-symbol port symbol escaped?)
  (cond (*symbol-delimiter*
         (writec port *symbol-delimiter*)
         (if escaped?
             (write-escaped-symbol port symbol)
             (plain-write-symbol port symbol))
         (writec port *symbol-delimiter*))
        (else
         (write-losing-symbol port symbol))))

;;; Try to print symbol, escaping any characters which can't be
;;; reread for one reason or another.

(define (write-escaped-symbol port symbol)
  (let ((len (symbol-length symbol))
        (writec (if (iob? port) vm-write-char write-char)))
    (cond (*escape-char*
           ;; we know it's a symbol.
           (if (and (fx>= len 1) (escaped-initial? symbol))
               (writec port *escape-char*))
           (writec port (symbol-elt symbol %%symbol-text-offset))
           (iterate loop ((i (fx+ %%symbol-text-offset 1)))
             (cond ((fx>= i len) (no-value))
                   (else
                    ;++ what about control characters?
                    (let* ((ch  (symbol-elt symbol i))
                           (syn (char-syntax *print-table* ch)))
                      (cond ((or (not (constituent-syntax? syn))         ; e.g. \(
                                 (and (not (fixnum? syn)) (fx= i 0))     ; e.g. \'x
                                 (neq? ch ((rt-translator *print-table*) ch))) ; e.g. \x
                             (writec port *escape-char*)
                             (writec port ch))
                            (*translate-constituent-inverse*
                             (writec port (*translate-constituent-inverse* ch)))
                            (else
                             (writec port ch))))
                    (loop (fx+ i 1))))))
          (*symbol-delimiter*
           (writec port *symbol-delimiter*)
           (plain-write-symbol port symbol)
           (writec port *symbol-delimiter*))
          (else
           (write-losing-symbol port symbol)))))

;;; We use this routine when none of the above strategies is
;;; appropriate.

(define (write-losing-symbol port symbol)
  (write-string port "#[Symbol \"")
  (write-string port (symbol->string symbol))
  (write-string port "\"]"))
