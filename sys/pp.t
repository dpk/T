(herald pp (env tsys))

;;;; a pretty-printer

(define-operation (pretty-print obj port)
  (print obj port))

;;; handler for lists.

(define (pp-list x port)
  (cond ((read-macro-inverse x)
         => (lambda (inverse)
              (writes port inverse)
              (pretty-print (cadr x) port)))
        ((print-width-greater? x (fx- (line-length port) (hpos port)))
         ;; if ordinary print won't win...
         (pp-list-vertically x port))
        (else
         (pp-list-horizontally x port))))

;++ obsolete

(define (print-width-greater? obj n)
  (catch abort
    (let ((count 0))
      (print obj (object nil
                   ((writec self char)
                    (ignore char)
                    (set count (fx+ count 1))
                    (if (fx> count n) (abort t)))
                   ((writes self string)
                    (set count (fx+ count (string-length string)))
                    (if (fx> count n) (abort t)))
                   ((output-port? self) '#t)
                   ((port? self) '#t)
                   ((print-type-string self) "Output-port"))))
    nil))


(define (pp-list-vertically   x port)
  (maybe-pp-list-vertically t x port))

(define (pp-list-horizontally x port)
  (maybe-pp-list-vertically nil x port))

(define (maybe-pp-list-vertically vertical? list port)
  (writec port #\()
  (if (null? list) (writec port #\))
    (let ((old-hpos (hpos port)))
      (pretty-print (if (syntax-descriptor? (car list))
                        (identification (car list))
                        (car list))
                    port)
      (if (and vertical? ;heuristic for things like do, cond, ...
               (pair? (car list))
               (not (null? (cdr list))))
          (indent-newline (fx- old-hpos 1) port))
      (let ((old-hpos (fx+ (hpos port) 1)))
        (iterate tail ((flag nil) (l (cdr list)))
          (cond ((pair? l)
                 (cond (flag (indent-newline old-hpos port))
                       (else (writec port #\space))) ; not (space port)!
                 (pretty-print (car l) port)
                 (tail vertical? (cdr l)))
                (else
                 (cond ((not (null? l))
                        (format port " . ")
                        (if flag (indent-newline old-hpos port))
                        (pretty-print l port)))
                 (writec port #\)))))))))

;;; utility: go to given column on a new line.

(define (indent-newline x port)
  (newline port)
  (set-hpos port x))

;;; find printed representation for internal representation of read
;;; macro.

(define (read-macro-inverse x)
  (cond ((and (pair? x)
              (pair? (cdr x))
              (null? (cddr x)))
         (case (car x)
           ((quote)            "'")
           ((quasiquote)       "`")
           ((unquote)          ",")
           ((unquote-splicing) ",@")
           (else nil)))
        (else nil)))

;;; "user interface" stuff

(define (*pp-symbol symbol env)
  (*pp (cond ((syntax-table-entry (env-syntax-table env) symbol)
              => identity)
             ((env-lookup env symbol nil nil)
              => (lambda (loc)
                   (let ((val (contents loc)))
                     (cond ((nonvalue? val) 'unbound)
                           (else val)))))
             (else 'unbound))))

(define (*pp obj)
  (let ((obj (or (disclose obj) obj)))
    (let ((port (terminal-output)))
      (fresh-line port)
      (cond ((and (procedure? obj) (where-defined obj))
             => (lambda (where) (format port "~&;see ~a~%" where)))
            (else
             (pretty-print obj port)))
      (fresh-line port)
      repl-wont-print)))
