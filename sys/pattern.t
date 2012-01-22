(herald pattern)

;;; An attempt at a general syntax checking mechanism ala JAR's 421 parser

;   #f             any s-expression
;   FOO            any X such that (FOO X) => #t
;   (X ... Y)      a list of X ... Y
;   (X ... Y . Z)  a list of X ... Y and then a cdr of Z
;   (* X)          proper list of Xs
;   (@ X)          spliced in list of Xs, this is not valid outside a list
;   (+ X)          nonempty list of Xs
;   (| X Y ...)    an X or a Y or ...
;   (! X)          the symbol X

;;; (pattern-predicate spec) => predicate for spec

(define-syntax (pattern-predicate pattern)
  (expand-pattern pattern))

;;; Main dispatch for patterns

(define (expand-pattern pattern)
  (cond ((not pattern)
         '(lambda (x) (ignore x) t))
        ((atom? pattern)
         pattern)
        (else
         (case (car pattern)
           ((*) (repetition-pattern (cadr pattern)))
           ((+) (positive-pattern (cadr pattern)))
           ((|) (disjunction-pattern (cdr pattern)))
           ((!) `(lambda (x) (eq? x ',(cadr pattern))))
           ((@) (error "match found @ outside of a sequence in ~S" pattern))
           (else 
            (sequence-pattern pattern))))))

(define (repetition-pattern pattern)
  (cond ((eq? pattern '#f)
         'proper-list?)               ; efficiency hack
        (else
         (let ((pred (expand-pattern pattern))
               (l (generate-symbol 'l)))
           `(lambda (,l)
              (iterate loop ((,l ,l))
                (cond ((null? ,l) t)
                      ((atom? ,l) nil)
                      ((,pred (car ,l)) (loop (cdr ,l)))
                      (else nil))))))))

(define (positive-pattern pattern)
  (let ((l (generate-symbol 'l)))
    `(lambda (,l)
       (if (null? ,l)
           nil
           (,(repetition-pattern pattern) ,l)))))

(define (disjunction-pattern patterns)
  (let ((x (generate-symbol 'x)))
    `(lambda (,x)
       (or . ,(map (lambda (pattern)
                     `(,(expand-pattern pattern) ,x))
                   patterns)))))

(define (sequence-pattern pattern)
  (iterate loop ((p pattern) (preds '()))
    (cond ((null? p)
           (finish-sequence preds 'null?))
          ((atom? p)
           (finish-sequence preds p))
          ((memq? (car p) '(* + | !))
           (finish-sequence preds (expand-pattern p)))
          ((and (pair? (car p))
                (eq? (caar p) '@))
           (finish-sequence preds (spliced-pattern p)))
          (else
           (loop (cdr p) (cons (expand-pattern (car p)) preds))))))

(define (finish-sequence preds final)
  (iterate loop ((preds preds) (form final))
    (if (null? preds)
        form
        (let ((l (generate-symbol 'l)))
          (loop (cdr preds)
                `(lambda (,l)
                   (and (pair? ,l)
                        (,(car preds) (car ,l))
                        (,form (cdr ,l)))))))))

(define (spliced-pattern pattern)
  (let ((pred (expand-pattern (cadar pattern)))
        (l (generate-symbol 'l))
        (p (generate-symbol 'p)))
    `(lambda (,l)
       (let ((,p ,(sequence-pattern (cdr pattern))))
         (iterate loop ((,l ,l))
           (cond ((null? ,l)
                  ,(if (null? (cdr pattern))
                       't
                       `(,p '())))
                 ((atom? ,l)
                  (,p ,l))
                 ((,pred (car ,l))
                  (loop (cdr ,l)))              
                 (else
                  (,p ,l))))))))

