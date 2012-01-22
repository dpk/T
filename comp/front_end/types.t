(herald types)

; A type is
;   a fixnum (bitvector) or
;   a list (<number> {<arg-vec> | #f} . (<op> . <arg-vec>)*)

(lset *next-index* 0)

(define type-names (make-table 'type-names))
(define type-indices (make-table 'type-indices))
(define simple-types (make-table 'simple-types))

(define (create-type simple proc)
  (if proc (cons simple proc) simple))

(define (type? x)
  (or (fixnum? x)
      (and (list? x)
           (fx< 1 (length x))
           (fixnum? (car x))
           (vector? (cadr x)))))

(define (proc-type? type)
  (and (pair? type)
       (cadr type)))

(define (proc-type-args type)
  (if (and (pair? type) (cadr type))
      (cadr type)
      (error "type ~S has no procedure part" type)))

(define (proc-type-nargs type)
  (if (and (pair? type) (cadr type))
      (fx- (vector-length (cadr type)) 1)
      (error "type ~S has no procedure part" type)))

(define (proc-type-n-ary? type)
  (if (and (pair? type) (cadr type))
      (vref (cadr type) 0)
      (error "type ~S has no procedure part" type)))

(define (object-type? type)
  (and (proc-type? type)
       (not (null? (cddr type)))))

(define (type-simple type)
  (if (fixnum? type)
      type
      (car type)))

(define (type-proc type)
  (if (fixnum? type)
      nil
      (cdr type)))

(define (type-name type)
  (let ((simple (type-simple type))
        (proc (type-proc type)))
    (let ((name (cond ((fx= simple (type-simple type/top))
                       '(top))
                      ((fxn= simple (type-simple type/bottom))
                       (append (simple-type-names simple)
                               (proc-type-names proc)))
                      (else
                       (proc-type-names proc)))))
      (if (cdr name) (cons 'or name) (car name)))))

(define (simple-type-names simple)
  (do ((i 0 (fx+ i 1))
       (r '() (if (fx= 1 (bit-field simple i 1))
                  (cons (table-entry type-names i) r)
                  r)))
      ((fx>= i *next-index*) (reverse! r))))

(define (proc-type-names procs)
  (cond ((null? procs)
         '())
        ((null? (cdr procs))
         `((proc . ,(print-arg-vector (car procs)))))
        (else
         `((object ,(if (car procs)
                        `(proc . ,(print-arg-vector (car procs)))
                        '#f)
                   . ,(map (lambda (p)
                             (if (car p)
                                 `(,(car p) . ,(print-arg-vector (cdr p)))
                                 '(#f)))
                           (cdr procs)))))))

(define (print-arg-vector vec)
  `(,(vref vec 0)
    . ,(map! type-name (cdr (vector->list vec)))))

(define type-predicates (make-table 'type-predicates))

(define (type-predicate type)
  (cond ((and (not (type-proc type))
              (table-entry type-predicates (type-simple type)))
         => identity)
        (else nil)))

(define (make-simple-type name predicate)
  (let* ((simple (set-bit-field 0 *next-index* 1 1))
         (new (create-type simple '())))
    (set (table-entry type-names *next-index*) name)
    (set (table-entry type-indices name) *next-index*)
    (set (table-entry simple-types name) new)
    (set (table-entry type-predicates simple) predicate)
    (increment *next-index*)
    new))

(define type/top (create-type -1 '()))
(set (table-entry simple-types 'top) type/top)
(define type/bottom (create-type 0 '()))
(set (table-entry simple-types 'bottom) type/bottom)

(define (type-top? type)
  (and (type? type)
       (fx= -1 (type-simple type))))

(define (type-bottom? type)
  (and (type? type)
       (fx= 0 (type-simple type))
       (null? (type-proc type))))

(define-local-syntax (define-simple-type name . maybe-pred)
  (let ((pred (if (null? maybe-pred)
                  (concatenate-symbol name '?)
                  (car maybe-pred))))
    `(define ,(concatenate-symbol 'type/ name)
       (make-simple-type ',name ',pred))))

(define-simple-type pair)
(define-simple-type fixnum)

(define-simple-type null)

(define-simple-type char)
(define-simple-type nonvalue)
(define-simple-type the-true true?)
(define-simple-type template)

(define-simple-type unit)
(define-simple-type text)
(define-simple-type string)
(define-simple-type vector)
(define-simple-type symbol)
(define-simple-type bytev)
(define-simple-type xenoid)
(define-simple-type cell)
(define-simple-type weak)
(define-simple-type task)
(define-simple-type vcell)
(define-simple-type vframe)
(define-simple-type stack)
(define-simple-type bignum)
(define-simple-type double-float)
(define-simple-type single-float)
(define-simple-type ratio)
(define-simple-type complex)
(define-simple-type foreign)

(define (type-lookup name)
  (cond ((table-entry simple-types name)
         => identity)
        (else
         (error "couldn't find type ~S" name))))

(define (make-composite-type name predicate . children)
  (iterate loop ((c children) (r type/bottom))
    (cond ((null? c)
           (set (table-entry simple-types name) r)
           (if predicate
               (set (table-entry type-predicates r) predicate))
           nil)
          (else
           (loop (cdr c)
                 (type-union r (table-entry simple-types (car c))))))))

(make-composite-type 'list    'list?    'null 'pair)
(make-composite-type 'integer 'integer? 'fixnum 'bignum)
(make-composite-type 'real    'real?    'double-float 'single-float)
(make-composite-type 'number  'number?  'integer 'real 'ratio)
(make-composite-type 'boolean 'top)
;(make-composite-type 'extend) ???

(define (->type spec)
  (cond ((type? spec)
         spec)
        ((null? spec)
         type/bottom)
        ((atom? spec)
         (type-lookup spec))
        ((eq? (car spec) 'or)
         (iterate loop ((l (cdr spec)) (type type/bottom))
           (cond ((null? l)
                  type)
                 (else
                  (loop (cdr l) (type-union (->type (car l)) type))))))
        ((eq? (car spec) 'proc)
         (create-type 0 (list (make-arg-vector (cdr spec)))))
        ((eq? (car spec) 'object)
         (make-object-type (cdr spec)))
        (else
         (error "don't know how to make ~S into a type" spec))))

(define (make-arg-vector specs)
  (let ((vec (make-vector (length specs))))
    (set (vref vec 0) (if (car specs) '#t '#f))
    (do ((i 1 (fx+ 1 i))
         (s (cdr specs) (cdr s)))
        ((null? s))
      (set (vref vec i) (->type (car s))))
    vec))

(define (make-object-type specs)
  (destructure (((proc . methods) specs))
    (create-type 0 `(,(if proc
                          (make-arg-vector (cdr proc))
                          '#f)
                     . ,(map parse-method-spec methods)))))

(define (parse-method-spec spec)
  (destructure (((op . args) spec))
    (cond ((not op) '(#f))
          ((not (symbol? op))
           (error "bad operation type specification ~S" spec))
          (else
           `(,op . ,(make-arg-vector args))))))

(define type/true (type-subtract type/top type/null))

(set (table-entry *read-keyword-table* 'type)
     (lambda (key port rt)
       (ignore key) 
       (->type (car (read-to-right-bracket port #\] rt)))))



