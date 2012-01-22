(herald herald (env tsys))

;;; HERALD parsing cruft!

;;; The CADR of a HERALD is just a filespec.  E.g.
;;;            (HERALD (TSYS LOAD T 54) ...)
;;;   or even  (HERALD #[Filename YALE-RING TSYS LOAD T 54] ...)
;;; This is the filespec for "what the file thinks it is."  It may be
;;; different from the place where the file is actually living.

;;; The parsed-HERALD structure.

(define-structure-type herald
    filename        ;Filename, or whatever.
    read-table      ;An expression specifying a read table.
    syntax-table    ;An expression specifying a syntax table.
    support         ;Expression specifying early binding environment.
    environment     ;An expression specifying environment into which to load.
                    ; (Actually, not yet; this is still compatible with 2.7.)
    language        ;Expression evaluating to a "language," whatever that is.
    )

(let ((h (stype-master herald-stype)))
  (set (herald-read-table   h) nil)
  (set (herald-syntax-table h) nil)
  (set (herald-support      h) nil)
  (set (herald-environment  h) nil)
  (set (herald-language     h) nil))

(define (make-default-herald filename)
  (let ((h (make-herald)))
    (set (herald-filename  h) filename)
    h))

(lset *herald-items*
      `((read-table   ,herald-read-table   0 context)
        (syntax-table ,herald-syntax-table 1 context)
        (support      ,herald-support      2 context)
        (env          ,herald-environment  3 context)
        (language     ,herald-language     4 context)))

;;; Parse a HERALD.  Returns a HERALD structure.

(define (parse-herald filespec clauses)
  (let ((h (make-herald))
        (clause-ordinal (lambda (c)
                          (cond ((and (pair? c)
                                      (assq (car c) *herald-items*))
                                 => caddr)
                                (else 1000)))))
    (set (herald-filename h)
         (cond ((null? filespec) nil)
               ((filespec? filespec) (->filename filespec))
               (else
                (syntax-error "bad filespec in ~s form~%  ~s"
                              'herald
                              `(herald ,filespec ,@clauses))
                nil)))
    (iterate loop ((l (sort clauses
                            (lambda (c1 c2)
                              (fx< (clause-ordinal c1) (clause-ordinal c2)))))
                   (items *herald-items*)
                   (prev nil))
      (cond ((or (null? items)
                 (null? l)
                 (and (not (null? l))
                      (not (pair? (car l)))))
             (if (not (null? l))
                 (syntax-error "illegal ~s clause(s)~%  ~s"
                               'herald
                               `(herald ,filespec . ,clauses)))
             h)
            ((eq? (caar l) prev)
             (syntax-error "duplicate ~s clause~%  ~s"
                           'herald (car l))
             (loop (cdr l) items prev))
            ((neq? (caar l) (caar items))
             ;; Use default value.
             (loop l (cdr items) nil))
            (else
             (let ((z (car items)) (c (car l)))
               (set ((cadr z) h)
                    (xcase (cadddr z)
                      ((context)
                       (let ((foo (cond ((and (eq? (car c) 'env)
                                              (eq? (cadr c) 't))
                                         'standard-early-binding-env)
                                        ((and (eq? (car c) 'env)
                                              (eq? (cadr c) 'tsys))
                                         'base-early-binding-env)
                                        (else
                                         (cadr c)))))
                         (cond ((null? (cdr c))
                                (syntax-error "illegal ~s clause~%  ~s"
                                              'herald c))
                               ((null? (cddr c)) foo)
                               (else
                                `(',augment-context ,foo
                                                    ,@(map (lambda (f) `',f)
                                                           (cddr c)))))))
                      ((boolean) t)))
               (loop (cdr l) (cdr items) (car c))))))))

;++ what is this?
(define-operation (augment-context context . specs)
  (cond ((environment? context)
         (walk (lambda (spec)
                 (*require nil spec context))
               specs)
         context)
        (else
         (error "unimplemented HERALD operation~%  ~S"
                `(augment-context ,context . ,specs))
         context)))
