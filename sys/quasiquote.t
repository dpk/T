(herald quasiquote)

;;; Actual quasiquote stuff

(define-syntax (quasiquote x)
  (expand-quasiquote x))

(define (unquote x)
  (syntax-error "comma not inside backquote form~%  ,~s" x))

(define (unquote-splicing x)
  (syntax-error "\",@\" not inside backquote form~%  ,@~s" x))

(define (expand-quasiquote x)
  (receive (mode arg)
           (descend-quasiquote x 0)
    (finalize-quasiquote mode arg)))

(define quasiquote-marker 'quasiquote)
(define unquote-marker    'unquote)
(define splice-marker     'unquote-splicing)

(define (finalize-quasiquote mode arg)
  (cond ((eq? mode 'quote) (list 'quote arg))
        ((eq? mode 'unquote) arg)
        ((eq? mode 'splice)
         (error ",@~S in illegal context"
                 arg))
        ((fx<= (length arg) *maximum-number-of-arguments*)
         (cons mode arg))
        ((eq? mode 'append)
         (build-quasiquote-list arg 'append 'append 'append))
        (else 
         (build-quasiquote-list arg 'list 'append! mode))))

(define (interesting-to-quasiquote? x marker)
  (and (pair? x)
       (eq? (car x) marker)
       (pair? (cdr x))
       (null? (cddr x))))

(define (build-quasiquote-list arg build join final)
  (iterate loop ((args arg) (len (length arg)) (parts '()))
    (cond ((fx<= len *maximum-number-of-arguments*)
           (do ((ps parts (cdr ps))
                (rs (cons final args) (list join (car ps) rs)))
               ((null? ps) rs)))
          (else
           (receive (part rest)
                    (remove-front-elts args *maximum-number-of-arguments*)
             (loop rest
                   (fx- len *maximum-number-of-arguments*)
                   (cons (cons build part) parts)))))))

(define (remove-front-elts l count)
  (do ((l l (cdr l))
       (e '() (cons (car l) e))
       (i count (fx- i 1)))
      ((fx<= i 0)
       (return (reverse! e) l))))

;; The continuation argument c is passed two values, mode and arg.
;; These are interpreted as follows:
;;    mode    arg          meaning
;;    QUOTE   x            'x
;;    UNQUOTE x            x
;;    LIST    (x1 x2 ...)  (LIST x1 x2 ...)
;;    CONS*   (x1 x2 ...)  (CONS* x1 x2 ...)
;;    APPEND  (x1 x2 ...)  (APPEND x1 x2 ...)

(define (descend-quasiquote x level)
  (cond ((vector? x)
         (descend-quasiquote-vector x level))
        ((not (pair? x))
         (return 'quote x))
        ((interesting-to-quasiquote? x quasiquote-marker)
         (descend-quasiquote-pair x (1+ level)))
        ((interesting-to-quasiquote? x unquote-marker)
         (cond ((= level 0)
                (return 'unquote (cadr x)))
               (else
                (descend-quasiquote-pair x (- level 1)))))
        ((interesting-to-quasiquote? x splice-marker)
         (cond ((= level 0)
                (return 'splice (cadr x)))
               (else
                (descend-quasiquote-pair x (- level 1)))))
        (else
         (descend-quasiquote-pair x level))))

;; It would be simple to make this generate only a correct expansion;
;; most of the complexity here is in order to generate an
;; "optimized" expansion.

(define (descend-quasiquote-pair x level)
  (receive (car-mode car-arg)
           (descend-quasiquote (car x) level)
    (receive (cdr-mode cdr-arg)
             (descend-quasiquote (cdr x) level)
      (cond ((and (eq? car-mode 'quote) (eq? cdr-mode 'quote))
             (return 'quote x))
            ((eq? car-mode 'splice)
             (cond ((and (eq? cdr-mode 'quote) (null? cdr-arg))
                    (return 'unquote
                            car-arg))
                   ((eq? cdr-mode 'append)
                    (return 'append
                            (cons car-arg cdr-arg)))
                   (else
                    (return 'append
                            (list car-arg
                                  (finalize-quasiquote cdr-mode cdr-arg))))))
            ((and (eq? cdr-mode 'quote) (null? cdr-arg))
             (return 'list
                     (list (finalize-quasiquote car-mode car-arg))))
            ((or (eq? cdr-mode 'list) (eq? cdr-mode 'cons*))
             (return cdr-mode
                     (cons (finalize-quasiquote car-mode car-arg)
                           cdr-arg)))
            (else
             (return 'cons*
                     (list (finalize-quasiquote car-mode car-arg)
                           (finalize-quasiquote cdr-mode cdr-arg))))))))

;;;   #(a b c)     ==>  '#(a b c)
;;;   #(a ,b ,c)   ==>  (vector 'a b c)
;;;   #(a ,@b ,c)  ==>  (list->vector (append '(a) b (list c)))
;;; [To do: fix #(a ,b ,c)]

(define (descend-quasiquote-vector x level)
  (receive (mode arg)
           (descend-quasiquote (vector->list x) level)
    (case mode
      ((quote) (return 'quote x))
      ((list)  (return 'vector arg))
      (else    (return 'list->vector
                       (list (finalize-quasiquote mode arg)))))))




