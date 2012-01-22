(herald aegis_float (env tsys))

(define-integrable (make-flonum)
  (make-vector-extend header/double-float 2 2 ))

;++ this should be written in T.

(define (kludgy-string->flonum s)
  (let ((s (if (iob? s) (buffer->string! s) s))
        (n (make-flonum))
        (b (get-string-buffer)))
    (set (string-length b) (text-length (string-text b)))
    (string-fill b #\space)
    (string-replace b s (string-length s))
    (t_$atod b n)
    (release-string-buffer b)
    n))

(define-foreign t_$atod
  ("T_$ATOD" (in rep/string)
           (out rep/extend))
    ignore)

;; expects a string of length at least 23

;++ flush
;(define-integrable (kludgy-flonum->string! n s)
;  (t_$dtoa s n)
;  s)

(define-foreign t_$dtoa
  ("T_$DTOA" (out rep/string)
           (in rep/extend))
    ignore)

(define (print-flonum-kludgily n stream)
  (let ((b (get-string-buffer)))
    (set (string-length b) 23)
    (string-fill b #\space)
    (t_$dtoa b n)
    (write-string stream b)
    (release-string-buffer b)
    (no-value)))

(define (*define-fl-proc-1 xenoid id)
  (object (lambda (x)
            (let ((x (enforce double-float? x))
                  (result (make-flonum)))
              (xenoid x result)
              result))
          ((identification self) id)))

(define (*define-fl-proc-2 xenoid id)
  (object (lambda (x y)
            (let ((x (enforce double-float? x))
                  (y (enforce double-float? y))
                  (result (make-flonum)))
              (xenoid x y result)
              result))
          ((identification self) id)))

(define (*define-fl-proc-3 xenoid id)
  (object (lambda (x y)
            (let ((x (enforce double-float? x))
                  (y (enforce double-float? y)))
              (fixnum-odd? (xenoid x y))))
          ((identification self) id)))
                    
(define-local-syntax (define-fl-proc-1 name)
  (let ((xeno-name (concatenate-symbol 't_$ name)))
    `(block (define-foreign ,xeno-name 
              (,(string-upcase! (symbol->string xeno-name)) (in rep/extend)
                          (out rep/extend))
              ignore)
            (define ,name (*define-fl-proc-1 ,xeno-name ',name)))))
  

(define-local-syntax (define-fl-proc-2 name)
  (let ((xeno-name (concatenate-symbol 't_$fl name))
        (t-name (concatenate-symbol 'flonum- name)))
    `(block (define-foreign ,xeno-name 
              (,(string-upcase! (symbol->string xeno-name)) (in rep/extend)
                          (in rep/extend)
                          (out rep/extend))
              ignore)
            (define ,t-name (*define-fl-proc-2 ,xeno-name ',t-name)))))
  
(define-local-syntax (define-fl-proc-3 name)
  (let ((xeno-name (concatenate-symbol 't_$fl name))
        (t-name (concatenate-symbol 'flonum- name '?)))
    `(block (define-foreign ,xeno-name 
              (,(string-upcase! (symbol->string xeno-name)) (in rep/extend)
                          (in rep/extend))
              rep/integer)
            (define ,t-name (*define-fl-proc-3 ,xeno-name ',t-name)))))

(define-fl-proc-1 sin)
(define-fl-proc-1 cos)
(define-fl-proc-1 tan)
;(define-fl-proc-1 asin)
;(define-fl-proc-1 acos)
(define-fl-proc-1 atan)
(define-fl-proc-1 exp)
(define-fl-proc-1 log)
(define-fl-proc-1 sqrt)

(define (asin n)
  (error "asin is not yet implemented in aegis t"))
(define (acos n)
  (error "acos is not yet implemented in aegis t"))

;;; ... also need power and atan2


(define-fl-proc-2 add)
(define-fl-proc-2 subtract)
(define-fl-proc-2 multiply)
(define-fl-proc-2 divide)

(define (fl+! x y)
  (t_$fladd x y x))

(define (fl-! x y)
  (t_$flsubtract x y x))

(define (fl*! x y)
  (t_$flmultiply x y x))

(define (fl/! x y)
  (t_$fldivide x y x))

(define-fl-proc-3 less)
(define-fl-proc-3 equal)
(define-fl-proc-3 greater)


(define (flonum-not-equal? a b) (not (flonum-equal? a b)))
(define (flonum-not-less? a b) (not (flonum-less? a b)))
(define (flonum-not-greater? a b) (not (flonum-greater? a b)))
                 
(define (fixnum->flonum fx)
  (let ((fx (enforce fixnum? fx))
        (result (make-flonum)))
    (t_$float fx result)
    result))
           
(define-foreign t_$float 
  ("T_$FLOAT" (in rep/integer)
            (out rep/extend))
    ignore)

(define (flonum->fixnum fl)
  (let ((fl (enforce double-float? fl)))
    (t_$fix fl)))

(define-foreign t_$fix
  ("T_$FIX" (in rep/extend))
    rep/integer)

(define-constant fl+  flonum-add)
(define-constant fl-  flonum-subtract)
(define-constant fl*  flonum-multiply)
(define-constant fl/  flonum-divide)
(define-constant fl=  flonum-equal?)
(define-constant fl<  flonum-less?)
(define-constant fl>  flonum-greater?)
(define-constant fln= flonum-not-equal?)
(define-constant fl>= flonum-not-less?)
(define-constant fl<= flonum-not-greater?)
