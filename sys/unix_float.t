(herald unix_float (env tsys))

(define-integrable (make-flonum)
  (make-vector-extend header/double-float 0 2 ))

(define (kludgy-string->flonum s)
  (let ((n (make-flonum))
        (b (get-string-buffer-of-size 50)))
    (set (string-length b) 50)
    (string-fill b #\space)
    (string-replace b s (string-length s))
    (sscanf b "%F" n)
    (release-string-buffer b)
    n))

(define-foreign sscanf (sscanf (in rep/string)
                               (in rep/string)
                               (in rep/extend))   ; pointer to double
  rep/undefined)
                                               
(define (print-flonum-kludgily n stream)
  (let ((b (get-string-buffer-of-size 50)))       
    (set (string-length b) 50)
    (sprintf b "%e!" n)
    (set (string-length b) (string-posq #\! b))
    (write-string stream b)
    (release-string-buffer b)
    (no-value)))

(define-foreign sprintf (sprintf (in rep/string)
                                 (in rep/string)
                                 (in rep/double))
  rep/undefined)

(define (*define-fl-proc-1 xenoid id)
  (object (lambda (x)
            (xenoid (enforce double-float? x)))
          ((identification self) id)))

(define (*define-fl-proc-3 xenoid id)
  (object (lambda (x y)
            (let ((x (enforce double-float? x))
                  (y (enforce double-float? y)))
              (fixnum-odd? (xenoid x y))))
          ((identification self) id)))
                    
(define-local-syntax (define-fl-proc-1 name)
  (let ((xeno-name (concatenate-symbol 'fl name)))
    `(block (define-foreign ,xeno-name (,name (in rep/double))
              rep/double)
            (define ,name (*define-fl-proc-1 ,xeno-name ',name)))))
  

(define-local-syntax (define-fl-proc-3 name)
  (let ((xeno-name (concatenate-symbol 'fl name))
        (t-name (concatenate-symbol 'flonum- name '?)))
    `(block (define-foreign ,xeno-name 
              (,xeno-name (in rep/double)
                          (in rep/double))
              rep/integer)
            (define ,t-name (*define-fl-proc-3 ,xeno-name ',t-name)))))
  
(define-foreign %flonum-add (fladd (in rep/extend) (in rep/extend) (in rep/extend))
  ignore)

(define-foreign %flonum-subtract (flsubtract (in rep/extend) (in rep/extend) (in rep/extend))
  ignore)

(define-foreign %flonum-multiply (flmultiply (in rep/extend) (in rep/extend) (in rep/extend))
  ignore)

(define-foreign %flonum-divide (fldivide (in rep/extend) (in rep/extend) (in rep/extend))
  ignore)

(define (make-flonum-binop proc)
  (lambda (x y)
    (let ((x (enforce double-float? x))
	  (y (enforce double-float? y))
	  (z (make-flonum)))
      (proc z x y)
      z)))

(define flonum-add  (make-flonum-binop %flonum-add))
(define flonum-subtract  (make-flonum-binop %flonum-subtract))
(define flonum-multiply  (make-flonum-binop %flonum-multiply))
(define flonum-divide  (make-flonum-binop %flonum-divide))

(define (fl+! x y)
  (%flonum-add x x y)
  x)

(define (fl-! x y)
  (%flonum-subtract x x y)
  x)


(define (fl*! x y)
  (%flonum-multiply x x y)
  x)


(define (fl/! x y)
  (%flonum-divide x x y)
  x)


(define-fl-proc-1 sin)
(define-fl-proc-1 cos)
(define-fl-proc-1 tan)
(define-fl-proc-1 asin)
(define-fl-proc-1 acos)
(define-fl-proc-1 atan)
(define-fl-proc-1 exp)
(define-fl-proc-1 log)
(define-fl-proc-1 sqrt)


;;; ... also need power and atan2

(define-fl-proc-3 less)
(define-fl-proc-3 equal)
(define-fl-proc-3 greater)


(define (flonum-not-equal? a b) (not (flonum-equal? a b)))
(define (flonum-not-less? a b) (not (flonum-less? a b)))
(define (flonum-not-greater? a b) (not (flonum-greater? a b)))
                 
(define-foreign float 
  (flote (in rep/integer))   ; losing C reserved words
    rep/double)

(define (fixnum->flonum fx)
  (float (enforce fixnum? fx)))
           
(define-foreign fix
  (fix (in rep/double))
    rep/integer)

(define (flonum->fixnum fl)
  (fix (enforce double-float? fl)))

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


