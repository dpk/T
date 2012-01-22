(herald aem68suspend (env tsys (link suspend)))

;;; Look at image.doc and template.doc
(define-local-syntax (dotimes spec . body)
  (let ((index (car spec))
        (limit (cadr spec)))
    `(do ((,index 0 (fx+ ,index 1)))
         ((fx= ,index ,limit))
       ,@body)))


(define (suspend obj out-spec x?)
  (set (experimental?) x?)
  (really-suspend obj out-spec 'image))

(define (vgc-foreign foreign)
  (let ((desc (vgc-extend foreign 1 2)))
    (push (lstate-foreign-reloc *lstate*)
          (cons (symbol->string (foreign-name foreign))
                (fx+ (heap-offset desc) (fx* CELL 2))))
    desc))

(define (generate-slot-relocation obj slot-address)
  (cond ((or (fixnum? obj) (immediate? obj)))
        (else                                               
         (heap-reloc-thunk slot-address (vgc obj)))))

(define (text-relocation addr)
  (push (lstate-text-reloc *lstate*) addr))

(define (data-relocation addr)
  (push (lstate-data-reloc *lstate*) addr))


(define (heap-reloc-thunk slot-address desc)
  (if (eq? (heap-stored desc) (lstate-impure *lstate*))
           (push (lstate-data-reloc *lstate*) slot-address)
           (push (lstate-text-reloc *lstate*) slot-address)))
   

(define (write-slot obj stream)
  (cond ((fixnum? obj)
         (write-fixnum stream obj))
        ((immediate? obj)
         (write-immediate stream obj))
        ((null? obj)
         (write-descriptor (lstate-null *lstate*) stream))
        ((lp-table-entry (lstate-reloc *lstate*) obj)
         => (lambda (desc) (write-descriptor desc stream)))
        (else
         (error "bad immediate type ~s" obj))))

(define-integrable (write-data stream int)
  (write-int stream int))

(define-integrable (write-int stream int)
  (write-half stream (fixnum-ashr int 16))
  (write-half stream int))
                       
(define-integrable (write-immediate stream imm)
  (let ((int (descriptor->fixnum imm)))
    (write-half stream (fixnum-ashr int 14))
    (write-half stream (fx+ (fixnum-ashl int 2) 1))))
                                                     
(define-integrable (write-scratch stream obj i)
  (let ((offset (fixnum-ashl i 2)))
    (write-half stream (mref-16-u obj offset))
    (write-half stream (mref-16-u obj (fx+ offset 2)))))
    
(define-integrable (write-half stream int)
  (vm-write-byte stream (fixnum-ashr int 8))
  (vm-write-byte stream int))

;(define-integrable (write-byte stream n)
;  (writec stream (ascii->char (fixnum-logand n 255))))

(define-integrable (write-fixnum stream fixnum)
  (write-half stream (fixnum-ashr fixnum 14))
  (write-half stream (fixnum-ashl fixnum 2)))

(define (write-link-file stream)
  (write-header     stream)
  (write-out-area   stream (lstate-pure *lstate*))
  (write-out-area   stream (lstate-impure *lstate*))
  (write-relocation stream (lstate-text-reloc *lstate*))
  (write-relocation stream (lstate-data-reloc *lstate*))
  (write-foreign-relocation stream (lstate-foreign-reloc *lstate*)))

(define (write-header stream)
  (let* ((text-size (+area-frontier (lstate-pure *lstate*)))
         (data-size (+area-frontier (lstate-impure *lstate*))))
    (cond ((lp-table-entry (lstate-reloc *lstate*) big_bang)
           => (lambda (desc)
                (write-descriptor desc stream)))  ; entry point
          (else
           (error "big_bang not defined")))
    (write-int stream (fx* (length (lstate-text-reloc *lstate*)) 4))
    (write-int stream (fx* (length (lstate-data-reloc *lstate*)) 4))
    (write-int stream (foreign-reloc-size (lstate-foreign-reloc *lstate*)))
    (write-int stream text-size)
    (write-int stream data-size)))

(define (write-out-area stream area)
  (walk (lambda (x) (write-store x stream))
        (reverse! (+area-objects area))))

(define (write-relocation stream items)
  (walk (lambda (addr) (write-int stream addr)) items))

(define (write-foreign-relocation stream syms)
  (walk (lambda (sym)
          (destructure (((name . addr) sym))
            (write-int stream addr)
            (vm-write-string stream name)
            (dotimes (i (fx- 32 (string-length name)))
              (vm-write-byte stream 32))))
        syms))

(define (foreign-reloc-size syms)  ; syms are (name . addr)
  (fx* (length syms) 36))
