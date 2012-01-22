(herald aem68link (env t (link defs)))

;;; Look at image.doc and template.doc


(define-local-syntax (dotimes spec . body)
  (let ((index (car spec))
        (limit (cadr spec)))
    `(do ((,index 0 (fx+ ,index 1)))
         ((fx= ,index ,limit))
       ,@body)))

;(define (link modules out-spec)
;  (with-open-streams
;   ((image (open (filename-with-type filename 'apollo_image) 'out))
;    (map   (open (filename-with-type filename 'apollo_map) 'out))
;    (noise (open (filename-with-type filename 'apollo_noise) 'out)))
;   (bind ((*linker-noise-file* (make-broadcast-stream (standard-output)
;                                                      noise-file)))
;     (linker-message "~&Linking ~a ... ~%" out-spec)
;     (really-link modules image map noise))))

(define (link modules out-spec)
  (really-link modules 'mo out-spec 'image))

(define-constant %%d-ieee-size 53)
(define-constant %%d-ieee-excess 1023)

(define (write-double-float stream float)
  (receive (sign mantissa exponent)
           (normalized-float-parts float
                                   %%d-ieee-size 
                                   %%d-ieee-excess 
                                   t)
    (write-int stream header/double-float)
    (write-half stream (fx+ (fixnum-ashl sign 15)
                            (fx+ (fixnum-ashl exponent 4)
                                 (bignum-bit-field mantissa 48 4))))
    (write-half stream (bignum-bit-field mantissa 32 16)) 
    (write-half stream (bignum-bit-field mantissa 16 16)) 
    (write-half stream (bignum-bit-field mantissa 0 16))))
  
(define (write-vcell-header var stream)
  (write-half stream 0)
  (write-byte stream (if (fx= (vector-length (var-node-refs var))
			      0)
			 0
			 -1))
  (write-byte stream (if (eq? (var-node-defined var) 'define)
			 (fx+ header/vcell 128)
			 header/vcell)))

(define (write-template stream tmplt)
  (write-byte stream (cit-pointer tmplt))
  (write-byte stream (cit-scratch tmplt))
  (write-half stream (cit-unit-offset tmplt))
  (write-byte stream (cit-header/nary? tmplt))
  (write-byte stream (cit-nargs tmplt))
  (write-half stream M68-JUMP-ABSOLUTE)
  (write-int  stream 
              (fx+ (heap-offset (table-entry *reloc-table* (cit-code-vec tmplt)))
                          (fx+ CELL (cit-aux-offset tmplt))))) ;; for header


;;; fetch the template store slots out of the closure-internal-template's
;;; auxiliary template.                  

(define (set-template-store-slots ts code index offset)
  (set (cit-unit-offset ts) (fx* (fx+ offset 1) CELL))
  (set (cit-pointer ts) (bref-8 code (fx- index 6)))
  (set (cit-scratch ts) (bref-8 code (fx- index 5)))
  (set (cit-nargs ts)   (bref-8 code (fx- index 1)))
  (set (cit-header/nary? ts) (bref-8 code (fx- index 2)))
  (set (cit-code-vec ts) code)
  (set (cit-aux-offset ts) index))

(define (vgc-copy-foreign foreign)
  (let* ((heap (lstate-impure *lstate*))
         (addr (area-frontier heap))
         (name (foreign-object-name foreign))
         (desc (object nil
                 ((heap-stored self) (lstate-impure *lstate*))
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-data stream (fx+ addr tag/extend)))
                 ((write-store self stream)
                  (write-int stream header/foreign)
                  (write-slot name stream)
                  (write-int stream 0)))))
    (set (area-frontier heap) (fx+ addr (fx* CELL 3)))
    (set-table-entry *reloc-table* foreign desc)
    (push (area-objects heap) desc)                
    (generate-slot-relocation name (fx+ addr CELL))
    (push (lstate-foreign-reloc *lstate*)
          (cons (symbol->string name)
                (fx+ addr (fx* CELL 2))))
    desc))


(define (relocate-unit-variable var addr external?)
  (let ((type (var-value-type var)))
   (cond (type
    (cond ((and external? (neq? (var-node-value var) NONVALUE))
           (push (lstate-symbols *lstate*)
                 (cons (var-node-name var) (unit-var-value (var-node-value var))))
           (reloc-thunk type addr))
          (else
           (reloc-thunk type addr)))))))

(define (var-value-type var)
  (let ((value (var-node-value var)))
    (cond ((eq? value NONVALUE) 
           (vgc (var-node-name var))
           nil)
          ((unit-loc? value) 'DATA)
          (else
           (let ((desc (vgc value)))
             (if (eq? (heap-stored desc) (lstate-impure *lstate*))
                 'DATA                                                                
                 'TEXT))))))
        
(define (generate-slot-relocation obj slot-address)
  (cond ((or (fixnum? obj) (char? obj) (eq? obj '#t)))
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
   

(define (reloc-thunk type slot-address)
  (if (eq? type 'data)
      (push (lstate-data-reloc *lstate*) slot-address)
      (push (lstate-text-reloc *lstate*) slot-address)))


(define (write-slot obj stream)
  (cond ((table-entry *reloc-table* obj)
         => (lambda (desc) (write-descriptor desc stream)))
        ((fixnum? obj)
         (write-fixnum stream obj))
        ((char? obj)
         (write-int stream (fx+ (fixnum-ashl (char->ascii obj) 8)
                                 header/char)))
        ((eq? obj '#t)
         (write-int stream header/true))
        (else
         (error "bad immediate type ~s" obj))))

(define (write-data stream int)
  (write-int stream int))

(define-integrable (write-int stream int)
  (write-half stream (fixnum-ashr int 16))
  (write-half stream int))

(define (write-half stream int)
  (write-byte stream (fixnum-ashr int 8))
  (write-byte stream int))

(define-integrable (write-byte stream n)
  (writec stream (ascii->char (fixnum-logand n 255))))
                                 
(define-integrable (write-fixnum stream fixnum)
  (write-half stream (fixnum-ashr fixnum 14))
  (write-half stream (fixnum-ashl fixnum 2)))

(define (write-link-file stream)
  (write-header     stream)
  (write-area       stream (lstate-pure *lstate*))
  (write-area       stream (lstate-impure *lstate*))
  (write-relocation stream (lstate-text-reloc *lstate*))
  (write-relocation stream (lstate-data-reloc *lstate*))
  (write-foreign-relocation stream (lstate-foreign-reloc *lstate*)))

(define (write-header stream)
  (let* ((text-size (area-frontier (lstate-pure *lstate*)))
         (data-size (area-frontier (lstate-impure *lstate*))))
    (cond ((assq 'big_bang (lstate-symbols *lstate*))
           => (lambda (pair)
                (write-int stream (cdr pair)))) ; entry point
          (else
           (error "big_bang not defined")))
    (write-int stream (fx* (length (lstate-text-reloc *lstate*)) 4))
    (write-int stream (fx* (length (lstate-data-reloc *lstate*)) 4))
    (write-int stream (foreign-reloc-size (lstate-foreign-reloc *lstate*)))
    (write-int stream text-size)
    (write-int stream data-size)))

(define (write-area stream area)
  (walk (lambda (x) (write-store x stream))
        (reverse! (area-objects area))))


(define (write-relocation stream items)
  (walk (lambda (addr) (write-int stream addr)) (sort-list! items fx<)))
                             
(define (write-map-entry stream name value)
  (if (unit-loc? value)
      (format stream "~s~20t~x~%" name 
         (fx+ (heap-offset (table-entry *reloc-table* (unit-loc-unit value)))
              (fx+ (unit-loc-offset value) tag/extend)))))
                                

(define (write-foreign-relocation stream syms)
  (walk (lambda (sym)
          (destructure (((name . addr) sym))
            (write-int stream addr)
            (write-string stream name)
            (dotimes (i (fx- 32 (string-length name)))
              (write-byte stream 32))))
        syms))

(define (foreign-reloc-size syms)  ; syms are (name . addr)
  (fx* (length syms) 36))
