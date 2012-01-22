(herald hp300link (env t (link defs)))

;;; Look at a Unix a.out description and template.doc

(define (link modules out-spec)
  (really-link modules 'mo out-spec 'o))

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


(define-constant RELOC-SIZE 8)
(define-constant CYMBOL-SIZE 8)
(define-constant N_TEXT #o2)
(define-constant N_DATA #o3)
(define-constant N_UNDF 0)
(define-constant N_EXT #o40)         
(define-constant R_TEXT (fx+ (fixnum-ashl 0 8) 2))  ; 0 for text, 2 for long
(define-constant R_DATA (fx+ (fixnum-ashl 1 8) 2))  ; 1 for data, 2 for long
(define-constant R_UNDF (fx+ (fixnum-ashl 3 8) 2))  ; 3 for undf, 2 for long

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
    (set (area-frontier heap) (fx+ addr 12))
    (set-table-entry *reloc-table* foreign desc)
    (generate-slot-relocation name (fx+ addr 4))
    (push (area-objects heap) desc)                
    (cymbol-thunk (symbol->string name) (fixnum-logior N_UNDF N_EXT) 0)
    (reloc-thunk (fx+ addr 8) (lstate-symbol-count *lstate*) R_UNDF)
    (modify (lstate-symbol-count *lstate*) (lambda (x) (fx+ x 1)))
    desc))

(define (relocate-unit-variable var addr external?)
  (let ((type (var-value-type var)))
   (cond (type
    (cond ((and external? (neq? (var-node-value var) NONVALUE))
           (cymbol-thunk (string-downcase! (symbol->string (var-node-name var)))
                         (fixnum-logior type N_EXT)
                         (unit-var-value (var-node-value var)))
           (modify (lstate-symbol-count *lstate*) (lambda (x) (fx+ x 1)))))
    (if (fx= type N_DATA)
        (reloc-thunk addr 0 R_DATA)
        (reloc-thunk addr 0 R_TEXT))))))



(define (var-value-type var)
  (let ((value (var-node-value var)))
    (cond ((eq? value NONVALUE) 
           (vgc (var-node-name var))
           nil)
          ((unit-loc? value) N_DATA)
          (else
           (let ((desc (vgc value)))
             (if (eq? (heap-stored desc) (lstate-impure *lstate*))
                 N_DATA                                                                
                 N_TEXT))))))

(define (generate-slot-relocation obj slot-address)
  (cond ((or (fixnum? obj) (char? obj) (eq? obj '#t)))
        ((eq? (heap-stored (vgc obj)) (lstate-impure *lstate*))
         (reloc-thunk slot-address 0 R_DATA))
        (else
         (reloc-thunk slot-address 0 R_TEXT))))
            
(define (text-relocation addr)
  (reloc-thunk addr 0 R_TEXT))

(define (data-relocation addr)
  (reloc-thunk addr 0 R_DATA))

(define (reloc-thunk address symbolnum type)
  (push (lstate-data-reloc *lstate*) 
        (cons address (fx+ (fixnum-ashl symbolnum 16) type))))

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

(define (cymbol-thunk string type value)
 (push (lstate-symbols *lstate*)              
   (let ((len (string-length string)))
     (object (lambda (stream)     
               (if (fx= value 0)
                   (write-int stream 0)
                   (write-data stream value))
               (write-byte stream type)
               (write-byte stream len)
               (write-int stream 0)
               (write-string stream string))
             ((cymbol-thunk.length self) len)))))

(define-operation (cymbol-thunk.length thunk))

(define (compute-cymbol-table-size)
  (do ((cyms (lstate-symbols *lstate*) (cdr cyms))
       (size 0 (fx+ size (fx+ 10 (cymbol-thunk.length (car cyms))))))
      ((null? cyms) size)))                                            


(define-integrable (write-data stream int)
  (write-int stream (fx+ (lstate-pure-size *lstate*) int)))

(define (write-zeroes stream n)
  (do ((i 0 (fx+ i 1)))
      ((fx= i n) t)
    (write-int stream 0)))

(define (write-link-file stream)
  (write-header     stream)
  (write-area       stream (lstate-pure *lstate*))
  (write-area       stream (lstate-impure *lstate*))
  (write-cymbol-table stream (reverse (lstate-symbols *lstate*)))
  (write-relocation stream (lstate-data-reloc *lstate*)))  

(define (write-header stream)
  (let* ((text-size (area-frontier (lstate-pure *lstate*)))
         (data-size (area-frontier (lstate-impure *lstate*))))
    (write-half stream #x20C)                 ; system-id
    (write-half stream #x106)                 ; file format
    (write-zeroes stream 2)
    (write-int stream text-size)              ;text segment size
    (write-int stream data-size)              ;data segment size
    (write-zeroes stream 2)                      ;bss  segment size
    (write-int stream (fx* (length (lstate-data-reloc *lstate*)) RELOC-SIZE))
    (write-int stream 0)
    (write-int stream (compute-cymbol-table-size))
    (write-zeroes stream 6))) 

(define (write-area stream area)
  (walk (lambda (x) (write-store x stream))
        (reverse! (area-objects area))))


(define (write-relocation stream items)
  (walk (lambda (reloc) 
          (write-int stream (car reloc))   ; address
          (write-int stream (cdr reloc)))
        (sort-list! items 
                    (lambda (x y)      
                      (fx< (car x) (car y))))))


                             
(define (write-map-entry stream name value) 
  (ignore stream name value))

(define (write-cymbol-table stream cyms)
  (walk (lambda (thunk) (thunk stream)) cyms))


