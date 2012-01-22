(herald hpsuspend (env tsys (link suspend)))

;;; Look at a Unix a.out description and template.doc

(define (suspend obj out-spec x?)
  (set (experimental?) x?)
  (really-suspend obj out-spec 'o))

(define-constant RELOC-SIZE 8)
(define-constant CYMBOL-SIZE 8)
(define-constant N_TEXT #o2)
(define-constant N_DATA #o3)
(define-constant N_UNDF 0)
(define-constant N_EXT #o40)         
(define-constant R_TEXT (fx+ (fixnum-ashl 0 8) 2))  ; 0 for text, 2 for long
(define-constant R_DATA (fx+ (fixnum-ashl 1 8) 2))  ; 1 for data, 2 for long
(define-constant R_UNDF (fx+ (fixnum-ashl 3 8) 2))  ; 3 for undf, 2 for long

(define (vgc-foreign foreign)
  (let* ((heap (lstate-impure *lstate*))
         (addr (+area-frontier heap))
         (name (foreign-name foreign))
         (desc (object nil
                 ((heap-stored self) (lstate-impure *lstate*))
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-data stream (fx+ addr tag/extend)))
                 ((write-store self stream)
                  (write-int stream header/foreign)
                  (write-slot name stream)
                  (write-int stream 0)))))
    (set (+area-frontier heap) (fx+ addr 12))
    (push (+area-objects heap) desc)
    (set-lp-table-entry (lstate-reloc *lstate*) foreign desc)
    (generate-slot-relocation name (fx+ addr 4))
    (cymbol-thunk (symbol->string name) (fixnum-logior N_UNDF N_EXT) 0)
    (reloc-thunk (fx+ addr 8) (lstate-symbol-count *lstate*) R_UNDF)
    (modify (lstate-symbol-count *lstate*) (lambda (x) (fx+ x 1)))
    desc))

(define (generate-slot-relocation obj slot-address)
  (cond ((or (fixnum? obj) (immediate? obj)))
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


(define (cymbol-thunk string type value)
 (push (lstate-symbols *lstate*)              
   (let ((len (string-length string)))
     (object (lambda (stream)     
               (if (fixnum? value)
                   (write-int stream 0)
                   (write-descriptor value stream))
               (vm-write-byte stream type)
               (vm-write-byte stream len)
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

(define (make-global-cymbol proc name)
  (cond ((lp-table-entry (lstate-reloc *lstate*) proc)
       => (lambda (desc)                                
            (cymbol-thunk (string-downcase! (symbol->string name))
                          (fixnum-logior N_DATA N_EXT)
                          desc)))
      (else
       (error "~s not defined" name))))


(define (write-link-file stream)                 
  (make-global-cymbol big_bang 'big_bang)
  (make-global-cymbol interrupt_dispatcher 'interrupt_dispatcher)
  (write-header     stream)
  (write-area       stream (lstate-pure *lstate*))
  (write-area       stream (lstate-impure *lstate*))
  (write-cymbol-table stream (reverse (lstate-symbols *lstate*)))
  (write-relocation stream (lstate-data-reloc *lstate*)))  

(define (write-header stream)
  (let* ((text-size (+area-frontier (lstate-pure *lstate*)))
         (data-size (+area-frontier (lstate-impure *lstate*))))
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
        (reverse! (+area-objects area))))


(define (write-relocation stream items)
  (walk (lambda (reloc) 
          (write-int stream (car reloc))   ; address
          (write-int stream (cdr reloc)))
        (sort-list! items 
                    (lambda (x y)      
                      (fx< (car x) (car y))))))

                             
(define (write-cymbol-table stream cyms)
  (walk (lambda (thunk) (thunk stream)) cyms))


