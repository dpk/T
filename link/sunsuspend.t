(herald sunsuspend (env tsys (link suspend)))

;;; Look at a Unix a.out description and template.doc

(define (suspend obj out-spec x?)
  (set (experimental?) x?)
  (really-suspend obj out-spec 'o))

(define-constant RELOC-SIZE 8)
(define-constant CYMBAL-SIZE 12)
(define-constant OMAGIC #o407)
(define-constant N_TEXT 4)
(define-constant N_DATA 6)
(define-constant N_UNDF 0)
(define-constant N_EXT 1)

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
    (reloc-thunk (fixnum-logior (fixnum-ashl (lstate-symbol-count *lstate*) 8)
                                #x50)
                 (fx+ addr 8))
    (modify (lstate-symbol-count *lstate*) (lambda (x) (fx+ x 1)))
    desc))

(define (generate-slot-relocation obj slot-address)
  (cond ((or (fixnum? obj) (immediate? obj)))
        ((eq? (heap-stored (vgc obj)) (lstate-impure *lstate*))
         (reloc-thunk #x640 slot-address))
        (else
         (reloc-thunk #x440 slot-address))))
            

(define (reloc-thunk type address)
  (push (lstate-data-reloc *lstate*)
        (cons address type)))

(define (text-relocation addr)
  (reloc-thunk #x440 addr))

(define (data-relocation addr)
  (reloc-thunk #x640 addr))
        

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


(define (cymbol-thunk stryng type value)
 (push (lstate-symbols *lstate*)
  (object (lambda (stream a)
            ;; a is offset into stryng table
            (write-int stream a)
            (vm-write-byte stream type)
            (vm-write-byte stream 0)       ; other
            (write-half stream 0)       ; see <stab.h>                 
            (if (fixnum? value)            ; undefined external (foreign)
                (write-int stream 0)
                (write-descriptor value stream)))
          ((cymbol-thunk.stryng self) stryng))))

(define-operation (cymbol-thunk.stryng thunk))

(define-integrable (write-data stream int)
  (write-int stream (fx+ (lstate-pure-size *lstate*) int)))

(define (make-global-cymbol proc name)
  (cond ((lp-table-entry (lstate-reloc *lstate*) proc)
       => (lambda (desc)                                
            (cymbol-thunk (string-downcase! (symbol->string name))
                          (fixnum-logior N_DATA N_EXT)
                          desc)
            (modify (lstate-symbol-count *lstate*) (lambda (x) (fx+ x 1)))))
           
      (else
       (error "~s not defined" name))))

                                       
(define (write-link-file stream)
  (make-global-cymbol big_bang 'big_bang)
  (make-global-cymbol interrupt_dispatcher 'interrupt_dispatcher)
  (write-header     stream)
  (write-area       stream (lstate-pure *lstate*))
  (write-area       stream (lstate-impure *lstate*))
  (write-relocation stream (lstate-data-reloc *lstate*))  
  (write-cymbol&stryng-table stream (reverse (lstate-symbols *lstate*))))

(define (write-header stream)
  (let* ((text-size (+area-frontier (lstate-pure *lstate*)))
         (data-size (+area-frontier (lstate-impure *lstate*))))
    (write-half stream 2)                     ; only on mc68020
    (write-half stream OMAGIC)                ;magic number
    (write-int stream text-size)              ;text segment size
    (write-int stream data-size)              ;data segment size
    (write-int stream 0)                      ;bss  segment size
    (write-int stream (fx* CYMBAL-SIZE (lstate-symbol-count *lstate*)))
    (write-int stream 0)                      ;bogus entry point
    (write-int stream 0)                      ; no text relocation
    (write-int stream (fx* (length (lstate-data-reloc *lstate*)) RELOC-SIZE))))

(define (write-area stream area)
  (walk (lambda (x) (write-store x stream))
        (reverse! (+area-objects area))))


(define (write-relocation stream items)
  (walk (lambda (item)
          (write-int stream (car item))
          (write-int stream (cdr item)))
        items))
          
                             
(define (write-cymbol&stryng-table stream cyms)
  (let ((z (write-cyms stream cyms))) ; cymbal table
    (write-int stream z)       ; size of stryng table
    (walk (lambda (s)             ; write stryng table
            (write-string stream (cymbol-thunk.stryng s))
            (vm-write-byte stream 0))
           cyms)))

(define (write-cyms stream cyms)
  (iterate loop ((a 4)                      ;; 4 bytes for size of stryng table
                 (l cyms))
    (cond ((null? l) a)
          (else
           (let ((e (car l)))
             (e stream a)
             (loop (fx+ (fx+ a (string-length (cymbol-thunk.stryng e))) 1) ;null
                   (cdr l)))))))


