(herald unvaxlink (env t (link defs)))

;;; Look at a Unix a.out description and template.doc

(define (link modules out-spec)
  (really-link modules 'vo out-spec 'o))

(define-constant RELOC-SIZE 8)
(define-constant CYMBAL-SIZE 12)
(define-constant OMAGIC #o407)
(define-constant N_TEXT 4)
(define-constant N_DATA 6)
(define-constant N_UNDF 0)
(define-constant N_EXT 1)

(define-constant DATA-RELOC (fixnum-logior N_DATA (fixnum-ashl 2 25)))
(define-constant TEXT-RELOC (fixnum-logior N_TEXT (fixnum-ashl 2 25)))
(define-constant UNDEFINED-RELOC (fixnum-logior (fixnum-ashl 2 25)
                                                (fixnum-ashl 1 27)))
(define-constant DATA-EXTERNAL (fixnum-logior DATA-RELOC N_EXT))
(define-constant TEXT-EXTERNAL (fixnum-logior TEXT-RELOC N_EXT))

(define-constant %%vax-d-size 56)
(define-constant %%vax-d-excess 128)

(define (write-double-float stream float)
  (receive (sign mantissa exponent)
           (normalized-float-parts float
                                   %%vax-d-size 
                                   %%vax-d-excess 
                                   t)
    (write-int stream header/double-float)
    (write-half stream (fx+ (fixnum-ashl sign 15)
                            (fx+ (fixnum-ashl exponent 7)
                                 (bignum-bit-field mantissa 48 7))))
    (write-half stream (bignum-bit-field mantissa 32 16)) 
    (write-half stream (bignum-bit-field mantissa 16 16)) 
    (write-half stream (bignum-bit-field mantissa 0 16))))

(define (write-vcell-header var stream)
  (write-byte stream (if (eq? (var-node-defined var) 'define)
			 (fx+ header/vcell 128)
			 header/vcell))
  (write-byte stream 0)
  (write-byte stream (if (fx= (vector-length (var-node-refs var))
			      0)
			 0
			 -1))
  (write-byte stream 0))



(define (write-template stream tmplt)
  (write-half stream (cit-unit-offset tmplt))
  (write-byte stream (cit-scratch tmplt))
  (write-byte stream (cit-pointer tmplt))
  (write-byte stream (cit-header/nary? tmplt))
  (write-byte stream (cit-nargs tmplt))
  (write-half stream VAX-JUMP-ABSOLUTE)
  (write-int  stream 
              (fx+ (heap-offset (table-entry *reloc-table* (cit-code-vec tmplt)))
                          (fx+ CELL (cit-aux-offset tmplt))))) ;; for header


;;; fetch the template store slots out of the closure-internal-template's
;;; auxiliary template.                  

(define (set-template-store-slots ts code index offset)
  (set (cit-unit-offset ts) (fx* (fx+ offset 1) CELL))
  (set (cit-pointer ts) (bref-8 code (fx- index 3)))
  (set (cit-scratch ts) (bref-8 code (fx- index 4)))
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
    (set (area-frontier heap) (fx+ addr 12))
    (set-table-entry *reloc-table* foreign desc)
    (generate-slot-relocation name (fx+ addr 4))
    (push (area-objects heap) desc)                
    (cymbal-thunk (symbol->string name) (fixnum-logior N_UNDF N_EXT) 0)
    (reloc-thunk (fixnum-logior (lstate-symbol-count *lstate*) UNDEFINED-RELOC)
                 (fx+ addr 8))
    (modify (lstate-symbol-count *lstate*) (lambda (x) (fx+ x 1)))
    desc))

(define (relocate-unit-variable var addr external?)
  (let ((area (lstate-impure *lstate*))
        (type (var-value-type var)))
   (cond (type
    (cond ((and external? (neq? (var-node-value var) NONVALUE))
           (cymbal-thunk (string-downcase! (symbol->string (var-node-name var)))
                         (fixnum-logior type N_EXT)
                         (unit-var-value (var-node-value var)))
           (modify (lstate-symbol-count *lstate*) (lambda (x) (fx+ x 1)))))
    (if (fx= type N_DATA)
        (reloc-thunk DATA-EXTERNAL addr)
        (reloc-thunk TEXT-EXTERNAL addr))))))



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
         (reloc-thunk DATA-RELOC slot-address))
        (else
         (reloc-thunk TEXT-RELOC slot-address))))

(define (text-relocation addr)
  (reloc-thunk TEXT-RELOC addr))

(define (data-relocation addr)
  (reloc-thunk DATA-RELOC addr))

(define (reloc-thunk type address)
  (push (lstate-data-reloc *lstate*)
        (cons address type)))

(define (cymbal-thunk stryng type value)
 (push (lstate-symbols *lstate*)
  (object (lambda (stream a)
            ;; a is offset into stryng table
            (linker-debug "  Cymbal ~s, type ~s, value ~s, offset ~s"
                           stryng type value a)
            (write-int stream a)
            (write-byte stream type)
            (write-byte stream 0)       ; other
            (write-half stream 0)       ; see <stab.h>                 
            (if (fx= type 1)            ; undefined external (foreign)
                (write-int stream 0)
                (write-data stream value)))
          ((cymbal-thunk.stryng self) stryng))))

(define-operation (cymbal-thunk.stryng thunk))


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

(define-integrable (write-data stream int)
  (write-int stream (fx+ (lstate-pure-size *lstate*) int)))

(define (write-int stream int)
  (write-half stream int)
  (let ((int (fixnum-ashr int 16)))
    (write-half stream int)))

(define (write-half stream int)
  (write-byte stream int)
  (let ((int (fixnum-ashr int 8)))
    (write-byte stream int)))

(define-integrable (write-byte stream n)
  (writec stream (ascii->char (fixnum-logand n 255))))

(define (write-fixnum stream fixnum)
  (write-half stream (fixnum-ashl fixnum 2))
  (write-half stream (fixnum-ashr fixnum 14)))


(define (write-link-file stream)
  (write-header     stream)
  (write-area       stream (lstate-pure *lstate*))
  (write-area       stream (lstate-impure *lstate*))
  (write-relocation stream (lstate-data-reloc *lstate*))  
  (write-cymbal&stryng-table stream (reverse (lstate-symbols *lstate*))))

(define (write-header stream)
  (let* ((text-size (area-frontier (lstate-pure *lstate*)))
         (data-size (area-frontier (lstate-impure *lstate*))))
    (write-int stream OMAGIC)                 ;magic number
    (write-int stream text-size)              ;text segment size
    (write-int stream data-size)              ;data segment size
    (write-int stream 0)                      ;bss  segment size
    (write-int stream (fx* CYMBAL-SIZE (lstate-symbol-count *lstate*)))
    (write-int stream 0)                      ;bogus entry point
    (write-int stream 0)                      ; no text relocation
    (write-int stream (fx* (length (lstate-data-reloc *lstate*)) RELOC-SIZE))))

(define (write-area stream area)
  (walk (lambda (x) (write-store x stream))
        (reverse! (area-objects area))))


(define (write-relocation stream items)
  (walk (lambda (item)
          (write-int stream (car item))
          (write-int stream (cdr item)))
        items))
          
                             
(define (write-map-entry stream name value) nil)

(define (write-cymbal&stryng-table stream cyms)
  (let ((z (write-cyms stream cyms))) ; cymbal table
    (linker-debug "Stryng table (size = ~s)" z)
    (write-int stream z)       ; size of stryng table
    (walk (lambda (s)             ; write stryng table
            (linker-debug "  ~s" (cymbal-thunk.stryng s))
            (write-string stream (cymbal-thunk.stryng s))
            (write-byte stream 0))
           cyms)))

(define (write-cyms stream cyms)
  (linker-debug "Cymbal table~%")
  (iterate loop ((a 4)                      ;; 4 bytes for size of stryng table
                 (l cyms))
    (cond ((null? l) a)
          (else
           (let ((e (car l)))
             (e stream a)
             (loop (fx+ (fx+ a (string-length (cymbal-thunk.stryng e))) 1) ;null
                   (cdr l)))))))


