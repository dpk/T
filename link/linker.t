(herald linker
  (env t (link defs))); (osys load_comex)))

;;; This is all straightforward except for the 'linker magic'  i.e. the
;;; bootstrap symbols.  These are variables which are needed by the image
;;; being built (referenced in modules being linked) but which cannot be
;;; defined in any single module.  After all other references have been resolved
;;; the linker pretends that these variables have been defined by filling
;;; the references with its own data structures.

(import t-implementation-env make-table-with-size iob? vm-write-char
	read-comex-from-file symbol-length symbol-hash %%symbol-text-offset
	double-float?)
	


(define-local-syntax (dotimes spec . body)
  (let ((index (car spec))
        (limit (cadr spec)))
    `(do ((,index 0 (fx+ ,index 1)))
         ((fx= ,index ,limit))
       ,@body)))

(lset *null-descriptor* nil)
(lset *symbols* nil)          
(lset *boot-env* nil)
(lset *lstate* nil)
(lset *var-table* nil)
(lset *reloc-table* nil)
(lset *linker-noise-file* nil)

(define (really-link modules obj-type out-spec out-type)                                             
  (linker-message "~&Linking ~a ... ~%" out-spec)
  (bind ((*null-descriptor* nil)
         (*symbols* '())
         (*boot-env* '())
         (*lstate* (create-lstate))
         (*var-table* (make-table-with-size 2000 'linker-var-table))
         (*reloc-table* (make-table-with-size 16000 'linker-reloc-table)))
    (let ((comex-list 
           (map (lambda (file) 
                  (read-comex-from-file (filename-with-type (->filename file) 
                                                            obj-type)))
                modules)))
      (linker-message "~&resolving modules~%")
      (let* ((units (linker-resolve comex-list))
             (unit-vec (list->vector units))
             (filename (->filename out-spec)))
        (define-null-descriptor (lstate-impure *lstate*))
        (relocate-units units unit-vec)
        (patch-in-definitions unit-vec 
                              (map cons (map comex-code comex-list) units))
        (with-open-ports 
         ((image (open (filename-with-type filename out-type) '(out)))
          (map  (open (filename-with-type filename 'map) '(out))))
         (linker-message "~&writing object file~%")
         (set (lstate-pure-size *lstate*) 
              (area-frontier (lstate-pure *lstate*)))
         (table-walk *var-table* 
                     (lambda (name node)
                       (cond ((not (var-node-defined node))
                              (warning "undefined global ~S" name))
                             (else
                              (write-map-entry map name (var-node-value node))))))
         (write-link-file image)
         *lstate*)))))

(define (linker-resolve comi)
  (do ((comi comi (cdr comi))
       (units '() (cons (instantiate-comex (car comi)) units)))
      ((null? comi)
       (reverse! units))))

(define (instantiate-comex comex)
  (let* ((objects (comex-objects comex))
         (opcodes (comex-opcodes comex))
         (code (comex-code comex))
         (unit-len (vector-length objects))
         (unit (make-vector (fx+ unit-len 1))))
    (do ((i 1 (fx+ i 1)))
        ((fx> i unit-len)
         unit)
      (let ((ob (vref objects (fx- i 1))))
        (xselect (bref opcodes (fx- i 1))
          ((op/literal)
           (set (vref unit i) ob))
          ((op/foreign)
           (set (vref unit i)
                (cond ((mem (lambda (x y) (eq? x (foreign-object-name y)))
                            ob
                            (lstate-foreign *lstate*))
                       => car)
                      (else
                       (let ((new (make-foreign-object)))
                         (set (foreign-object-name new) ob)
                         (push (lstate-foreign *lstate*) new)
                         new)))))
          ((op/closure)
           (let ((new (make-templat)))
             (set (templat-code-vec new) code)
             (set (templat-offset new) ob)  
             (set (vref unit i) new)))
          ((op/template1)
           (let ((new (make-cit)))
             (set-template-store-slots new code ob i)
             (set (vref unit i) new)))
          ((op/template2) (set (vref unit i) no-op))
          ((op/template3) (set (vref unit i) no-op))
          ((op/vcell-stored-definition)
           (let ((v (get-vcell (car ob) 'define unit i)))
             (set (var-node-value (vcell-struct-var v))
		  (create-unit-loc unit (cdr ob)))
             (set (vref unit i) v)))
          ((op/vcell-defined)                
           (set (vref unit i) (get-vcell ob 'define unit i)))
          ((op/vcell-lset)           
           (set (vref unit i) (get-vcell ob 'lset unit i)))
          ((op/vcell)
           (set (vref unit i) (get-vcell ob nil unit i)))
          ((op/variable-value)
           (set (vref unit i) (add-to-var-refs ob unit i))))))))

(define (cons-a-var-node name)
  (let ((var (make-var-node))
	(vcell (make-vcell-struct)))
    (set (var-node-name var) name)
    (set-table-entry *var-table* name var)
    (set (vcell-struct-var vcell) var)
    (set (var-node-vcell var) vcell)
    (push *boot-env* (cons name vcell))
    var))
                     
    
(define (add-to-var-refs name unit index)
  (let ((node (cond ((table-entry *var-table* name))
                    (else (cons-a-var-node name)))))
    (push (var-node-refs node) (cons unit (fx- index 1))) ; unit is closure
    node))                                         

(define (get-vcell name definer unit index)
  (let ((node (cond ((table-entry *var-table* name))
                    (else (cons-a-var-node name)))))
    (cond (definer                                        ; not vector
           (if (var-node-defined node) (warning "~S multiply defined" name))
           (set (var-node-defined node) definer)))
    (push (var-node-vcell-refs node) (cons unit (fx- index 1))) ; unit is closure
    (var-node-vcell node)))


(define-constant BOOTSTRAP-SYMBOLS 
  '(*boot-env* 
    *the-initial-symbols* 
    *the-slink*
    *the-initial-modules*
    *code-unit-map*))

;;; these better not get called

(define (patch-in-definitions unit-vec code-unit-map) 
  (patch '*the-initial-modules* unit-vec)
  (patch '*code-unit-map* code-unit-map)
  (patch '*the-slink* nil)
  (patch '*the-initial-symbols* (list->vector *symbols*))
  (patch '*boot-env* *boot-env*))

(define (patch name definition)
  (cond ((table-entry *var-table* name)
         => (lambda (node)
              (and (var-node-defined node)
		   (warning "~S multiply defined" name))
              (set (var-node-defined node) 'define)
              (set (var-node-value node) definition)
	      (let ((desc (table-entry *reloc-table* (var-node-vcell node))))
		(generate-slot-relocation definition (fx+ (heap-offset desc) 4)))
	      (let* ((vec (var-node-refs node))
                     (size (vector-length vec)))
                (do ((i 0 (fx+ i 2)))
                    ((fx>= i size))
                  (generate-slot-relocation
                    definition
                    (fx+ (heap-offset (table-entry *reloc-table* (vref vec i))) 
                         (fx* CELL (fx+ (vref vec (fx+ i 1)) 1))))))))))
                    

		


;;; Virtual GC
          
(define (vgc root)
  (cond ((null? root) *null-descriptor*)
        ((table-entry *reloc-table* root))
        (else
         (allocate root))))

;;; ALLOCATE reserves space on an appropriate heap for obj, and
;;; associates the resulting descriptor object with obj in the
;;; relocation table.  It checks all of obj's children to ensure that
;;; they have descriptors in the relocation table (and are thus
;;; allocated), and generates relocation requests for all obj's slots
;;; that contain stored descriptors.

(define (allocate obj)
  ((xcond ((pair? obj) vgc-copy-pair)
          ((vector? obj) vgc-copy-vector)
          ((templat? obj) vgc-copy-template)
          ((symbol? obj) vgc-copy-symbol)
          ((bytev? obj)  vgc-copy-bytev)
          ((string? obj) vgc-copy-string)
          ((text? obj) vgc-copy-text)        
          ((vcell-struct? obj) vgc-copy-vcell)       
          ((address? obj) vgc-copy-address)
          ((foreign-object? obj) vgc-copy-foreign)
          ((double-float? obj) vgc-copy-double-float))
   obj))

(define (define-null-descriptor heap)
  (modify (area-frontier heap)
          (lambda (x) (fx+ x %%slink-size)))
  (set *null-descriptor*
       (object nil
         ((heap-stored self) heap)
         ((heap-offset self) tag/pair)
         ((write-descriptor self stream)
          (write-data stream tag/pair))
         ((write-store self stream)
          (let ((pi (fx+ slink/initial-pure-memory-begin 3)))
            (do ((i 0 (fx+ i 4)))
                ((fx= i pi)
                 (write-int stream 0)
                 (write-int stream (area-frontier (lstate-pure *lstate*)))
                 (write-data stream 0)
                 (write-data stream (area-frontier (lstate-impure *lstate*)))
                 (do ((i (fx+ i 16) (fx+ i 4)))
                     ((fx= i %%slink-size))
                   (write-int stream 0)))
              (write-int stream 0))))))
  (push (area-objects heap) *null-descriptor*)
  (set-table-entry *reloc-table* nil *null-descriptor*)
  (text-relocation (fx+ slink/initial-pure-memory-begin 3))
  (text-relocation (fx+ slink/initial-pure-memory-end 3))
  (data-relocation (fx+ slink/initial-impure-memory-begin 3))
  (data-relocation (fx+ slink/initial-impure-memory-end 3)))

                                          
(define (vgc-copy-pair pair)
  (let* ((heap (lstate-impure *lstate*))
         (addr (area-frontier heap))
         (desc (object nil
                 ((heap-stored self) (lstate-impure *lstate*))
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)       
                  (write-data stream (fx+ addr tag/pair)))
                 ((write-store self stream)
                  (write-slot (cdr pair) stream)
                  (write-slot (car pair) stream)))))
      (set (area-frontier heap) (fx+ addr (fx* CELL 2)))
      (push (area-objects heap) desc)
      (set-table-entry *reloc-table* pair desc)
      ;;Trace from the cdr first to linearise lists
      (generate-slot-relocation (cdr pair) addr)
      (generate-slot-relocation (car pair) (fx+ CELL addr))
      desc))

(define (vgc-copy-vector vec)
  (let* ((heap (lstate-impure *lstate*))
         (addr (area-frontier heap))
         (nelts (vector-length vec))
         (desc (object nil
                 ((heap-stored self) (lstate-impure *lstate*))
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-data stream (fx+ addr tag/extend)))
                 ((write-store self stream)
                  ;;The header                                
                  (let ((nelts (vector-length vec)))
                    (write-int stream (fx+ (fixnum-ashl nelts 8)
                                           (fx+ header/general-vector 128)))
                    (dotimes (i nelts)
                      (write-slot (vref vec i) stream)))))))
      (set (area-frontier heap) (fx+ addr (fx+ CELL (fx* CELL nelts))))
      (push (area-objects heap) desc)
      (set-table-entry *reloc-table* vec desc)
      (do ((i 0 (fx+ i 1))
           (a (fx+ addr CELL) (fx+ a CELL)))
          ((fx= i nelts))
        (generate-slot-relocation (vref vec i) a))
      desc))
                                              
(define (relocate-units the-units unit-vec)
  (let* ((heap (lstate-impure *lstate*))
         (begin (area-frontier heap))) 
    (do ((units the-units (cdr units))
         (addr begin (fx+ addr (fx* CELL (vector-length (car units))))))
        ((null? units)
         (set (area-frontier heap) addr)
         (vgc-copy-vector unit-vec)
         (walk (lambda (unit)
                 (relocate-unit-1 unit))
               the-units))
      (let ((desc (object nil
                    ((heap-stored self) (lstate-impure *lstate*))
                    ((heap-offset self) addr)   
                    ((write-descriptor self stream)
                     (write-data stream (fx+ addr tag/extend)))
                    ((write-store self stream)
                     (let ((slots (fx- (vector-length (car units)) 1)))
                       (write-int stream (fx+ (fixnum-ashl slots 8) 
                                              header/unit))
                       (do ((i 1 (fx+ i 1)))
                           ((fx> i slots) t)
                         (let ((ob (vref (car units) i)))
                           ;;We have to special case closure-internal templates
                           (cond ((cit? ob)
                                  (write-template stream ob))
                                 ((var-node? ob)
                                  (write-var-ref stream ob))
                                 ((no-op? ob))
                                 (else
                                  (write-slot ob stream))))))))))
        (push (area-objects heap) desc)
        (set-table-entry *reloc-table* (car units) desc)))))
                                                                          
(define (relocate-unit-1 unit)
  (let* ((desc (table-entry *reloc-table* unit))
         (nslots (vector-length unit)))
    (do ((i 1 (fx+ i 1))
         (a (fx+ (heap-offset desc) CELL) (fx+ a CELL)))
        ((fx= i nslots))
      (let ((ob (vref unit i)))
        ;;We have to special case closure-internal templates
        (cond ((cit? ob)
               (generate-slot-relocation (cit-code-vec ob) 
                                         (fx+ a (fx* CELL 2))
                                         ))
              ((no-op? ob))              
              ((var-node? ob)
               (relocate-unit-variable ob a nil))
              (else
               (generate-slot-relocation ob a)))))))

(define (vgc-copy-vcell vcell)
  (let* ((heap (lstate-impure *lstate*))
         (addr (area-frontier heap))
         (var (vcell-struct-var vcell))
         (desc (object nil
                 ((heap-stored self) (lstate-impure *lstate*))
                 ((heap-offset self) addr)   
                 ((write-descriptor self stream)
                  (write-data stream (fx+ addr tag/extend)))
                 ((write-store self stream)
		  (write-vcell-header var stream)
                  (write-var-ref stream var)
                  (write-data stream (fx+ addr 22)) 
                  (write-slot (var-node-name var) stream)
		  (write-data stream (fx+ addr 30))
                  (write-int stream header/weak-alist)
                  (write-slot (var-node-refs var) stream)
                  (write-int stream header/weak-alist)
                  (write-slot (var-node-vcell-refs var) stream)))))
    (set (area-frontier heap) (fx+ addr (fx* CELL 9)))  ; 5 for vcell
    (set-table-entry *reloc-table* vcell desc)          ; 4 for weak-alists
    (push (area-objects heap) desc) 
    (relocate-unit-variable var (fx+ addr CELL) t)
    (set (var-node-refs var) (a-list->vector (var-node-refs var)))
    (set (var-node-vcell-refs var) (a-list->vector (var-node-vcell-refs var)))
    (generate-slot-relocation (var-node-refs var) (fx+ addr (fx* CELL 6)))
    (generate-slot-relocation (var-node-vcell-refs var) (fx+ addr (fx* CELL 8)))
    (generate-slot-relocation (var-node-name var) (fx+ addr (fx* CELL 3)))
    (data-relocation (fx+ addr (fx* CELL 2)))
    (data-relocation (fx+ addr (fx* CELL 4)))
    desc))
                                                                  
(define (a-list->vector a)
  (let ((vec (make-vector (fx* (length a) 2))))
    (do ((i 0 (fx+ i 2))
         (a a (cdr a)))
        ((null? a) vec)
      (set (vref vec i) (caar a))
      (set (vref vec (fx+ i 1)) (cdar a)))))

(define (vgc-copy-template tmplt)
  (let* ((cv (vgc (templat-code-vec tmplt)))
         (desc (object nil
                 ((heap-stored self) (lstate-pure *lstate*))
                 ((write-descriptor self stream)
                  (write-int stream (fx+ (templat-offset tmplt)
                                         (fx+ (heap-offset cv) CELL)))))))
    (set-table-entry *reloc-table* tmplt desc)
    desc))

(define (vgc-copy-symbol sym)
  (push *symbols* sym)
  (let* ((heap (lstate-pure *lstate*))
         (addr (area-frontier heap))
         (end-addr (fx+ CELL (fx+ addr (symbol-length sym))))
         (desc (object nil
                 ((heap-stored self) (lstate-pure *lstate*))
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-int stream (fx+ (heap-offset self) tag/extend)))
                 ((write-store self stream)
                  (let ((len (symbol-length sym)))
                    (write-int stream (fx+ (fixnum-ashl len 8)
                                           (fx+ header/symbol 128)))
                    (write-fixnum stream (symbol-hash sym))
                    (write-block stream sym %%symbol-text-offset len)
                    (dotimes (i (fx- (align len 2) len))
                      (write-byte stream 0)))))))
    (set (area-frontier heap) (align end-addr 2))
    (push (area-objects heap) desc)
    (set-table-entry *reloc-table* sym desc)
    desc))
                                                                                  
(define (vgc-copy-bytev vec)
  (vgc-copy-bytes vec (bytev-length vec) header/bytev))

(define (vgc-copy-text text) 
  (vgc-copy-bytes text (text-length text) header/text))

(define (vgc-copy-bytes bytes vlen header)
  (let* ((heap (lstate-pure *lstate*))
         (addr (area-frontier heap))
         (end-addr (fx+ CELL (fx+ addr vlen)))
         (desc (object nil
                 ((heap-stored self) (lstate-pure *lstate*))
                 ((heap-offset self) addr)    
                 ((write-descriptor self stream)
                  (write-int stream (fx+ addr tag/extend)))
                 ((write-store self stream)           
                  (let ((vlen (bytev-length bytes)))
                    (write-int stream (fx+ (fixnum-ashl vlen 8)
                                           (fx+ header 128)))
                    (write-block stream bytes 0 vlen)
                    ;;Pad to the next cell boundary.
                    (dotimes (i (fx- (align vlen 2) vlen))
                      (write-byte stream 0)))))))
    (set (area-frontier heap) (align end-addr 2))
    (push (area-objects heap) desc)
    (set-table-entry *reloc-table* bytes desc)
    desc))

(define (vgc-copy-string str)
  (let* ((heap (lstate-impure *lstate*))
         (addr (area-frontier heap))
         (text (string-text str)) 
         (desc (object nil
                 ((heap-stored self) (lstate-impure *lstate*))
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-data stream (fx+ addr tag/extend)))
                 ((write-store self stream)
                  (write-int stream (fx+ (fixnum-ashl (text-length text) 8)
                                         header/slice))
                  (write-slot text stream)
                  (write-int stream 0)))))       ;; offset
    (set (area-frontier heap) (fx+ addr (fx* CELL 3)))
    (set-table-entry *reloc-table* str desc)
    (push (area-objects heap) desc)
    (generate-slot-relocation text (fx+ addr CELL))
    desc))                    

(define (write-var-ref stream var)
  (cond ((neq? (var-node-value var) NONVALUE)
         (let ((value (var-node-value var)))
            (if (unit-loc? value)
                (write-unit-loc stream value)
                (write-slot value stream))))
        (else
         (write-int stream header/nonvalue))))
                                                     
;;; Flonum dismemberment.

;;; Returns sign, and normalized mantissa and exponent  
;;; PRECISION is number of bits desired in the mantissa 
;;; EXCESS is the exponent excess
;;; HIDDEN-BIT-IS-1.? is true if the hidden bit preceeds the
;;;  binary point (it does in Apollo IEEE, does not on the VAX).

(define (normalized-float-parts flonum precision excess hidden-bit-is-1.?)
    (cond ((fl= flonum 0.0)
           (return 0 (%ash 1 (fx+ precision 1)) 0))
          (else
           (receive (#f m e) (integer-decode-float flonum)
              (let* ((have (integer-length m))
                     (need (fx- precision have))
                     (normalized-m (%ash m need))
                     (normalized-e (- (+ e 
                                         precision 
                                         excess
                                         (if hidden-bit-is-1.? -1 0))
                                       need)))
                 (return (if (fl< flonum 0.0) 1 0) normalized-m normalized-e))))))

(define (vgc-copy-double-float float)
  (let* ((heap (lstate-pure *lstate*))
         (addr (area-frontier heap))
         (desc (object nil
                 ((heap-stored self) (lstate-pure *lstate*))
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-int stream (fx+ addr tag/extend)))
                 ((write-store self stream)
                  (write-double-float stream float)))))
    (set (area-frontier heap) (fx+ addr (fx* CELL 3)))
    (set-table-entry *reloc-table* float desc)
    (push (area-objects heap) desc)
    desc))                    
                                                          
;;; Floating point bit fields.

;;; <n,s> means bit field of length s beginning at bit n of the first
;;; WORD (not longword)
;;;                    sign      exponent   MSB       fraction
;;; Apollo IEEE flonum <15,1>    <4,11>     hidden    <0,4>+next 3 words
;;; VAX11 flonum (D)   <15,1>    <7,8>      hidden    <0,7>+next 3 words
;;; Apollo IEEE flonum - binary point follows  hidden MSB, 53 bits of
;;;     precision, if hidden bit is included
;;; VAX11 flonum (D)   - binary point precedes hidden MSB, 56 bits of
;;;     precision, if hidden bit is included 


(define (write-block port obj start len)
  (let ((writec (if (iob? port) vm-write-char write-char)))
    (do ((i start (fx+ i 1)))
        ((fx>= i len))
      (writec port (text-elt obj i)))))


(define (write-unit-loc stream u)
  (write-data stream (fx+ (heap-offset (table-entry *reloc-table* (unit-loc-unit u)))
                         (fx+ tag/extend
                              (unit-loc-offset u)))))

(define (unit-var-value value)
  (if (unit-loc? value)
      (fx+ (heap-offset (table-entry *reloc-table* (unit-loc-unit value)))
           (fx+ (unit-loc-offset value) tag/extend))
      (heap-offset (table-entry *reloc-table* value))))

(define-integrable (align n m)
  (let ((2^m-1 (fx- (fixnum-ashl 1 m) 1)))
    (fixnum-logand (fx+ n 2^m-1) (fixnum-lognot 2^m-1))))

(define-operation (heap-stored obj))
(define-operation (heap-offset obj))           
(define-operation (write-descriptor obj stream))
(define-operation (write-store obj stream))
