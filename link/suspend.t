(herald suspend (env tsys))

(define-local-syntax (dotimes spec . body)
  (let ((index (car spec))
        (limit (cadr spec)))
    `(do ((,index 0 (fx+ ,index 1)))
         ((fx= ,index ,limit))
       ,@body)))


(lset *lstate* nil)
                         
(define (system-suspend path experimental?) 
  (suspend top-level-environments path experimental?))

(define-structure-type lstate   ;linker state
    pure            
    impure          
    foreign-reloc   
    foreign                     
    symbols                        
    symbol-count
    text-reloc   ;List of relocation items
    data-reloc
    pure-size
    reloc 
    null
    )
                          
(define lp-table-size (fx* 256 1024))
                                           
(define (create-lstate)
  (let ((l (make-lstate)))
    (set (lstate-foreign l) '())
    (set (lstate-pure l) (make-+area))
    (set (lstate-impure l) (make-+area))
    (set (lstate-symbols l) '())
    (set (lstate-symbol-count l) 0)
    (set (lstate-foreign-reloc l) '())
    (set (lstate-text-reloc l) '())
    (set (lstate-data-reloc l) '())
    (set (lstate-reloc l) (make-lp-table lp-table-size 'reloc-table))
    l))


(define-structure-type +area         ;A.k.a. "heap"
  frontier      ;Address of next available cell
  objects       ;List of objects allocated
  )

(let ((master (stype-master +area-stype)))
  (set (+area-frontier    master) 0)
  (set (+area-objects     master) '()))
                              
(*define t-implementation-env '*boot* 
  (lambda (root-process boot-args debug?)
    (ignore debug?)
    (dispatch-init)
    (set (system-global slink/boot-area-base) (make-vector 0))
    (set (system-global slink/initial-impure-base) top-level-environments)
    (set (*value t-implementation-env '**up**) luser-typed-eof-at-top-level)
    (re-initialize-systems)
    (top-level)))

(define (omit null args)
  (walk (lambda (arg)
          (set-lp-table-entry (lstate-reloc *lstate*) arg null))
        args))
       
(block (lset *omit-list* 
         (list *the-initial-symbols*
               *the-initial-modules*
               *code-unit-map*
               *boot-env*
               **cont**
               **up**
               **ret**

               bootstrap-env
               boot-adjust-initial-units
               initialize-symbol-table
               make-base-environment
               object-hash-table
               object-unhash-table
               ))
 nil)


(define (really-suspend object out-spec out-type)                                             
  (format t "~&Suspending ~a ... ~%" out-spec)
  (bind ((*lstate* (create-lstate)))
    (with-open-ports
        ((image (open (filename-with-type (->filename out-spec) out-type) '(out))))
      (omit (set-up-the-slink) (cons *lstate* *omit-list*))
      (modify (system-%link-edit t-system)
	      (lambda (x) (fx+ x 1)))
      (vgc object)
      (format t "writing object file~%")
      (set (lstate-pure-size *lstate*) 
           (+area-frontier (lstate-pure *lstate*)))
      (write-link-file image)
      *lstate*)))
                     
(define (set-up-the-slink)
  (modify (+area-frontier (lstate-impure *lstate*))
          (lambda (x) (fx+ x %%slink-size)))
  (let ((null 
         (object nil
           ((heap-stored self) (lstate-impure *lstate*))
           ((heap-offset self) tag/pair)
           ((write-descriptor self stream)
            (write-data stream tag/pair))
           ((write-store self stream)
            (let ((pi (fx+ slink/initial-pure-memory-begin 3)))
              (do ((i 0 (fx+ i 4)))
                  ((fx= i pi)
                   (write-int stream 0)
                   (write-int stream (+area-frontier (lstate-pure *lstate*)))
                   (write-data stream 0)
                   (write-data stream (+area-frontier (lstate-impure *lstate*)))
                   (write-int stream (fx-ashl (fx+ (gc-stamp) 1) 2))
                   (do ((i (fx+ i 20) (fx+ i 4)))
                       ((fx= i %%slink-size))
                     (write-int stream 0)))
                (write-int stream 0)))))))
    (set (lstate-null *lstate*) null)
    (push (+area-objects (lstate-impure *lstate*)) null)
    (text-relocation (fx+ slink/initial-pure-memory-begin 3))
    (text-relocation (fx+ slink/initial-pure-memory-end 3))
    (data-relocation (fx+ slink/initial-impure-memory-begin 3))
    (data-relocation (fx+ slink/initial-impure-memory-end 3))
    null))

;;; Virtual GC
          
(define (vgc obj)
  (cond ((null? obj) (lstate-null *lstate*))
        ((lp-table-entry (lstate-reloc *lstate*) obj))
        ((pair? obj)
         (vgc-pair obj))
        (else
         (let ((header (extend-header obj)))
           (cond ((template-header? header)
                  (vgc-template obj))
                 ((extend? header)
                  (vgc-closure obj header))
                 ((immediate? header)
                  ((vref *suspend-dispatch-vector* 
                         (header-type (extend-header obj)))
                    obj))
                 (else
                  (error "Corrupt-header ~s ~s" obj header)))))))


(define (vgc-pair pair)
  (let* ((heap (lstate-impure *lstate*))
         (addr (+area-frontier heap))
         (desc (object nil
                 ((heap-stored self) (lstate-impure *lstate*))
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)       
                  (write-data stream (fx+ addr tag/pair)))
                 ((write-store self stream)
                  (write-slot (cdr pair) stream)
                  (write-slot (car pair) stream)))))
      (set (+area-frontier heap) (fx+ addr (fx* CELL 2)))
      (push (+area-objects heap) desc)
      (set-lp-table-entry (lstate-reloc *lstate*) pair desc)
      ;;Trace from the cdr first to linearise lists
      (generate-slot-relocation (cdr pair) addr)
      (generate-slot-relocation (car pair) (fx+ CELL addr))
      desc))

(define (vgc-template tmplt)
  (vgc-internal-object tmplt 
                       (template-enclosing-object tmplt) 
                       (template-encloser-offset tmplt)))

(define (vgc-closure closure template)
  (cond ((template-internal-bit? template)
         (vgc-internal-object closure 
                              (closure-enclosing-object closure)
                              (closure-encloser-offset closure)))
        (else
         (let* ((ptrs (template-pointer-slots template))
                (size (fx+ ptrs (template-scratch-slots template))))
           (vgc-extend closure ptrs size)))))
                                             

(define (vgc-extend obj ptrs size)
  (let* ((heap (lstate-impure *lstate*))
         (addr (+area-frontier heap))
         (desc 
           (if (fx= ptrs size)
               (object nil
                 ((heap-stored self) (lstate-impure *lstate*))
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-data stream (fx+ addr tag/extend)))
                 ((write-store self stream)
                  (do ((i -1 (fx+ i 1)))
                      ((fx= i ptrs) t)
                    (write-slot (extend-elt obj i) stream))))
               (object nil
                 ((heap-stored self) (lstate-impure *lstate*))
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-data stream (fx+ addr tag/extend)))
                 ((write-store self stream)
                  (do ((i -1 (fx+ i 1)))
                      ((fx= i ptrs)
                       (do ((i i (fx+ i 1)))
                           ((fx= i size) t)
                         (write-scratch stream obj i)))
                    (write-slot (extend-elt obj i) stream)))))))
      (set (+area-frontier heap) (fx+ addr (fx+ (fx* CELL size) CELL)))
      (push (+area-objects heap) desc)
      (set-lp-table-entry (lstate-reloc *lstate*) obj desc)
      (do ((i -1 (fx+ i 1))
           (a addr (fx+ a CELL)))
          ((fx= i ptrs) desc)
        (generate-slot-relocation (extend-elt obj i) a))))
  

(define (vgc-internal-object obj obj-encloser offset)
  (let ((encloser (vgc obj-encloser)))
    (cond ((lp-table-entry (lstate-reloc *lstate*) obj))
          (else
           (let* ((addr (fx+ (fixnum-ashl offset 2) 
                             (fx+ (heap-offset encloser) tag/extend)))
                  (desc 
                   (if (bytev? obj-encloser)
                       (object nil
                         ((heap-stored self) (lstate-pure *lstate*))
                         ((write-descriptor self stream)
                          (write-int stream addr)))
                       (object nil
                         ((heap-stored self) (lstate-impure *lstate*))
                         ((write-descriptor self stream)
                          (write-data stream addr))))))
             (set-lp-table-entry (lstate-reloc *lstate*) obj desc)
             desc)))))

(define (vgc-bytes bytes vlen pure?)
  (let* ((heap (if pure? (lstate-pure *lstate*) (lstate-impure *lstate*)))
         (addr (+area-frontier heap))
         (end-addr (fx+ CELL (fx+ addr vlen)))
         (desc 
           (if pure?
               (object nil
                 ((heap-stored self) (lstate-pure *lstate*))
                 ((heap-offset self) addr)    
                 ((write-descriptor self stream)
                  (write-int stream (fx+ addr tag/extend)))
                 ((write-store self stream)
                  (write-slot (extend-header bytes) stream)
                  (let ((len (bytev-length bytes)))
                    (do ((i 0 (fx+ i 1)))
                        ((fx>= i len)
                         (dotimes (i (fx- (align len 2) len))
                           (vm-write-byte stream 0)))
                      (vm-write-byte stream (bref bytes i))))))
               (object nil
                 ((heap-stored self) (lstate-impure *lstate*))
                 ((heap-offset self) addr)    
                 ((write-descriptor self stream)
                  (write-data stream (fx+ addr tag/extend)))
                 ((write-store self stream)
                  (write-slot (extend-header bytes) stream)
                  (let ((len (bytev-length bytes)))
                    (do ((i 0 (fx+ i 1)))
                        ((fx>= i len)
                         (dotimes (i (fx- (align len 2) len))
                           (vm-write-byte stream 0)))
                      (vm-write-byte stream (bref bytes i)))))))))
    (set (+area-frontier heap) (align end-addr 2))
    (push (+area-objects heap) desc)
    (set-lp-table-entry (lstate-reloc *lstate*) bytes desc)
    desc))


(define *suspend-dispatch-vector* (make-vector %%number-of-immediate-types))

(let ((gc-copiers
      `(
        (,header/text           ,vgc-text)
        (,header/general-vector ,vgc-general-vector)
        (,header/unit           ,vgc-unit)
        (,header/slice          ,vgc-string)
        (,header/symbol         ,vgc-symbol)
        (,header/bytev          ,vgc-bytev)
        (,header/foreign         ,vgc-foreign)
        (,header/template       ,vgc-template)
        (,header/cell           ,vgc-cell)
        (,header/vcell          ,vgc-vcell)
        (,header/bignum         ,vgc-bignum)
        (,header/double-float   ,vgc-double-float)
        (,header/weak-set       ,vgc-weak)
        (,header/weak-alist     ,vgc-weak)
        (,header/weak-table     ,vgc-weak-table)
        (,header/weak-cell      ,vgc-weak-cell)
        )))
  (vector-fill *suspend-dispatch-vector* vgc-error)
  (walk (lambda (x) (set (vector-elt *suspend-dispatch-vector*
                                     (fixnum-ashr (car x) 2))
                         (cadr x)))
        gc-copiers))

(define (vgc-error obj)
  (error "Don't know how to vgc ~s" obj))

(define (vgc-text text) 
  (vgc-bytes text (text-length text) (pure? text)))

(define (vgc-symbol sym)
  (vgc-bytes sym (symbol-length sym) t))
                                                                                  
(define (vgc-bytev bytev)
  (vgc-bytes bytev (bytev-length bytev) (pure? bytev)))

(define (vgc-general-vector vec)
  (vgc-extend vec (vector-length vec) (vector-length vec)))
                                                           
(define (vgc-unit unit)
  (unit-snap-links unit)
  (vgc-extend unit (unit-length unit) (unit-length unit)))
                                                           
(define (vgc-string str)              
  (vgc-extend str 1 2))

(define (vgc-cell cell)
  (vgc-extend cell 1 1))

(define (vgc-vcell vcell)
  (vgc-extend vcell %%vcell-size %%vcell-size))

(define (vgc-bignum bignum)
  (vgc-extend bignum 0 (bignum-length bignum)))

(define (vgc-double-float d)
  (vgc-extend d 0 2))

(define (vgc-weak weak)
  (vgc-extend weak 1 1))

(define (vgc-weak-cell weak)
  (let* ((heap (lstate-impure *lstate*))
         (addr (+area-frontier heap))
         (desc (object nil
                 ((heap-stored self) heap)
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-data stream (fx+ addr tag/extend)))
                 ((write-store self stream)
                  (write-slot (extend-elt weak -1) stream)
                  (write-slot nil stream)))))
    (set (+area-frontier heap) (fx+ addr (fx+ (fx* CELL 1) CELL)))
    (push (+area-objects heap) desc)
    (set-lp-table-entry (lstate-reloc *lstate*) weak desc)
    (generate-slot-relocation nil (fx+ addr CELL))
    desc))

(define (vgc-weak-table weak)
  (vgc-extend weak 1 2))

(define-integrable (align n m)
  (let ((2^m-1 (fx- (fixnum-ashl 1 m) 1)))
    (fixnum-logand (fx+ n 2^m-1) (fixnum-lognot 2^m-1))))
      
(define-operation (heap-stored obj))
(define-operation (heap-offset obj))           
(define-operation (write-descriptor obj stream))
(define-operation (write-store obj stream))


(define (unit-snap-links unit)
  (let ((len (unit-length unit)))
    (do ((i 0 (fx+ i 1)))
	((fx>= i len) t)
      (let ((thing (extend-elt unit i)))
	(or (template? thing)
	    (not (extend? thing))
	    (neq? (extend-header thing) *link-snapper-template*)
	    (set (extend-elt unit i) (extend-elt thing 0)))))))