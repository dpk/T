(herald defs)


(define NONVALUE (list '**nonvalue**))

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
    )

(define (create-lstate)
  (let ((l (make-lstate)))
    (set (lstate-foreign l) '())
    (set (lstate-pure l) (make-area))
    (set (lstate-impure l) (make-area))
    (set (lstate-symbols l) '())
    (set (lstate-symbol-count l) 0)
    (set (lstate-foreign-reloc l) '())
    (set (lstate-text-reloc l) '())
    (set (lstate-data-reloc l) '())
    l))



(define-structure-type area         ;A.k.a. "heap"
  frontier      ;Address of next available cell
  objects       ;List of objects allocated
  )

(let ((master (stype-master area-stype)))
  (set (area-frontier    master) 0)
  (set (area-objects     master) '()))

    
(define-structure-type foreign-object
  name     ;the string that is the name of this procedure
  )

(define-structure-type templat
  code-vec  ;the code vector
  offset)   ;and offset within the vector where this template points to.


;;; closure-internal-templates

(define-structure-type cit
  pointer      ;number of pointer slots  
  scratch      ;number of scratch slots  
  nargs        ;number of args this template's fun takes
  header/nary? ; byte with high bit on if nary, low bits template header
  code-vec     ;the code vector
  aux-offset   ;and offset within the vector where the auxiliary template begins
  unit-offset  ;offset of template in unit (bytes)
  )

(define-integrable (no-op? x)
  (eq? x no-op))

(define-structure-type vcell-struct
  var           ; var node
  )

(define-structure-type unit-loc
  unit
  offset
  )
                                 

(define (create-unit-loc unit offset)
  (let ((u (make-unit-loc)))
    (set (unit-loc-unit u) unit)
    (set (unit-loc-offset u) offset)
    u))

(define-structure-type address
  heap 
  addr)

(define (create-address heap addr)
  (let ((a (make-address)))
    (set (address-heap a) heap)
    (set (address-addr a) addr)
    a))


(define-structure-type var-node
  name          ;the symbol that is this var's name
  refs          ;a list of (unit . slot) pairs giving the unit slots where this
                ;var's value is kept.
  defined
  vcell
  value         ; slot where linker definition (closure) occurs in unit
  vcell-refs
  )

(let ((node (stype-master var-node-stype)))
  (set (var-node-refs node) '())                                  
  (set (var-node-vcell-refs node) '())                                  
  (set (var-node-defined node) nil)
  (set (var-node-vcell node) nil)
  (set (var-node-value node) NONVALUE))

(lset *linker-debug?* nil)

(define (linker-message message . args)
  (apply format t message args))

;++(define (linker-message message . args)
;++  (apply format *linker-noise-file* message args))

(define (linker-debug message . args)
  (cond (*linker-debug?*
         (apply format t message args)
         (newline (terminal-output)))))
