(herald scanner (env tsys))

;;; Heap and Stack Scanner

;;; This code is not GC safe, it uses illegal pointers.

;;;===========================================================================

;;; Useful scanning procedures.

;;; Find all the pointers that point to THING.

(define (heap-find-it pred)
  (scan-heap (find-it pred)))

(define (find-it pred)
  (lambda (ptr h-offset type o-offset)
    (cond ((pred ptr)
           (format t "~&Obj= ~a  @~D: ~S(~D)~%"
                   ptr h-offset type o-offset)
           (breakpoint)))))



;;; Find all unreasonable pointers.  Anything that points anywhere
;;; in the stack space (including beyond the current stack top)
;;; is considered reasonable.

(define (reasonable-check)
  (scan-heap reasonable-check-proc))

(define (reasonable-check-proc ptr h-offset type o-offset)
  (if (not (reasonable?? ptr))
      (format t "~&~D: ~S(~D)~%"
              h-offset type o-offset)))

(define (reasonable?? ptr)
  (or (reasonable? ptr)
      (template-header? ptr)
      (let ((num (descriptor->fixnum ptr)))
        (and (fx>= num (descriptor->fixnum
                        ;;(vref *boot-args* 4)  ; Aargh!!!
                        (system-global slink/boot-args)))
             (fx<= num (process-global task/stack))))))

;;; Print out the heap.  

(define (print-it ptr h-offset type o-offset)
  (ignore h-offset type o-offset)
  (z-print ptr (standard-output))
  (vm-newline (standard-output)))

;;; Do nothing

(define (null-proc #f #f #f #f) nil)

;;; Scan the heap looking at pointers

(define (scan-heap ptr-proc)
  (let ((base     (area-base (current-area)))
        (frontier (process-global task/area-frontier)))
    (scan base frontier (pointer-check-proc ptr-proc))))

(define (pointer-check-proc ptr-proc)
  (lambda (ptr offset ptrs scrs type)
    (ignore scrs)
    (if (memq? type '(closure pair))
        (ptr-proc (extend-header ptr) offset type -1))
    (scan-slots ptr offset ptrs ptr-proc type)))

(define (scan-slots ptr offset size ptr-proc type)
  (do ((i 0 (fx+ i 1)))
      ((fx>= i size))
    (ptr-proc (extend-elt ptr i) offset type i)))

;;;===========================================================================
;;; Impure area scanning

(define (scan-impure-area ptr-proc)
  (scan (system-global slink/initial-impure-base)
        (system-global slink/initial-impure-memory-end)
        (pointer-check-proc ptr-proc)))

(define (impure-find-it pred)
  (scan-impure-area (find-it pred)))


;;;===========================================================================

;;; Stack Scanning

;;;   The stack is just like the heap except that currently it contains only
;;; closures and fault frames.

(define (scan-stack obj-proc)
  (let ((stack-base (process-global task/stack))
        (top-of-stack (current-continuation)))
    (scan top-of-stack stack-base obj-proc)))

;;;===========================================================================

;;; Heap Scanning

;;; The actual scanning procedure.  BASE is an extend pointer to
;;; the beginning of the area to be scanned.  LIMIT is the size
;;; of the area (in cells) to be scanned.  OBJECT-PROC is a procedure
;;; of five arguments that is called on every pointer in the area
;;; scanned:
;;;    (OBJECT-PROC PTR OFFSET PTRS SCRS TYPE)
;;; PTR is the current value.  H-OFFSET is the offset from BASE
;;; of PTR.  PTRS and SCRS are the number of the object's pointer
;;; and scratch slots.  TYPE is a symbol describing the type of
;;; the object.  Currently TYPE in one of CLOSURE, FAULT-FRAME,
;;; PAIR, UNIT, GENERAL-VECTOR, STRING-SLICE, CELL, WEAK, VCELL,
;;; VFRAME, RATIO, BIGNUM, TEXT, FOREIGN, DOUBLE-FLOAT, BYTEV, and
;;; maybe a few others.  Look at the immediate dispatch below.
;++ GC defer around this.


(define (scan base limit object-proc)
  (format t "~&base= (~D) ~A~&start= ~D limit= ~D size= ~D~%"
            (object-hash base)
            base
            (descriptor->fixnum base)
            limit
            (fx- limit (descriptor->fixnum base))) 
  (real-scan base limit object-proc))

(define (real-scan base limit object-proc)
  (let* ((start (descriptor->fixnum base))
         (size (fx- (fx- limit start) 1)))
    (iterate loop ((offset -1) (count 0))
      (cond ((fx>= offset size) count)
            (else
             (let ((ptr (make-pointer base offset)))
               (receive (ptrs scrs type)
                        (scan-object-size ptr)
                 (object-proc ptr offset ptrs scrs type)
                 (loop (fx+ offset (fx+ 1 (fx+ ptrs scrs)))
                       (fx+ count 1)))))))))

;;;   Get the size and type of PTR.  This dispatches on the header.
;;; The only thing HEADER cannot be is a template header.  PTR is either a
;;; closure, an immediate with a header, or a pair.  The appropriate procedure
;;; is called for each.

(define (scan-object-size ptr)
  (let ((header (extend-header ptr)))
    (cond ((and (template-header? header)   ; 68000 requires this first
                (not (fixnum? header)))
           (error "extend ~S with template header #x~X~%" ptr header))
          ((template? header)
           (scan-closure ptr header))
          ((and (immediate? header)
                (not (or (char? header)
                         ;++ flush when true changed
                         (eq? header t))))
           ((vref *scan-dispatch-vector* (header-type header)) ptr))
          (else
           (return 1 0 'pair)))))

;;;   Scan a closure first checking that it is not supposed to be inside some
;;; other closure.

(define (scan-closure ptr template)
  (cond ((template-internal-bit? template)
         (error "internal closure ~S not inside.~%" ptr))
        (else
         (return (template-pointer-slots template)
                 (template-scratch-slots template)
                 'closure))))

;;;===========================================================================

;;; Scanning immediate objects.  
;;;   The procedures for scanning immediate objects are put into a dispatch
;;; vector indexed by the header types of the objects.  Scanning is just
;;; a matter of pulling the appropriate procedure out of the vector.

(define *scan-dispatch-vector* (make-vector %%number-of-immediate-types))

;;;    Initialize the dispatch vector.  This is called when the file is loaded.
;;; See the last line of the file.  The vector is first filled with SCAN-ERROR
;;; and then the individual scanner procedures are installed.

(define (initialize-immediate-scanners)
  (let ((scanners
        `(
         ; (,header/char           ,scan-char)  ; chars are only inside other objects
          (,header/unit           ,scan-unit)
          (,header/text           ,scan-text)
          (,header/general-vector ,scan-general-vector)
          (,header/slice          ,scan-string-slice)
          (,header/symbol         ,scan-symbol)
          (,header/bytev          ,scan-bytev)
          (,header/foreign         ,scan-foreign)
          (,header/template       ,scan-template)
          (,header/cell           ,scan-cell)
         ; (,header/weak           ,scan-weak)
          (,header/weak-cell      ,scan-weak-cell)
          (,header/weak-set       ,scan-weak-set)
          (,header/weak-alist     ,scan-weak-alist)
          (,header/weak-table     ,scan-weak-table)
         ; (,header/task           ,scan-error)
         ; (,header/true           ,scan-error) ; true only exists inside other objects
          (,header/vcell          ,scan-vcell)
          (,header/vframe         ,scan-vframe)
          (,header/fault-frame     ,scan-fault-frame)

          ;; Numbers
          (,header/bignum         ,scan-bignum)
         ; (,header/short-float    ,scan-error) ;unimplemented
          (,header/double-float   ,scan-double-float)
          (,header/single-float   ,scan-single-float)
          (,header/ratio          ,scan-ratio)
;          (,header/complex        ,scan-complex)
          )))
    (vector-fill *scan-dispatch-vector* scan-error)
    (walk (lambda (x) (set (vector-elt *scan-dispatch-vector*
                                       (fixnum-ashr (car x) 2))
                           (cadr x)))
          scanners)))

;;; The default scan procedure for immediate objects.

(define (scan-error ptr)
  (error "no scan method for immediate ~A~%" ptr))

;;; All of the various scanning procedures for immediate objects.  These are
;;; all simple and straight forward (but not necessarily correct).

(define (scan-template ptr)
  (error "immediate with template header ~A~%" ptr))

(define (scan-bytev ptr)
  (return 0 (bytev-cells ptr) 'bytev))

(define (scan-text ptr)
  (return 0 (bytev-cells ptr) 'text))

(define (scan-symbol ptr)
  (return 0 (bytev-cells ptr) 'symbol))

(define (scan-bignum ptr)
  (return 0 (bignum-length ptr) 'bignum))

(define (scan-foreign ptr)
  (ignore ptr)
  (return 1 1 'foreign))

(define (scan-unit ptr)
  (return (unit-length ptr) 0 'unit))

(define (scan-general-vector ptr)
  (return (vector-length ptr) 0 'general-vector))

(define (scan-string-slice ptr)
  (ignore ptr)
  (return 1 1 'string-slice))

(define (scan-cell ptr)
  (ignore ptr)
  (return 1 0 'cell))

(define (scan-weak ptr)
  (ignore ptr)
  (return 1 0 'weak))

(define (scan-weak-cell ptr)
  (ignore ptr)
  (return 1 0 'weak-cell))

(define (scan-weak-set ptr)
  (ignore ptr)
  (return 1 0 'weak-set))

(define (scan-weak-alist ptr)
  (ignore ptr)
  (return 1 0 'weak-alist))

(define (scan-weak-table ptr)
  (ignore ptr)
  (return 2 0 'weak-table))

(define (scan-vcell ptr)
  (ignore ptr)
  (return %%vcell-size 0 'vcell))

(define (scan-vframe ptr)
  (return (vframe-pointer-slots ptr)
          (vframe-scratch-slots ptr)
          'vframe))
                               
(define (scan-fault-frame ptr)
  (return 0
          (fault-frame-slots ptr)
          'fault-frame))


(define (scan-double-float ptr)
  (ignore ptr)
  (return 0 2 'double-float))

(define (scan-single-float ptr)
  (ignore ptr)
  (error "single cell floats are unimplemented."))

(define (scan-ratio ptr)
  (ignore ptr)
  (return 2 0 'ratio))

(define (scan-complex ptr)
  (ignore ptr)
  (error "complex numbers are unimplemented."))

;;; Do the initializing.

(initialize-immediate-scanners)
