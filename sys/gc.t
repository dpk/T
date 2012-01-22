(herald gc
  (env tsys
       (osys table)       ;; %TABLE-VECTOR must be integrated here
       (osys gc_weak)))   ;; for the GC-WEAK-???-LISTs

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.
;;; This material was developed by the T Project at the Yale University Computer 
;;; Science Department.  Permission to copy this software, to redistribute it, 
;;; and to use it for any purpose is granted, subject to the following restric-
;;; tions and understandings.
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;

;;; T 3.0 garbage collector, based on Clark's algorithm.
;;; Tested using a simulated memory.  See GCTEST.T, GCSIM.T, etc.
;;;
;;; For a description of the algorithm and related information see
;;; GC.DOC. For a description of T3 data representations see DATA.DOC.
;;;
;;; ***Important***
;;;    The first two slots in a closure cannot contain closure
;;;    internal closures.  There cannot be pointers into either
;;;    of the first two slots of any extend that contains pointers.
;;;    This is because those slots are used to hold back pointers
;;;    during GC.
;;;
;;; The following procedures are needed for MOVE-OBJECT to run:
;;;
;;;    (K-LIST)
;;;        A global variable that cannot be a variable because of circularity
;;;        problems.  This is a list of unfinished objects, linked in a list
;;;        in the old heap.  It is a pseudonym for (PROCESS-GLOBAL TASK/K-LIST).
;;;    (DESCRIPTOR->FIXNUM pointer)
;;;        Change a descriptor to a fixnum by clobbering the tag.
;;;    (DESCRIPTOR-TAG pointer)
;;;        Returns the type tag of POINTER.
;;;    (CLOSURE? obj)
;;;        Is OBJ a closure?
;;;    (TEMPLATE-HEADER? header)
;;;        Is HEADER the header of a template?
;;;    (EXTEND-ELT extend offset)
;;;        The contents of EXTEND + OFFSET(in zero based longwords).  This is
;;;        settable.
;;;    (EXTEND-HEADER extend)
;;;        Returns the header of EXTEND i.e. (EXTEND-ELT EXTEND -1)
;;;    (HEADER-TYPE header)
;;;        The type field of an extend header.
;;;    (MAKE-POINTER pointer offset)
;;;        Returns a pointer to POINTER + OFFSET.
;;;    (GC-EXTEND->PAIR extend)
;;;    (GC-PAIR->EXTEND pair)
;;;        Change the type tag as indicated.
;;;    *OLD-SPACE-BEGIN*
;;;    *OLD-SPACE-FRONTIER*
;;;        The limits of old-space (these are fixnums).
;;;    (IN-NEW-SPACE? obj)
;;;        Is OBJ within new-space.
;;;    (CLOSURE-ENCLOSING-OBJECT <closure-pointer>)
;;;    (CLOSURE-ENCLOSER-OFFSET  <closure-pointer>)
;;;    (TEMPLATE-ENCLOSING-OBJECT <template-pointer>)  
;;;    (TEMPLATE-ENCLOSER-OFFSET  <template-pointer>)      
;;;    (TEMPLATE-POINTER-SLOTS    <template-pointer>)        
;;;    (TEMPLATE-SCRATCH-SLOTS    <template-pointer>)        
;;;    (TEMPLATE-INTERNAL-BIT?    <template-pointer>)        
;;;    HEADER/...
;;;
;;;      Simulator procedures that must shadow definitions in this file:
;;;    K-LIST
;;;        
;;;    (GC-COPY-PAIR pair) 
;;;        Copies PAIR into new space, putting a forwarding pointer in the cdr.
;;;    (GC-COPY-EXTEND obj size)
;;;        Copies an extend into new space.  OBJ is the extend, SIZE is the
;;;        length.  A forwarding pointer is put in the header of OBJ.
;;;    (GC-ERROR-MESSAGE string loc)
;;;        Print an error message.
;;;
;;;      Simulator procedures that must shadow definitions in T system:
;;;    CAR, CDR, LIST?, EXTEND?, VECTOR-LENGTH, NULL?,
;;;    IMMEDIATE?, TEMPLATE?, BYTEV?, BYTEV-LENGTH?

;;; 3/14/86:
;;;  Flushed statistics other than the object count.
;;;  MOVE-OBJECT does the range check before anything else.
;;;  Old-space limits are in variables and not in a structure.

;;; To do:
;;;  Vcells and weaks flushed in favor of weak-sets, weak-alists and
;;;    weak-tables.

;;;   The top level procedure.  O-LOC is an extend containing a pointer to the
;;; object to be copied.  This is overwritten by a pointer to the new copy.
;;;   This procedure dispatches on the tag.  Nonpointers and nonrelocating
;;; pointers are left alone.  Pairs are checked to see if the cdr contains a
;;; forwarding pointer.  Extends require further dispatch.  The M68000 requires
;;; the TEMPLATE-HEADER? check first since the other extend tests are not
;;; valid on templates.  Extends are then checked for a forwarding pointer.

(define (move-object o-loc)
  (let* ((obj (extend-header o-loc))
         (fxobj (descriptor->fixnum obj)))
    (if (not (and (fx>= fxobj *old-space-begin*)
                  (fx< fxobj *old-space-frontier*)))
        (pop-k-list)
        (xselect (descriptor-tag obj)
          ((tag/fixnum tag/immediate)
           (pop-k-list))
          ((tag/pair)
           (cond ((and (list? (cdr obj)) ; This is a safety check
                       (in-new-space? (cdr obj)))
                  (set (extend-header o-loc) (cdr obj))
                  (pop-k-list))
                 (else
                  (move-pair obj o-loc))))
          ((tag/extend)
           (let ((header (extend-header obj)))
             (cond ((template-header? header)   ; 68000 requires this first
                    (move-template obj o-loc))
                   ((extend? header)
                    (cond ((in-new-space? header)
                           (set (extend-header o-loc) (extend-header obj))
                           (pop-k-list))
                          ((template? header)
                           (move-closure obj o-loc))
                          (else
                           (gc-error-message "header is a non-template extend" o-loc)
                           (pop-k-list))))
                   ((immediate? header)
                    (move-immediate-object obj o-loc))
                   (else
                    (gc-error-message "corrupt header" o-loc)
                    (pop-k-list)))))))))

;;;   The K-LIST is a list of partially copied objects that are linked together
;;; in old space.  This cannot be a normal global variable as the GC would
;;; attempt to move it into new space.

(define-constant k-list
  (object (lambda ()
            (process-global task/k-list))
    ((setter self)
     (lambda (k)
       (set (process-global task/k-list) k)))))

;;;   Pop the next thing off the list and move it.  If it is a pair,
;;; remove it from the K-list and call MOVE-OBJECT to copy the cdr.
;;; Otherwise, (extend-elt K 1) contains the index of the next pointer
;;; to be copied. If there are none to be copied then remove the
;;; extend from the K-list and recur; otherwise, decrement the
;;; pointer and call MOVE-OBJECT to do the copying.

(define (pop-k-list)
  (let ((next (k-list)))
    (cond ((null? next)
           '#t)   ; The only (non-error) return in the GC.
          ((list? next)
           (let* ((fwd (cdr next))
                  (to-copy (if (list? fwd) (gc-pair->extend fwd) fwd)))
             (set (k-list) (car next))
             (move-object (make-pointer to-copy 0))))
          ((fx< (extend-elt next 1) 0)
           (set (k-list) (extend-elt next 0))
           (pop-k-list))
          (else
           (let ((offset (fx- (extend-elt next 1) 1))
                 (forward (extend-header next)))
             (set (extend-elt next 1) offset)
             (move-object (make-pointer forward offset)))))))

;;;   Forward OBJ using copy-pair.  Push the old pair onto the k-list.
;;; Set the contents of O-LOC to the forwarded pair.  Then recursively
;;; move the car of the forwarded pair The object in the cdr will
;;; be moved when the k-list is popped.

(define (move-pair obj o-loc)
  (let* ((new (gc-copy-pair obj))
         (xnew (gc-pair->extend new)))
    (set (car obj) (k-list))
    (set (k-list) obj)
    (set (extend-header o-loc) new)
    (move-object xnew)))

;;;   Forward OBJ which is an extend of SIZE longwords with NDESC
;;; descriptor slots. (Note: Descriptor slots are always the first
;;; slots of an extend.) O-LOC is the location into which the descriptor
;;; (+ FORWARDED-OBJECT E-OFF) should be stored.
;;;   OBJ is forwarded by the primitive COPY-EXTEND which copies the
;;; old object into the new area.  A forwarding pointer is put in the
;;; header of the old object.
;;;   If there are zero descriptors pop the k list.  If there is a
;;; single descriptor, move it.  If there is more than one descriptor,
;;; link the object into the K-list, put the number of addresses
;;; into (extend-elt obj 2), and move the first address.  Closures with
;;; only one slot are treated as pairs.

(define (move-extend obj size ndesc o-loc e-off)
  (let ((new (gc-copy-extend obj size)))
    (set (extend-header o-loc) (make-pointer new e-off))
    (cond ((fx> ndesc 1)
           ;; Push obj onto K list, and set slot-offset.
           (set (extend-elt obj 0) (k-list))
           (set (k-list) obj)
           (let ((last-elt (fx- ndesc 1)))
             (set (extend-elt obj 1) last-elt)
             (move-object (make-pointer new last-elt))))
          ((closure? new) ; OBJ's header is now a forwaring pointer.
           (xcond ((fx= ndesc 0)
                   (move-object new))
                  ((fx= ndesc 1)
                   (set (extend-elt obj 0) (k-list))
                   (set (k-list) (gc-extend->pair obj))
                   (move-object new))))
          (else
           (xcond ((fx= ndesc 0)
                   (pop-k-list))
                  ((fx= ndesc 1)
                   (move-object (make-pointer new 0))))))))

;;;   There are 3 types of templates: code vector, closure internal,
;;; and dynamic.  All templates are enclosed in other objects.

(define (move-template obj o-loc)
  (let ((encloser (template-enclosing-object obj))
        (offset   (template-encloser-offset  obj)))
    (move-internal-object encloser (fx- offset 1) o-loc)))

;;;   This procedure is only called on heap closures since stack closures
;;; are traced and not copied.  If the closure is internal to another object
;;; then the enclosing object is moved, otherwise, it is moved as a normal
;;; extend.

(define (move-closure obj o-loc)
  (let ((template (extend-header obj)))
    (cond ((template-internal-bit? template)
           (let ((encloser  (closure-enclosing-object obj))
                 (offset    (closure-encloser-offset obj)))
             (move-internal-object encloser (fx- offset 1) o-loc)))
          (else
           (let* ((ptrs (template-pointer-slots template))
                  (size (fx+ ptrs (template-scratch-slots template))))
             (move-extend obj size ptrs o-loc -1))))))

;;;   Move ENCLOSER which was traced through an internal pointer with an offset
;;; of OFFSET.  Dispatch on the location and type of ENCLOSER.

(define (move-internal-object encloser offset o-loc)
  (let ((header (extend-header encloser)))
    (cond ((and (extend? header)
                (in-new-space? header))
           (set (extend-header o-loc) (make-pointer header offset))
           (pop-k-list))
          ((bytev? encloser)
           (set (extend-header o-loc)
                (make-pointer (gc-copy-extend encloser (bytev-cells encloser))
                              offset))
           (pop-k-list))
          ((unit? encloser)
           (let ((size (unit-length encloser)))
             (move-extend encloser size size o-loc offset)))
          ((template? header)
           (let* ((ptrs (template-pointer-slots header))
                  (size (fx+ ptrs (template-scratch-slots header))))
             (move-extend encloser size ptrs o-loc offset)))
          (else
           (gc-error-message "corrupt internal object" o-loc)
           (pop-k-list)))))

;;;   Find out whether a value has been copied into the new heap and return a
;;; a flag and the new location.  The flag is true if the object was indeed
;;; retained.  This is a simpler version of MOVE-OBJECT.  Symbols are always
;;; copied.

(define (get-new-copy obj)
  (let ((fxobj (descriptor->fixnum obj)))
    (if (not (and (fx>= fxobj *old-space-begin*)
                  (fx< fxobj *old-space-frontier*)))
        (return t obj)
        (xselect (descriptor-tag obj)
          ((tag/fixnum tag/immediate)
           (return t obj))
          ((tag/pair)
           (if (and (list? (cdr obj))
                    (in-new-space? (cdr obj)))
               (return t (cdr obj))
               (return nil nil)))
          ((tag/extend)
           (let ((header (extend-header obj)))
              (cond ((extend? header)
                     (get-new-extend-copy obj header))
                    ((symbol? obj)
                     (return t (gc-copy-object obj)))
                    (else
                     (return nil nil)))))))))

(define (get-new-extend-copy obj header)
  (cond ((template-header? header)   ; 68000 requires this first
         (receive (traced? new-loc)
                  (get-new-copy (template-enclosing-object obj))
           (if traced?
               (return t (make-pointer new-loc
                                       (fx- (template-encloser-offset obj) 1)))
               (return nil nil))))
        ((in-new-space? header)
         (return t (extend-header obj)))
        ((template-internal-bit? header)
         (receive (traced? new-loc)
                  (get-new-copy (closure-enclosing-object obj))
           (if traced?
               (return t (make-pointer new-loc
                                        (fx- (closure-encloser-offset obj) 1)))
               (return nil nil))))
        (else
         (return nil nil))))

;;; Copy an object and return the new pointer

(define copy-object-cell
  (make-vector 1))

(define (gc-copy-object thing)
  (set (vref copy-object-cell 0) thing)
  (move-object (make-pointer copy-object-cell 0))
  (vref copy-object-cell 0))

;;; Procedures for moving the immediate extends.

(define (move-error obj o-loc)
  (ignore obj)
  (gc-error-message "no method for an immediate" o-loc)
  (pop-k-list))

(define (move-bytes obj o-loc)
  (set (extend-header o-loc) (gc-copy-extend obj (bytev-cells obj)))
  (pop-k-list))

(define (move-foreign obj o-loc)
  (move-extend obj 2 1 o-loc -1))

(define (move-general-vector obj o-loc)
  (let ((len (vector-length obj)))
    (move-extend obj len len o-loc -1)))

(define (move-unit obj o-loc)
  (let ((len (unit-length obj)))
    (move-extend obj len len o-loc -1)))

(define (move-string-slice obj o-loc)
  (move-extend obj 2 1 o-loc -1))

(define (move-cell obj o-loc)
  (move-extend obj 1 1 o-loc -1))

;;; Bignums contain only fixnums and thus do not need to be traced.

(define (move-bignum obj o-loc)
  (set (extend-header o-loc) (gc-copy-extend obj (bignum-length obj)))
  (pop-k-list))

;;; Stacks must be scanned.

(define (move-stack obj o-loc)
  (let ((new (gc-copy-extend obj (stack-length obj))))
    (set (extend-header o-loc) new)
    (real-scan (make-pointer new 0)
               (fx+ (descriptor->fixnum new)
                    (fx- (stack-length new) 1))
               stack-trace-proc)
    t)) ; GC returns from here if there were any stacks copied.

;;; Floats

(define (move-double-float obj o-loc)
  (set (extend-header o-loc)
       (gc-copy-extend obj 2))
  (pop-k-list))

(define (move-single-float obj o-loc)
  (set (extend-header o-loc)
       (gc-copy-extend obj 1))
  (pop-k-list))
                      
(define (move-vcell obj o-loc)
  (move-extend obj %%vcell-size %%vcell-size o-loc -1))

;;; Weak sets

(define (bogus-move-weak-set obj o-loc)
  (move-extend obj 1 1 o-loc -1))

(define (move-weak-set obj o-loc)
  (cond ((weak-semaphore-set? obj)
         (move-extend obj 1 1 o-loc -1))
        (else
         (let ((new (gc-copy-extend obj 1)))
           (set (extend-header o-loc) new)
           (set (extend-header new) (gc-weak-set-list))
           (set (gc-weak-set-list) new)
           (pop-k-list)))))

;;; The code for weak alists is just like the code for weak sets.

(define (bogus-move-weak-alist obj o-loc)
  (move-extend obj 1 1 o-loc -1))

(define (move-weak-alist obj o-loc)
  (cond ((weak-semaphore-set? obj)
         (move-extend obj 1 1 o-loc -1))
        (else
         (let ((new (gc-copy-extend obj 1)))
           (set (extend-header o-loc) new)
           (set (extend-header new) (gc-weak-alist-list))
           (set (gc-weak-alist-list) new)
           (pop-k-list)))))

;;; Weak Tables

;;; WEAK-TABLE-TABLE must be the first slot in a WEAK-TABLE

(define (bogus-move-table obj o-loc)
  (move-extend obj 2 2 o-loc -1))

(define (move-weak-table obj o-loc)
  (cond ((weak-semaphore-set? obj)
         (move-extend obj 2 2 o-loc -1))
        (else
         (exchange (weak-table-vector obj)
                   (%table-vector (weak-table-table obj)))
         (let ((new (gc-copy-extend obj 2)))
           (set (extend-header o-loc) new)
           (set (extend-header new) (gc-weak-table-list))
           (set (gc-weak-table-list) new)
           (move-object (make-pointer new 0))))))

(define (move-weak-cell obj o-loc)
  (set (weak-cell-contents obj) '#f)
  (move-extend obj 1 1 o-loc -1))

;;; Moving immediates

(define (move-immediate-object obj o-loc)
  ((vref gc-dispatch-vector (header-type (extend-header obj)))
   obj o-loc))

(define gc-dispatch-vector (make-vector %%number-of-immediate-types))

(let ((gc-copiers
      `(
        (,header/text           ,move-bytes)
        (,header/general-vector ,move-general-vector)
        (,header/unit           ,move-unit)
        (,header/slice          ,move-string-slice)
        (,header/symbol         ,move-bytes)
        (,header/bytev          ,move-bytes)
        (,header/foreign         ,move-foreign)
        (,header/template       ,move-template)
        (,header/cell           ,move-cell)
        (,header/bignum         ,move-bignum)
        (,header/stack          ,move-stack)
        (,header/double-float   ,move-double-float)
        (,header/single-float   ,move-single-float)
        (,header/vcell          ,move-vcell)
        (,header/weak-set       ,move-weak-set)
        (,header/weak-alist     ,move-weak-alist)
        (,header/weak-table     ,move-weak-table)
        (,header/weak-cell      ,move-weak-cell)
       ; (,header/task           ,move-error)
       ; (,header/true           ,move-error)
       ; (,header/char           ,move-error)
       ; (,header/bitv           ,move-bitv)
       ; (,header/vframe         ,move-error) only on stack
       ; (,header/short-float    ,move-error) unimplemented
        )))
  (vector-fill gc-dispatch-vector move-error)
  (walk (lambda (x) (set (vector-elt gc-dispatch-vector
                                     (fixnum-ashr (car x) 2))
                         (cadr x)))
        gc-copiers))

;;; Three little utilities.

(define (gc-copy-pair pair)
  (gc-count-message)
  (let ((new (cons (car pair) (cdr pair))))
    (set (cdr pair) new)
    new))

(define (gc-copy-extend obj size)
  (gc-count-message)
  (let ((new (%make-extend (extend-header obj) size)))
    (%copy-extend new obj size)
    (set (extend-header obj) new)
    new))

(define (bytev-cells bytev)
  (fixnum-ashr (fx+ (bytev-length bytev) 3) 2))

;;; Statistics and messages.

(lset *gc-object-count* 0)       ;;; objects copied up to last message
(lset *gc-click* 0)              ;;; objects copied since last message
(lset *gc-message-frequency* 10000)

(define (initialize-gc-stats)
  (set *gc-click* 0)
  (set *gc-object-count* 0))
       
(define-constant (gc-count-message)
  (set *gc-click* (fx+ *gc-click* 1))
  (cond ((fx>= *gc-click* *gc-message-frequency*)
         (set *gc-object-count* (fx+ *gc-object-count* *gc-click*))
         (set *gc-click* 0)    
         (gc-message *gc-object-count*))))
