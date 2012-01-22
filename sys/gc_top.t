(herald gc_top
  (env tsys (osys gc)
            (osys gc_weak)       ;; for the GC-WEAK-???-LISTs
            (osys frame)         ;; vframe stuff (temporary)
            (osys table)))       ;; %TABLE-VECTOR must be integrated here

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

(lset *old-space* nil)
(lset *old-space-begin* nil)
(lset *old-space-frontier* nil)

(lset *new-space* nil)
(lset *new-space-begin* nil)
(lset *new-space-limit* nil)

(define-simple-switch gc-noisily? boolean? '#f)

(lset *pre-gc-agenda*
  (list pre-gc-fix-weak-sets
        pre-gc-fix-weak-alists
        pre-gc-fix-weak-tables
        ))

(lset *post-gc-agenda*
  (list post-gc-fix-weak-tables
        post-gc-fix-weak-sets     ; fix any new ones
        post-gc-fix-weak-alists
        object-unhash-post-gc
        ))

;;;  GC sensitive things:
;;;                     PRE         POST
;;;    weaks             +           +
;;;    vcells            +           +
;;;    populations                   +      GC-UPDATE-THE-POPULATIONS
;;;    tables
;;;    pools             +                  POOL-PRE-GC-HOOK
;;;    streams                       +
;;;    free list         +                  PAIR-FREELIST-PRE-GC-HOOK

(lset *gc-problem?*         nil)
(lset *gc-problem?-default* nil)

(define-operation (synch-area  area))
(define-operation (reset-area  area))
(define-operation (write-area  area))

(define-integrable (incr-area-frontier area length)
  (set (area-frontier area) (fx+ (area-frontier area) length)))

(define-integrable (area-extent area)
  (fx- (area-frontier area) (area-begin area)))

(define-structure-type area
  id               
  uid              ; for gc debugging (id,uid) must come first
  size
  base             ; base of area as an extend - see GC-FLIP
  begin            ; base of area as a fixnum
  frontier         ;++ changed from POINTER
  limit            ; consing beyond this point causes a GC
  (((reset-area self)
    (if (eq? self (current-area))
        (error "(reset-area ~s): area is current" self))
    (set (area-base self) 0)
    (zero-out-area self)
    (set (area-frontier self) (area-begin self)))
   ((synch-area self)
    (if (neq? self (current-area))
        (error "(synch-area ~s): area is not current" self))
    (set (area-frontier self) (process-global task/area-frontier)))
   ((write-area self fd)
    (vm-write-block fd (area-base self) (area-extent self)))
   ((print-type-string self) "Area")
   ((identification self) (area-id self))))

;++flush uid ar 

(define (create-area id begin size uid)
  (let ((area (make-area)))
    (set (area-begin area) begin)
    (set (area-frontier area) begin)
    (set (area-limit area) (fx+ begin size))
    (set (area-id area) id)
    (set (area-uid area) uid)
    (set (area-size area) size)
    area))

(define-integrable (current-area)
  (process-global task/area))

(define (area-space-remaining)
  (fx- (area-limit (current-area))
       (process-global task/area-frontier)))

(define (gc)
  (set (process-global task/doing-gc?) t)
  (really-gc)
  (clear-extra-registers)
  (set (process-global task/doing-gc?) nil)
  (return))

(define (really-gc)
  (let ((cc    (current-continuation))
        (z     *z?*))
    (set *z?* t)
    (set *gc-problem?* *gc-problem?-default*)
    (gc-write-line ";Beginning GC")
    (walk1 (lambda (item) (item)) *pre-gc-agenda*)
    (gc-write-line ";*PRE-GC-AGENDA* done")
    (gc-flip)
    (gc-write-line ";GC-FLIP done")
    (initialize-gc-stats)  ;;; Must come after flip
    (object-unhash-pre-gc) ;;; Must come after flip
    (set (system-global slink/pair-freelist) nil)
    (set (system-global slink/snapper-freelist) nil)
    (gc-write-line ";Starting to root")
    (gc-root cc)
    ;; The next line can't happen until after GC, when the area-object
    ;; has been moved to new space.
    (set (process-global task/area) *new-space*)
    (walk1 (lambda (item) (item)) *post-gc-agenda*)
    (gc-write-line ";*POST-GC-AGENDA* done")
    (gc-done)
    (gc-write-line ";GC done")
    (set *z?* z)
    (if *gc-problem?* (breakpoint 'really-gc t-implementation-env))))

(define (gc-flip)
  (exchange *old-space* *new-space*)
  (synch-area *old-space*)
  (set *old-space-begin* (area-begin *old-space*))
  (set *old-space-frontier* (area-frontier *old-space*))
  (set (process-global task/area-frontier) (area-begin *new-space*))
  (set (process-global task/area-begin) (area-begin *new-space*))
  (set (process-global task/area-limit) (area-limit *new-space*))
  (set (area-base *new-space*) (make-vector 0))
;  (advise-impure-area-access 'gc)
;  (advise-area-access *new-space* 'gc)
  )

(define (gc-done)
;  (advise-impure-area-access 'random)
;  (advise-area-access *new-space* 'random)
  (increment-gc-stamp)
  (reset-area *old-space*)
  (format t "; ~D objects copied~%" (fx+ *gc-click* *gc-object-count*))
  (let ((free (fx- (process-global task/area-limit)
                   (process-global task/area-frontier)))
        (total (fx- (process-global task/area-limit)
                    (process-global task/area-begin))))
    (format t ";Space Remaining: ~D left out of ~D (~D% free)~%"
              free total (fx/ (fx* 100 free) total))))

(define (gc-root stack)
  (real-scan (system-global slink/initial-impure-base)
             (system-global slink/initial-impure-memory-end)
             impure-trace-proc)
  (gc-write-line ";IIM traced")
  (real-scan stack (process-global task/stack) stack-trace-proc)
  (gc-write-line ";Stack traced")
  )

(define (impure-trace-proc ptr impure-offset ptrs scrs type)
  (ignore impure-offset scrs)
  (cond ((eq? type 'weak-cell)
         (set (weak-cell-contents ptr) nil))
        ((weak-semaphore-set? ptr)     ; Speed hack
         (move-object (make-pointer ptr -1))
         (trace-pointers ptr ptrs))
        ((eq? type 'weak-set)
         (set (extend-header ptr) (gc-weak-set-list))
         (set (gc-weak-set-list) ptr))
        ((eq? type 'weak-alist)
         (set (extend-header ptr) (gc-weak-alist-list))
         (set (gc-weak-alist-list) ptr))
        ((eq? type 'weak-table)
         (modify (%table-vector (weak-table-table ptr))
                 (lambda (v) (set (weak-table-vector ptr) v) nil))
         (set (extend-header ptr) (gc-weak-table-list))
         (set (gc-weak-table-list) ptr)
         (move-object (make-pointer ptr 0)))
        (else
         (move-object (make-pointer ptr -1))
         (trace-pointers ptr ptrs))))

(define (stack-trace-proc ptr stack-offset ptrs scrs type)
  (ignore scrs stack-offset)
  (cond ((eq? type 'fault-frame)
         (trace-fault-frame ptr))
        ((eq? type 'closure)
         (move-object (make-pointer ptr -1))
         (trace-pointers ptr ptrs))
        ((eq? type 'vframe)
         (trace-pointers ptr ptrs))
        (else
         (gc-write-line "; WARNING: weird thing on stack.")
         (set *gc-problem?* t))))

(define (trace-pointers obj ptrs)
  (do ((i 0 (fx+ i 1)))
      ((fx>= i ptrs) t)
    (move-object (make-pointer obj i))))

;;; True if an object is in old space.
(define (in-old-space? obj)
  (let ((obj (descriptor->fixnum obj)))
    (and (fx>= obj *old-space-begin*)
         (fx< obj *old-space-frontier*))))

;;; True if an object is in new space.
(define (in-new-space? obj)
  (let ((obj (descriptor->fixnum obj)))
    (and (fx>= obj (process-global task/area-begin))
         (fx< obj (process-global task/area-frontier)))))

;;; The following two procedures MUST NOT CONS AT ALL since there
;;; may be 0 heap space available.

(define (in-code-vector? pc bytev)
  (let ((dist (fx- (descriptor->fixnum pc) (descriptor->fixnum bytev))))
    (and (fx> dist 1)          ;;; Worry about this...
         (fx< dist (fixnum-ashr (bytev-length bytev) 2)))))

;;; This code is not GC safe - do not use it if DOING-GC? is false.

(define (pc-code-vector pc)
  (cond ((not (in-new-space? pc)) nil)   ; happens before gc-flip
        (else
         (iterate loop ((l (weak-set-elements code-population)))
           (cond ((null? l) nil)
                 ((in-code-vector? pc (car l))
                  (car l))
                 (else (loop (cdr l))))))))

(define (gc-write-line string)
  (fresh-line (error-output))
  (write-string (error-output) string)
  (newline (error-output)))

;;; This is not used in the GC but is used elsewhere....

(define-operation (gc-pointer-slots frame)
  (return (template-pointer-slots (extend-header frame)) 0))

(set (gc-present?) '#t)
