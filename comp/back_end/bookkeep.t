(herald (back_end bookkeep)
  (env t (orbit_top defs)))

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

;;; Copyright (c) 1985 David Kranz

(define (addressable? value)
  (or (fixnum? value)
      (char? value)
      (eq? value '#F)
      (eq? value '#T)))

(define-integrable (reg-offset x y) (cons x y))

(define-integrable (machine-num x) ($ x))


(define (lit x)
  (xcond ((fixnum? x)
          ($ (* x 4)))
         ((char? x)
          ($ (fx+ (fixnum-ashl (char->ascii x) 8) header/char)))))


(define-integrable (register? x)
  (and (fixnum? x) (fx>= x 0) (fx< x *real-registers*)))


;;; Registers and temps are represented in the same structure

(define-integrable reg-node
  (object (lambda (reg) 
            (vref *registers* reg))
          ((setter self) 
           (lambda (reg node)
             (vset *registers* reg node)))))
                         
(define-integrable temp-node reg-node)


(define (reg-type reg)
    (if (or (fx< reg *scratch-registers*)
            (fx>= reg (fx+ *real-registers* *pointer-temps*)))
        'scratch 
        'pointer))



;;; ->REGISTER Move the value of leaf-node REF into a register of type TYPE
;;; which can be either '* or a specific register. Force an existing value out
;;; if necessary,

(define (->register type node var where)
  (let ((accessor (access-value node var)))
    (cond ((and (register? accessor)
                (or (and (eq? (reg-type accessor) type)
                         (eq? where '*))
                    (eq? accessor where)))
           accessor)
          (else 
           (cond ((register? accessor)
                  (set (register-loc var) nil)
                  (cond ((locked? accessor)
                         (set (cdr (reg-node accessor)) nil))
                        (else
                         (set (reg-node accessor) nil)))))
           (into-register type node var accessor where)))))
          
(define (in-register? type value where)
  (let ((reg (register-loc value)))
    (and reg
         (eq? (reg-type reg) type)
         (or (eq? where '*)
             (eq? where reg)))))


(define (get-target-register node t-spec)
  (cond ((register? t-spec)
         (cond ((or (maybe-free t-spec ((call-arg 1) node))
                    (if (locked? t-spec)
                        (dying? (cdr (reg-node t-spec)) node)
                         (dying? (reg-node t-spec) node)))
                t-spec)
               (else
                (get-register (reg-type t-spec) node '*))))
        (else
         (get-register t-spec node '*))))

(lset get-register (lambda (type node where)
  (cond ((neq? where '*)
         (free-register node where)
         where)
        ((neq? type 'scratch)
         (really-get-register 'pointer node *scratch-registers* *real-registers* t))
        (else
         (really-get-register 'scratch node
                              0
                              *scratch-registers* t)))))

;(lset get-register (lambda (type node where)
;  (cond ((neq? where '*)
;         (free-register node where)
;         where)
;        ((neq? type 'scratch)
;         (iterate loop ((i AN))
;           (cond ((fx< i P)
;                  (if kick? (select-and-kick-register node 'pointer) nil))
;                 ((not (reg-node i))
;                  i)
;                 (else
;                  (loop (fx- i 1))))))
;        (else
;         (really-get-register 'scratch node
;                              0
;                              *scratch-registers* t)))))

(define (get-reg-if-free spec node)
  (xcond ((register? spec)
          (if (reg-node spec) nil spec))
         ((eq? spec 'pointer)
          (really-get-register spec node *scratch-registers* *real-registers* nil))
         ((eq? spec 'scratch)
          (really-get-register spec node 0 *scratch-registers* nil))
         ((eq? spec '*)
          (really-get-register spec node 0 *real-registers* nil))))

(define (really-get-register type node start stop kick?)
  (iterate loop ((i start))
    (cond ((fx>= i stop)
           (if kick? (select-and-kick-register node type) nil))
          ((not (reg-node i))
           i)
          (else
           (loop (fx+ i 1))))))

(define (into-register type node value access where)
  (cond ((in-register? type value where)
         (register-loc value))
        (else         
         (let ((reg (get-register type node where)))
           (generate-move access reg)
           (cond ((register-loc value)
                  => (lambda (reg)
                       (set (reg-node reg) nil))))
           (mark value reg)
           reg))))


;;; SELECT-AND-KICK-REGISTER The first register which is not locked or used soo
;;; is selected.  If none satisfy then the first register  is selected.
                                          
(define (select-and-kick-register node type)
  (cond ((eq? type 'pointer) 
         (iterate loop ((i (fx+ *scratch-registers* 1)) (default P)) ;kick P?
           (cond ((fx>= i *real-registers*)
                  (kick-register node default)
                  default)
                 ((locked? i) 
                  (loop (fx+ i 1) default))
                 ((not (used-soon? node (reg-node i)))
                  (kick-register node i) 
                  i)
                 (else (loop (fx+ i 1) i)))))
        (else
         (iterate loop ((i 0) (default nil))
           (cond ((fx>= i *scratch-registers*)
                  (free-register node default)
                  default)
                 ((locked? i) 
                  (loop (fx+ i 1) default))
                 ((not (used-soon? node (reg-node i))) 
                  (free-register node i)
                  i)
                 (else (loop (fx+ i 1) i)))))))
                                         

;;; USED-SOON? Is this variable used at this node or at one of its
;;; continuations?

(define (used-soon? node value)                                        
  (let ((var-used? (lambda (arg)
                      (and (leaf-node? arg)
                           (eq? (leaf-value arg) value)))))
     (or (any? var-used? (call-args node))
         (any? (lambda (cont)
                 (any? var-used? (call-args (lambda-body cont))))
               (continuations node)))))

(define-integrable (free-register node reg)
  (if (reg-node reg) (kick-register node reg)))

(define (maybe-free reg cont)
  (cond ((reg-node reg)
         => (lambda (var)
              (cond ((and (variable? var)
                          (lambda-node? cont)
                          (let ((spec (likely-next-reg var cont)))
                            (cond ((and (fixnum? spec)
                                        (not (reg-node spec)))
                                   (generate-move reg spec)   
                                   (set (reg-node reg) nil)
                                   (set (register-loc var) nil)
                                   (mark var spec)
                                   t)
                                  (else nil)))))
                     (else nil))))
         (else t)))



(define (kick-register node reg) 
  (let ((value (reg-node reg)))
    (cond ((locked? reg)
           (error "attempt to kick out of locked register"))
          ((or (temp-loc value)
               (not (variable? value)))
           (set (register-loc value) nil)
           (set (reg-node reg) nil))
          (else
           (let ((temp (get-temp value (reg-type reg) node)))
             (set (register-loc value) nil)
             (set (temp-loc value) temp)
             (set (reg-node reg) nil)
             (really-rep-convert node 
                                 reg 
                                 (variable-rep value) 
                                 temp 
                                 (variable-rep value)))))))



(define (really-get-temp type node)
  (cond ((eq? type 'scratch)
         (really-get-register 'scratch node
                              (fx+ *real-registers* *pointer-temps*)
                              *no-of-registers*
                              nil))
        (else
         (really-get-register 'pointer node
                              *real-registers*
                              (fx+ *real-registers* *pointer-temps*)
                              nil))))

(define (get-temp value type node)
  (cond ((really-get-temp type node)
         => (lambda (temp)
              (if (fx> temp *max-temp*)
                  (set *max-temp* temp))
              (set (temp-node temp) value)
              temp))
        (else
         (bug "all temps used"))))

(define-integrable (cont node)
  (car (call-args node)))
             
(define (continuations node)               
  (iterate loop ((i (call-exits node)) (args '()))
    (cond ((fx= i 0) args)
          (else
           (let ((arg ((call-arg i) node)))
             (loop (fx- i 1)
                   (cond ((lambda-node? arg) (cons arg args))
                         ((variable-known (leaf-value arg))
                          => (lambda (label) (cons label args)))
                         (else args))))))))

(define-integrable (then-cont node)
  (car (call-args node)))

(define-integrable (else-cont node)
  (cadr (call-args node)))

(define-integrable (kill-if-dying var node)
  (if (dying? var node) (kill var)))


(define (kill-if-dead node where)
  (cond ((lambda-node? node)
         (walk (lambda (var)
                 (if (not (or (memq? var (lambda-live where))
                              (fx= (variable-number var) 0)))
                     (kill var)))
               (lambda-live node)))
        ((or (not (variable? (leaf-value node)))
             (not (memq? (leaf-value node) (lambda-live where))))
         (kill (leaf-value node)))))

(define (kill value)
    (cond ((register-loc value)
           => (lambda (reg)
                (cond ((locked? reg)
                       (if (neq? (cdr (reg-node reg)) value)
                           (bug "horrible inconsistancy reg ~S value ~S"
                                 reg
                                 value))
                       (set (cdr (reg-node reg)) nil))
                      (else
                       (if (neq? (reg-node reg) value)
                           (bug "horrible inconsistancy reg ~S value ~S"
                                 reg
                                 value))
                       (set (reg-node reg) nil)))
                 (set (register-loc value) nil))))
    (cond ((temp-loc value)
           => (lambda (reg)
                (cond ((locked? reg)
                       (if (neq? (cdr (temp-node reg)) value)
                           (bug "horrible inconsistancy reg ~S value ~S"
                                 reg
                                 value))
                       (set (cdr (temp-node reg)) nil))
                      (else
                       (if (neq? (temp-node reg) value)
                           (bug "horrible inconsistancy reg ~S value ~S"
                                 reg
                                 value))
                       (set (temp-node reg) nil)))
                 (set (temp-loc value) nil)))))

(define (live? value node)                    
  (let ((value (cond ((and (pair? value) (variable? (cdr value)))
                      (cdr value))
                     ((variable? value) value)
                     (else nil))))
     (cond ((not value) nil)
	   ((eq? value (lambda-self-var *lambda*)) t)
           (else 
            (any? (lambda (cont)
                     (memq? value (lambda-live cont)))
                  (continuations node))))))

(define-integrable (dying? value node)
  (not (live? value node)))

(define (dead? value node)
  (let ((parent (node-parent node)))
    (not (and (variable? value)
              (or (memq? value (lambda-variables parent))
                  (memq? value (lambda-live parent)))))))

;;; pools for vector of registers (see ALLOCATE-CONDITIONAL-PRIMOP in reg.t)

(define register-vector-pool 
        (make-pool 'reg-vec-pool 
                   (lambda () (make-vector *no-of-registers*))
                   15
                   vector?))

(define-integrable (copy-registers)
  (vector-replace (obtain-from-pool register-vector-pool)
                  *registers*
                  *no-of-registers*))
                           
(define-integrable (return-registers)
  (return-to-pool register-vector-pool *registers*))

(define (restore-slots)
    (restore-registers)
    (restore-temps))

(define (restore-registers)
  (do ((i 0 (fx+ i 1)))
      ((fx>= i *real-registers* ))
    (cond ((reg-node i)
           (set (register-loc (reg-node i)) i)))))

(define (restore-temps)
  (do ((i *real-registers* (fx+ i 1)))
      ((fx>= i *no-of-registers* ))
    (cond ((temp-node i)
           (set (temp-loc (temp-node i)) i)))))



(define (clear-slots)
  (vector-fill *registers* nil)
  (recycle *locations*)
  (set *locations* (make-table 'locations)))
         
(define *lock-mark* (object nil ((identification self) 'lock)))


(define-integrable (lock reg)
  (if (fx< reg *no-of-registers*)
      (set (reg-node reg)
           (cons *lock-mark* (reg-node reg)))))

(define-integrable (unlock reg)
  (if (fx< reg *no-of-registers*)
      (set (reg-node reg)
           (cdr (reg-node reg)))))

(define-integrable (locked? reg)
  (let ((n (reg-node reg)))
    (and (pair? n) (eq? (car n) *lock-mark*))))

(define (protect-access access)
  (cond ((fixnum? access)
         (cond ((fx>= access 0)
                (lock access))))
        ((fg? access))
        ((register? (car access)) 
         (if (fxn= (car access) SP)
             (lock (car access))))
        ((pair? (car access))
         (lock (caar access))
         (lock (cdar access)))))
         
(define (release-access access)
  (cond ((fixnum? access)
         (cond ((fx>= access 0)
                (unlock access))))
        ((fg? access))
        ((register? (car access)) 
         (if (fxn= (car access) SP)
             (unlock (car access))))
        ((pair? (car access))
         (unlock (caar access))
         (unlock (cdar access)))))
              
(define (mark value reg)
  (set (reg-node reg) value)
  (if (register? reg)
      (set (register-loc value) reg)
      (set (temp-loc value) reg)))
         

(define (mark-temp value reg)
  (set (temp-node reg) value)
  (set (temp-loc value) reg))



;;; Locations
;;;==========================================================================
;;;   Keeps track of where values are.
;;; A table of a-lists of form ((<type-of-location> . <index>)...) indexed by
;;; leaf values, i.e. variables, primops, or literals.

(lset *locations* (make-table 'locations))

(define-integrable (leaf-locations value)
   (table-entry *locations* value))

(define-integrable register-loc
  (object (lambda (value)
            (get-location value 'reg))
    ((identification self) 'register-loc)
    ((setter self)
     (lambda (value reg)
       (if (null? reg)
           (clear-location value 'reg)
           (set-location value 'reg reg))))))

(define-integrable temp-loc
  (object (lambda (value)
            (get-location value 'temp))
    ((identification self) 'temp-loc)
    ((setter self)
     (lambda (value temp)
       (if (null? temp)
           (clear-location value 'temp)
           (set-location value 'temp temp))))))

(define-integrable (get-location value type)
  (cdr (assq type (leaf-locations value))))

(define (set-location value type number)
  (let ((locs (leaf-locations value)))
    (cond ((assq type locs)
           => (lambda (pair)
                (set (cdr pair) number)))
          (else
           (set-table-entry *locations* value (cons (cons type number) locs))))))

(define (clear-location value type)
  (let ((locs (leaf-locations value)))
    (set-table-entry *locations* value
         (del! (lambda (x y) (eq? x (car y))) type locs))
    nil))
