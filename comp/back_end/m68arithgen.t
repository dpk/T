(herald (back_end m68arithgen)
  (env t (orbit_top defs) (back_end bookkeep)))

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

(define (m68-op op)
  (xcase op                
    ((mov) m68/move)
    ((add) m68/add)
    ((sub) m68/sub)
    ((and) m68/and)
    ((cmp) m68/cmp)
    ((or)  m68/or)
    ((xor) m68/eor)))
                                             
(define (m68-size rep)
  (xcase rep
    ((rep/char rep/integer-8-u rep/integer-8-s) .b)
    ((rep/integer-16-u rep/integer-16-s) .w)
    ((rep/pointer rep/integer) .l)))


(define (fixnum-comparator node inst)       
  (comparator node inst fixnum?))

(define (character-comparator node inst)
  (comparator node inst char?))

(define (comparator node inst type)
  (destructure (((then else () ref1 ref2) (call-args node)))
    (let* ((val1 (leaf-value ref2))     ;; ARGHH opposite of VAX for cond branch
           (val2 (leaf-value ref1))
           (rep (cond ((and (variable? val1) 
                            (neq? (variable-rep val1) 'rep/pointer))
                       (variable-rep val1))
                      ((variable? val2) (variable-rep val2))
                      (t 'rep/pointer))))
      (let ((access2 (access-with-rep node val2 rep)))
        (protect-access access2)
        (let ((access1 (access-with-rep node val1 rep)))
          (cond ((register? access1)
                 (emit m68/cmp (m68-size rep) access2 access1)
                 (emit-jump (get-jop inst rep t) else then))
                ((register? access2)
                 (emit m68/cmp (m68-size rep) access1 access2)
                 (emit-jump (get-jop inst rep nil) else then))
                ((and (type val2) (not (type val1)))
                 (emit m68/cmp (m68-size rep) access2 access1)
                 (emit-jump (get-jop inst rep t) else then))
                ((and (type val1) (not (type val2)))
                 (emit m68/cmp (m68-size rep) access1 access2)
                 (emit-jump (get-jop inst rep nil) else then))
                (t
                 (emit m68/move (m68-size rep) access2 SCRATCH)
                 (emit m68/cmp (m68-size rep) access1 SCRATCH)
                 (emit-jump (get-jop inst rep nil) else then))))
        (release-access access2)))))
                                                
(define (get-jop inst rep reverse?)
  (xcase inst 
    ((jneq) jump-op/jn=)
    ((jgeq)
     (case rep
       ((rep/char rep/integer-8-u rep/integer-16-u)  ;;; unsigned guys
        (if reverse? jump-op/uj<= jump-op/uj>=))
       (else
         (if reverse? jump-op/j<= jump-op/j>=))))))
            

(define (generate-char->ascii node)
  (destructure (((cont arg) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let ((var (leaf-value arg))
            (t-reg (get-target-register node t-spec))) 
        (lock t-reg)            
        (cond ((variable? var)
               (let ((acc (access-value node var)))
                 (unlock t-reg)
                 (kill-if-dying var node)
                 (case (variable-rep var)
                   ((rep/char)
                    (really-rep-convert node acc 'rep/integer-8-u
                                                t-reg t-rep))
                   (else
                    (let ((reg (if (eq? (reg-type t-reg) 'scratch)
                                   t-reg
                                   SCRATCH)))
                      (generate-move acc reg)
                      (emit m68/lsr .w (machine-num (if (eq? t-rep 'rep/pointer)
                                                        6 8))
                                       reg)
                      (generate-move reg t-reg))))))
              (else
               (emit m68/move .l 
                     (access-with-rep node (char->ascii var) t-rep) 
                     t-reg)))
          (mark-continuation node t-reg)))))

(define (generate-ascii->char node)
  (destructure (((cont arg) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let ((var (leaf-value arg))
            (t-reg (get-target-register node t-spec)))
        (lock t-reg)                        
        (cond ((variable? var)
               (let ((acc (access-value node var)))
                 (unlock t-reg)
                 (kill-if-dying var node)
                 (case (variable-rep var)
                   ((rep/pointer)
                    (case t-rep 
                      ((rep/pointer)
                       (let ((reg (if (eq? (reg-type t-reg) 'scratch)
                                      t-reg SCRATCH)))
                         (generate-move acc reg)
                         (emit m68/asl .w (machine-num 6) reg)
                         (emit m68/and .l (machine-num #xffff) reg)
                         (emit m68/move .b (machine-num header/char) reg)
                         (generate-move reg t-reg)))
                      (else                         
                       (generate-move acc t-reg)
                       (emit m68/lsr .w (machine-num 2) t-reg))))
                   (else
                    (case t-rep
                      ((rep/pointer)
                       (let ((reg (if (eq? (reg-type t-reg) 'scratch)
                                      t-reg SCRATCH)))
                         (emit m68/move (m68-size (variable-rep var)) acc reg)
                         (emit m68/asl .w (machine-num 8) reg)
                         (emit m68/and .l (machine-num #xffff) reg)
                         (emit m68/move .b (machine-num header/char) reg)
                         (generate-move reg t-reg)))
                      (else
                       (if (neq? acc t-reg)
                           (emit m68/move (m68-size (variable-rep var)) acc t-reg))))))))
              (else
               (emit m68/move .l
                     (access-with-rep node (ascii->char var) t-rep)
                     t-reg)))
          (mark-continuation node t-reg)))))

(define (generate-fixnum-binop node inst commutes? strange?)
 (case inst 
   ((ashl ashr) (do-ash node inst))
   ((and or xor) (do-logical node inst))
   (else
  (destructure (((cont right left) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((lvar (leaf-value left))
             (rvar (leaf-value right))
             (l-acc (access-with-rep-reg node lvar t-rep t-spec)))
        (protect-access l-acc)
        (let ((r-acc (access-with-rep-reg node rvar t-rep t-spec)))
          (release-access l-acc) 
          (let ((l-target? (and (register? l-acc) 
                                (dying? lvar node) 
                                commutes?))
                (r-target? (and (register? r-acc) 
                                (dying? rvar node))))
            (cond ((and l-target?
                        (or (not r-target?) 
                            (eq? t-spec l-acc)))
                   (emit (m68-op inst) (m68-size t-rep) r-acc l-acc)
                   (kill lvar)
                   (mark-continuation node l-acc))
                  (r-target?
                   (emit (m68-op inst) (m68-size t-rep) l-acc r-acc)
                   (kill rvar)
                   (mark-continuation node r-acc))
                  (else
                   (protect-access l-acc)
                   (let ((t-reg (cond ((not (register? t-spec))
                                       (get-register t-spec node '*))
                                      ((and (not (locked? t-spec))
                                            (maybe-free t-spec cont))
                                        t-spec)
                                      (else
                                       (get-register (reg-type t-spec) node '*)))))
                     (release-access l-acc)
                     (emit m68/move (m68-size t-rep) r-acc t-reg)
                     (emit (m68-op inst) (m68-size t-rep) l-acc t-reg)                                                                       
                     (mark-continuation node t-reg))))))))))))

(define (do-logical node inst)
  (destructure (((cont right left) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((lvar (leaf-value left))
             (rvar (leaf-value right))
             (l-acc (access-with-rep-reg node lvar t-rep t-spec)))
        (protect-access l-acc)
        (let ((r-acc (access-with-rep-reg node rvar t-rep t-spec)))
          (cond ((and (register? l-acc) 
                      (eq? (reg-type l-acc) 'scratch)
                      (dying? lvar node))
                 (cond ((or (and (register? r-acc) 
                                 (eq? (reg-type r-acc) 'pointer))
                            (and (eq? inst 'xor)
                                 (not (register? r-acc))
                                 (not (fixnum? rvar))))
                        (emit m68/move (m68-size t-rep) r-acc SCRATCH)
                        (emit (m68-op inst) (m68-size t-rep) SCRATCH l-acc))
                       (else
                        (emit (m68-op inst) (m68-size t-rep) r-acc l-acc)))
                 (release-access l-acc)
                 (kill lvar)
                 (mark-continuation node l-acc))
                ((and (register? r-acc) 
                      (eq? (reg-type r-acc) 'scratch)
                      (dying? rvar node))
                 (cond ((or (and (register? l-acc) 
                                 (eq? (reg-type l-acc) 'pointer))
                            (and (eq? inst 'xor)
                                 (not (register? l-acc))
                                 (not (fixnum? lvar))))
                        (emit m68/move (m68-size t-rep) l-acc SCRATCH)
                        (emit (m68-op inst) (m68-size t-rep) SCRATCH r-acc))
                       (else
                        (emit (m68-op inst) (m68-size t-rep) l-acc r-acc)))
                 (release-access l-acc)
                 (kill rvar)
                 (mark-continuation node r-acc))
                (else
                 (let ((t-reg (if (and (register? t-spec) 
                                       (eq? (reg-type t-spec) 'scratch)
                                       (maybe-free t-spec cont))
                                  t-spec
                                  (get-register 'scratch node '*))))
                   (if (neq? r-acc t-reg)
                       (emit m68/move (m68-size t-rep) r-acc t-reg))
                   (cond ((or (and (register? l-acc) 
                                   (eq? (reg-type l-acc) 'pointer))
                              (eq? inst 'xor))
                          (emit m68/move (m68-size t-rep) l-acc SCRATCH)
                          (emit (m68-op inst) (m68-size t-rep) SCRATCH t-reg))
                         (else
                          (emit (m68-op inst) (m68-size t-rep) l-acc t-reg)))
                   (release-access l-acc)
                   (mark-continuation node t-reg)))))))))


(define (do-ash node inst)
  (destructure (((cont right left) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((lvar (leaf-value left))
             (rvar (leaf-value right))
             (l-acc (access-with-rep-reg node 
                                         lvar 
                                         'rep/integer
                                          t-spec)))
       (protect-access l-acc)
       (let ((r-acc (access-with-rep-reg node 
                                         rvar 
                                         (if (eq? t-rep 'rep/pointer)
                                             'rep/pointer 
                                             'rep/integer)
                                             t-spec)))
         (release-access l-acc)
         (let* ((r-reg (cond ((and (dying? rvar node) 
                                   (register? r-acc)
                                   (eq? (reg-type r-acc) 'scratch))
                              (kill rvar)
                              r-acc)
                             ((and (register? t-spec) 
                                   (eq? (reg-type t-spec) 'scratch)
                                   (not (reg-node t-spec)))
                              (emit m68/move .l r-acc t-spec)
                              t-spec)
                             (else
                              (protect-access l-acc)
                              (protect-access r-acc)
                              (let ((r (get-register 'scratch node '*)))
                                (release-access l-acc)
                                (release-access r-acc)
                                (emit m68/move .l r-acc r)
                                r))))
                (l-reg (cond ((and (fixnum? lvar) (fx<= lvar 8) (fx>= lvar 1))
                              (machine-num lvar))
                             ((and (register? l-acc)
                                   (eq? (reg-type l-acc) 'scratch))
                              l-acc)
                             (else
                              (emit m68/move .l l-acc SCRATCH)
                              SCRATCH))))
           (emit (xcase inst ((ashl) m68/asl) ((ashr) m68/asr)) .l l-reg r-reg)
           (if (and (eq? t-rep 'rep/pointer) 
                    (eq? inst 'ashr))
               (emit m68/and .b (machine-num #b11111100) r-reg))
           (mark-continuation node r-reg)))))))

