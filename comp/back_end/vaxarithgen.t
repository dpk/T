(herald (back_end vaxarithgen)
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

(define (vax-2op op size)
  (xcase op                
    ((mov) (select size
             ((size/byte) vax/movb)
             ((size/word) vax/movw)
             ((size/long) vax/movl)
             ((size/double) vax/movd)))
    ((add) (select size
             ((size/byte) vax/addb2)
             ((size/word) vax/addw2)
             ((size/long) vax/addl2)
             ((size/double) vax/addd2)))
    ((sub) (select size
             ((size/byte) vax/subb2)
             ((size/word) vax/subw2)
             ((size/long) vax/subl2)
             ((size/double) vax/subd2)))
    ((div) (select size
             ((size/byte) vax/divb2)
             ((size/word) vax/divw2)
             ((size/long) vax/divl2)
             ((size/double) vax/divd2)))
    ((andc) (select size
              ((size/byte) vax/bicb2)
              ((size/word) vax/bicw2)
              ((size/long) vax/bicl2)))
    ((mul) (select size
             ((size/byte) vax/mulb2)
             ((size/word) vax/mulw2)
             ((size/long) vax/mull2)
             ((size/double) vax/muld2)))
    ((cmp) (select size
             ((size/byte) vax/cmpb)
             ((size/word) vax/cmpw)
             ((size/long) vax/cmpl)
             ((size/double) vax/cmpd)))
    ((or)  (select size
             ((size/byte) vax/bisb2)
             ((size/word) vax/bisw2)
             ((size/long) vax/bisl2)))
    ((xor) (select size
             ((size/byte) vax/xorb2)
             ((size/word) vax/xorw2)
             ((size/long) vax/xorl2)))))
                                   

(define (vax-3op op size)
  (xcase op                
    ((add) (select size
             ((size/byte) vax/addb3)
             ((size/word) vax/addw3)
             ((size/long) vax/addl3)
             ((size/double) vax/addd3)))
    ((sub) (select size
             ((size/byte) vax/subb3)
             ((size/word) vax/subw3)
             ((size/long) vax/subl3)
             ((size/double) vax/subd3)))
    ((div) (select size
             ((size/byte) vax/divb3)
             ((size/word) vax/divw3)
             ((size/long) vax/divl3)
             ((size/double) vax/divd3)))
    ((andc) (select size
              ((size/byte) vax/bicb3)
              ((size/word) vax/bicw3)
              ((size/long) vax/bicl3)))
    ((mul) (select size
             ((size/byte) vax/mulb3)
             ((size/word) vax/mulw3)
             ((size/long) vax/mull3)
             ((size/double) vax/muld3)))
    ((or)  (select size
             ((size/byte) vax/bisb3)
             ((size/word) vax/bisw3)
             ((size/long) vax/bisl3)))
    ((xor) (select size
             ((size/byte) vax/xorb3)
             ((size/word) vax/xorw3)
             ((size/long) vax/xorl3)))))

(define (fixnum-comparator node inst)       
  (comparator node inst))

(define (character-comparator node inst)
  (comparator node inst))

(define (comparator node inst)
  (destructure (((then else () ref1 ref2) (call-args node)))
    (let* ((val1 (leaf-value ref1))
           (val2 (leaf-value ref2))
           (rep (cond ((and (variable? val1) 
                            (neq? (variable-rep val1) 'rep/pointer))
                       (variable-rep val1))
                      ((variable? val2) (variable-rep val2))
                      (t 'rep/pointer))))
        (let ((access (access-with-rep node val2 rep)))
          (protect-access access)
          (emit (vax-2op 'cmp (rep-size rep))
                (access-with-rep node val1 rep) 
                access)
          (emit-jump (get-jop inst rep) else then)
          (release-access access)))))
                                        
(define (get-jop inst rep)
  (xcase inst 
    ((jneq) jump-op/jn=)
    ((jgeq)
     (case rep
       ((rep/char rep/integer-8-u rep/integer-16-u)  ;;; unsigned guys
        jump-op/uj>=)
       (else
        jump-op/j>=)))))
            


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
                    (case t-rep 
                      ((rep/pointer)
                       (let ((s (get-register 'scratch node '*)))
                         (emit vax/movzbl acc s)
                         (emit vax/ashl (machine-num 2) s t-reg)))
                      (else
                       (emit vax/movzbl acc t-reg))))
                   (else
                    (case t-rep
                      ((rep/pointer)
                       (emit vax/ashl (machine-num -6) acc t-reg))
                      (else
                       (emit vax/ashl (machine-num -8) acc t-reg)))))))
              (else
               (emit vax/movl 
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
                       (emit vax/ashl (machine-num 6) acc t-reg)
                       (emit vax/bicl3 (machine-num #xffff0000) t-reg t-reg)
                       (emit vax/movb (machine-num header/char) t-reg))
                      (else
                       (emit vax/ashl (machine-num -2) acc t-reg))))
                   (else
                    (case t-rep
                      ((rep/pointer)
                       (emit vax/ashl (machine-num 8) acc t-reg)
                       (emit vax/bicl3 (machine-num #xffff0000) t-reg t-reg)
                       (emit vax/movb (machine-num header/char) t-reg))
                      (else
                       (if (neq? acc t-reg)
                           (emit (vax-2op 'mov (rep-size (variable-rep var)))
				 acc t-reg))))))))
              (else
               (emit vax/movl
                     (access-with-rep node (ascii->char var) t-rep)
                     t-reg)))
          (mark-continuation node t-reg)))))


(define (generate-fixnum-binop node inst commutes? strange?)
 (case inst 
   ((ash) (do-ash node))
   ((mul) (do-multiply node))
   ((div) (do-divide node))
   (else
  (destructure (((cont right left) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((lvar (leaf-value left))
             (rvar (leaf-value right))
             (l-acc (access-with-rep node lvar t-rep)))
        (protect-access l-acc)
        (let ((r-acc (access-with-rep node rvar t-rep)))
          (release-access l-acc)
          (cond ((and (register? l-acc) (dying? lvar node) commutes?)
                 (emit (vax-2op inst (rep-size t-rep)) r-acc l-acc)
                 (kill lvar)
                 (mark-continuation node l-acc))
                ((and (register? r-acc) (dying? rvar node))
                 (emit (vax-2op inst (rep-size t-rep)) l-acc r-acc)
                 (kill rvar)
                 (mark-continuation node r-acc))
                (else
                 (let ((t-reg (cond ((not (register? t-spec))
                                     (get-register t-spec node '*))
                                    ((and (not (locked? t-spec))
                                          (maybe-free t-spec cont))
                                     t-spec)
                                    (else
                                     (get-register (reg-type t-spec) node '*)))))
                   (emit (vax-3op inst (rep-size t-rep)) l-acc r-acc t-reg)                                                                       
                   (mark-continuation node t-reg)))))))))))


(define (do-multiply node)           
  (destructure (((cont right left) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let ((lvar (leaf-value left))
            (rvar (leaf-value right)))
        (receive (l-rep r-rep)
                 (if (eq? t-rep 'rep/pointer)
                     (cond ((variable? lvar) 
                            (if (eq? (variable-rep lvar) 'rep/pointer)
                                (return 'rep/pointer 'rep/integer)
                                (return 'rep/integer 'rep/pointer)))
                           ((and (variable? rvar)
                                 (eq? (variable-rep rvar) 'rep/pointer))
                            (return 'rep/integer 'rep/pointer))
                           (else
                            (return 'rep/pointer 'rep/integer)))
                     (return t-rep t-rep))
          (let ((l-acc (access-with-rep node lvar l-rep)))
            (protect-access l-acc)
            (let ((r-acc (access-with-rep node rvar r-rep)))
              (release-access l-acc)
              (cond ((and (register? l-acc) (dying? lvar node))
                     (emit (vax-2op 'mul (rep-size t-rep)) r-acc l-acc)
                     (kill lvar)
                     (mark-continuation node l-acc))
                    ((and (register? r-acc) (dying? rvar node))
                     (emit (vax-2op 'mul (rep-size t-rep)) l-acc r-acc)
                     (kill rvar)
                     (mark-continuation node r-acc))
                    (else
                     (let ((t-reg (cond ((not (register? t-spec) )
                                         (get-register t-spec node '*))
                                        ((and (not (locked? t-spec))
                                              (maybe-free t-spec cont))
                                         t-spec)
                                        (else
                                         (get-register (reg-type t-spec) node '*)))))
                       (emit (vax-3op 'mul (rep-size t-rep)) l-acc r-acc t-reg)                                                                       
                       (mark-continuation node t-reg)))))))))))

(define (do-divide node)           
  (destructure (((cont right left) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let ((lvar (leaf-value left))
            (rvar (leaf-value right)))
        (let ((l-acc (access-with-rep node lvar t-rep)))
          (protect-access l-acc)
          (let ((r-acc (access-with-rep node rvar t-rep)))
            (release-access l-acc)
            (cond ((eq? t-rep 'rep/pointer)
                   (let* ((t-reg (cond ((not (register? t-spec) )
                                        (get-register t-spec node '*))
                                       ((and (not (locked? t-spec))
                                             (maybe-free t-spec cont))
                                        t-spec)
                                       (else
                                        (get-register (reg-type t-spec) node '*))))
                          (scratch (if (eq? (reg-type t-reg) 'scratch)
                                       t-reg
                                       (get-register 'scratch node '*))))
                     (emit vax/divl3 l-acc r-acc scratch)
                     (emit vax/ashl (machine-num 2) scratch t-reg)
                     (mark-continuation node t-reg)))
                  ((and (register? r-acc) (dying? rvar node))
                   (emit (vax-2op 'div (rep-size t-rep)) l-acc r-acc)
                   (kill rvar)
                   (mark-continuation node r-acc))
                  (else
                   (let ((t-reg (cond ((not (register? t-spec) )
                                       (get-register t-spec node '*))
                                      ((and (not (locked? t-spec))
                                            (maybe-free t-spec cont))
                                       t-spec)
                                      (else
                                       (get-register (reg-type t-spec) node '*)))))
                     (emit (vax-3op 'div (rep-size t-rep)) l-acc r-acc t-reg)                                                                       
                     (mark-continuation node t-reg))))))))))

(define (do-ash node)           
  (destructure (((cont right left) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let ((lvar (leaf-value left))
            (rvar (leaf-value right)))
        (let ((l-acc (access-with-rep node lvar 'rep/integer)))
          (protect-access l-acc)
          (let ((r-acc (access-with-rep node rvar 'rep/integer)))
            (release-access l-acc)
            (cond ((rep-converter 'rep/integer t-rep)
                   => (lambda (converter)
                        (let* ((t-reg (cond ((not (register? t-spec) )
                                             (get-register t-spec node '*)) 
                                            ((and (not (locked? t-spec))
                                                  (maybe-free t-spec cont))
                                             t-spec)
                                            (else
                                             (get-register (reg-type t-spec)
                                                            node '*))))
                               (scratch (if (eq? (reg-type t-reg) 'scratch)
                                            t-reg
                                            (get-register 'scratch node '*))))
                          (emit vax/ashl l-acc r-acc scratch)
                          (converter node scratch t-reg)
                          (mark-continuation node t-reg))))
                  (else
                   (let ((t-reg (cond ((not (register? t-spec) )
                                       (get-register t-spec node '*))
                                      ((and (not (locked? t-spec))
                                            (maybe-free t-spec cont))
                                       t-spec)
                                      (else
                                       (get-register (reg-type t-spec) node '*)))))
                     (emit vax/ashl l-acc r-acc t-reg)       
                     (mark-continuation node t-reg))))))))))

 
(define (generate-two-fixnums node compare?)
  (destructure (((then else () ref1 ref2) (call-args node)))
    (let ((val1 (leaf-value ref1))
          (reg2 (let ((reg (get-register 'scratch node '*)))
                  (generate-move (access-with-rep node (leaf-value ref2)
						  'rep/pointer) reg)
                  reg)))
      (lock reg2)
      (let ((reg1 (let ((reg (get-register 'scratch node '*)))
                                (generate-move (access-with-rep node val1
								'rep/pointer) reg)
                                reg)))
	(lock reg1)
	(let ((scratch (get-register 'scratch node '*)))
	(unlock reg1)
        (unlock reg2)                 
        (generate-move reg1 SCRATCH)
        (if (variable? (leaf-value ref2)) 
            (emit vax/bisw2 reg2 SCRATCH))
        (emit vax/bitl (machine-num 3) SCRATCH)
        (emit-jump jump-op/jn= else then)      
        (or compare?
           (destructure (((arg1 arg2) (lambda-variables then)))
             (mark arg1 reg1)
             (mark arg2 reg2))))))))

(define (generate-op-with-overflow node op) 
  (destructure (((then else () ref1 ref2) (call-args node)))
    (let ((reg1 (register-loc (leaf-value ref1)))
          (reg2 (register-loc (leaf-value ref2))))
      (xcase op
	((add) (emit vax/addl2 reg2 reg1))
	((subtract) (emit vax/subl2 reg2 reg1))
	((multiply)
	 (emit vax/ashl ($ -2) reg1 reg1)
	 (emit vax/mull2 reg2 reg1)))
      (emit-jump jump-op/overflow then else)                               
      (kill (leaf-value ref1))
      (kill (leaf-value ref2))
      (mark (car (lambda-variables else)) reg1))))

(define (generate-hack-dr node op)
  (destructure (((#f ref1 ref2) (call-args node)))
    (let ((reg1 (register-loc (leaf-value ref1)))
          (reg2 (register-loc (leaf-value ref2))))
      (xcase op
	((divide)
	 (emit vax/divl2 reg2 reg1)
	 (emit vax/ashl ($ 2) reg1 reg1))
	((remainder)
	 (lock reg1)
	 (lock reg2)
	 (let ((temp (get-register 'scratch node '*)))
	   (unlock reg1)
	   (unlock reg2)
	   (emit vax/divl3 reg2 reg1 temp)
	   (emit vax/mull2 reg2 temp)
	   (emit vax/subl2 temp reg1))))
      (kill (leaf-value ref1))
      (kill (leaf-value ref2))
      (mark-continuation node reg1))))







