(herald (front_end node)
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

;;; Creating nodes and CPS converting calls and blocks.
;;;============================================================================

;;; Two useful ways of calling ->NODE:

;;; Convert EXP into a value node or else.

(define (->value-node exp syntax shape)
  (receive (exp-node c-parent c-role)
           (->node exp syntax shape)
    (ignore c-parent c-role)
    (if (call-node? exp-node)
        (bug '"alpha-value got a call-node ~S for ~S" exp-node exp))
    exp-node))

;;; Convert EXP into a call node.  If EXP converts to a value node then return
;;; a call that calls it's continuation with the converted EXP as the argument.

(define (->call-node exp syntax shape)
  (receive (node c-parent c-role)
           (->node exp syntax shape)
    (cond ((call-node? node)
           (return node c-parent c-role))
          (else
           (let ((call (create-call-node '2 '0)))
             (relate (call-arg '1) call node)
             (return call call call-proc))))))

;;; A useful node building procedure:

;;; Make a thunk that returns that value of NODE when it is called.  C-PARENT
;;; and C-ROLE locate the continuation of NODE (if it has one).

(define (make-thunk node c-parent c-role)
  (let* ((var (create-variable 'k))
         (l-node (create-lambda-node 'c (flist1 var '()))))
    (cond ((call-node? node)
           (relate lambda-body l-node node)            
           (relate c-role c-parent (create-reference-node var)))
          (else
           (let ((call-node (create-call-node '2 '0)))
             (relate call-proc call-node (create-reference-node var))
             (relate (call-arg '1) call-node node)
             (relate lambda-body l-node call-node))))            
    l-node))

;;;                    CPS CONVERSION OF CALLS
;;;=============================================================================

;;; Turn a call into a node.  Just a front for CPS-ARGS.

(define (make-call exp syntax shape)
  (let* ((call (create-call-node (fx+ '1 (length exp)) '1))
         (top (cps-args call `(,(car exp) ,empty . ,(cdr exp)) syntax shape)))
    (return top call (call-arg '1))))

;;; Another front for CPS-ARGS.  This is called by various compilators.

(define (make-call-with-exits exits args syntax shape)
  (cps-args (create-call-node (length args) exits) args syntax shape))

;;; Structure to hold information about arguments to calls.

(define-structure-type arg
  index          ; The index of this argument in the call.
  rank           ; The estimated cost of executing this node at run time.
  node           ; What ->NODE returned for this argument.
  c-parent       ;   "
  c-role         ;   "
  )

(define (create-arg node index c-parent c-role)
  (let ((arg (obtain-from-pool *argument-pool*)))
    (set (arg-index    arg) index)
    (set (arg-rank     arg) (node-rank node))
    (set (arg-node     arg) node)
    (set (arg-c-parent arg) c-parent)
    (set (arg-c-role   arg) c-role)
    arg))

;;; Storage management for argument structures.

(define *argument-pool*
  (make-pool '*argument-pool* make-arg '20 arg?))

(define (return-arg-list args)
  (iterate loop ((args args))
    (cond ((null? args) nil)
          (else
           (let ((n (cdr args)))
             (return-to-pool *argument-pool* (car args))
             (return-to-freelist args)
             (loop n))))))

;;; The actual work of CPS conversion.  Takes a list of ARG structures and 
;;; builds them into a call, returning the top node of the resulting tree.
;;; If an argument is a call a continuation is made for it.
;;;
;;; ((p a1 a2) EMPTY (r (s b1 b2 b3) t))
;;; =>
;;; (EMPTY    (s C_1 b1 b2 b3))              ; Call to s
;;;   (C_1 () (V_2)  (r C_3 V_2 t))          ; Call to r
;;;     (C_3 () (V_4)  (p C_5 a1 a2))        ; Call to p
;;;       (C_5 () (V_6)  (V_6 EMPTY q V_4))  ; Call to result of (p a1 a2)

(define (cps-args call args syntax shape)
  (let ((arguments (make-arg-nodes args syntax shape)))
    (iterate loop ((top-node call) (args arguments))
      (cond ((null? args)
             (return-arg-list arguments)
             top-node)
            (else
             (let ((arg (car args)))
               (cond ((call-node? (arg-node arg))
                      (let* ((c-var (create-variable 'v))
                             (l-node (create-lambda-node
                                      'c (flist2 nil c-var '()))))
                        (relate lambda-body l-node top-node)
                        (relate (arg-c-role arg) (arg-c-parent arg) l-node)
                        (relate (call-arg (arg-index arg))
                                call
                                (create-reference-node c-var))
                        (loop (arg-node arg) (cdr args))))
                     (else
                      (relate (call-arg (arg-index arg)) call (arg-node arg))
                      (loop top-node (cdr args))))))))))

;;; Convert the elements of EXP into nodes (if they aren't already) and put
;;; them into an ARG structure.  Returns the list of ARG structure sorted
;;; by ARG-RANK.

(define (make-arg-nodes exp syntax shape)
  (let ((do-arg (lambda (arg index)
                  (receive (node c-parent c-role)
                           (->node arg syntax shape)
                    (create-arg node index c-parent c-role)))))
    (do ((i '0 (fx+ i '1))
         (args exp (cdr args))
         (vals '() (if (not (empty? (car args)))
                       (cons-from-freelist (do-arg (car args) i) vals)
                       vals)))
        ((null? args)
         (sort-list! vals
                     (lambda (v1 v2) (fx< (arg-rank v1) (arg-rank v2))))))))

;;; Complexity analysis used to order argument evaluation.  More complex
;;; arguments are to be evaluated first.  This is a simple heuristic.

(define (node-rank node)
  (if (or (empty? node)
          (not (call-node? node)))
      '0
      (complexity-analyze node)))

(define (complexity-analyze node)
  (cond ((empty? node)
         '0)
        ((reference-node? node)
         (if (get-variable-definition (reference-variable node)) '0 '1))
        ((leaf-node? node) '0)
        ((lambda-node? node)
         (complexity-analyze (lambda-body node)))
        ((call-node? node)
         (let ((q (complexity-analyze-list (call-proc+args node))))
           (set (call-complexity node) q)
           q))
        ((object-node? node)
         (let ((q1 (complexity-analyze (object-proc node)))
               (q2 (complexity-analyze-list (object-operations node)))
               (q3 (complexity-analyze-list (object-methods node))))
           (fx+ q1 (fx+ q2 q3))))
        (else
         (bug '"funny node ~S" node))))

(define (complexity-analyze-list list)
  (do ((q '0 (fx+ q (complexity-analyze (car l))))
       (l list (cdr l)))
      ((null? l) q)))

;;; Convert an expression list into a block.  This is guarenteed to return a
;;; call node.  
;;;
;;; TOP-CALL is the root of the tree for the block.
;;; VALUE is the node for the previous expression if it didn't alphatize to a
;;; call.
;;; C-PARENT and C-ROLE are the latest continuation for the block.
;;;
;;; Each expression in passed to ->NODE in turn.  The resulting nodes are
;;; linked using n-ary continuation lambdas whose variables are never
;;; referenced.
;;;
;;; A non-call that is not the last expression in the block is ignored.
;;;
;;; (BLOCK (p a1 a2 ...)     
;;;        (q b1 b2 ...)
;;;        (r c1 c2 ...))
;;; =>
;;; (#empty#  (p B_1 a1 a2 ...))
;;;   (B_1 IGNORE_2 ()  (q B_3 b1 b2 ...))
;;;     (B_3 IGNORE_4 ()  (r #cont# c1 c2 ...))
;;;
;;; (BLOCK (p a1 a2 ...)     
;;;        i              ;;; This will disappear since the value is not used.
;;;        (q b1 b2 ...)
;;;        j)
;;; =>
;;; (#empty# (p B_1 a1 a2 ...))
;;;   (B_1 IGNORE_2 ()  (q B_3 b1 b2 ...))  
;;;     (B_3 IGNORE_4 ()  (#cont# j))

(define (make-block exp-list syntax shape)
  (iterate loop ((exps exp-list)
                 (value nil)
                 (top-call nil)
                 (c-parent nil)
                 (c-role nil))
    (cond ((and (null? exps) top-call (not value))
           (return top-call c-parent c-role))
          ((null? exps)
           (finish-block value top-call c-parent c-role))
          (else
           (if value (erase-all value))
           (receive (node n-parent n-role)
                    (->node (car exps) syntax shape)
             (cond ((not (call-node? node))
                    (loop (cdr exps) node top-call c-parent c-role))
                   ((not top-call)
                    (loop (cdr exps) nil node n-parent n-role))
                   (else
                    (let ((l-node (create-lambda-node
                                   'b
                                   (flist1 (create-variable 'ignore) '()))))
                      (relate lambda-body l-node node)
                      (relate c-role c-parent l-node)
                      (loop (cdr exps) nil top-call n-parent n-role)))))))))
           
;;; Create a call node to return the last expression in the block.

(define (finish-block value top-call c-parent c-role)
  (let ((call (create-call-node '2 '0))
        (value (if value value (create-primop-node primop/undefined))))
    (relate (call-arg '1) call value)
    (cond ((not top-call)
           (return call call call-proc))
          (else
           (let ((l-node (create-lambda-node
                          'b
                          (flist1 (create-variable 'ignore) '()))))
             (relate lambda-body l-node call)
             (relate c-role c-parent l-node)
             (return top-call call call-proc))))))
                     



                            

