(herald (front_end simplify_let)
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

;;; Simplifying LET nodes, i.e. any call with a lambda node as the procedure.
;;; Many of these not created by the LET macro itself.

;;; This is the first pass at simplifying a LET node.
;;;  (CALL-PROC CALL) == PROC which is a lambda node.  This does as many
;;; substitutions as possible without simplifying the body of PROC.  If any
;;; arguments remain unsubstituted then REALLY-SIMPLIFY-LET is called.
;;;
;;; Compiling STRING.T this removed 192 out of 350 LET nodes (that reached
;;; the ELSE clause) without resorting to REALLY-SIMPLIFY-LET.  Compiling 
;;; EVAL.T 775 out of 1253 LET nodes were removed.

(define (simplify-let proc call)
  (cond ((not (arg-check-of-lambda proc call))
         (walkcdr simplify (call-args call)) ; Just looking for more errors
         (simplify-call proc)
         (fix-call-to-lambda call proc)
         t)
        ((and (null? (lambda-variables proc))
              (null? (lambda-rest-var proc)))
         (replace call (detach (lambda-body proc)))
         t)
        (else
         (set (node-simplified? call) t)
         (remove-unused-arguments proc call)
         (walkcdr simplify (call-args call))
         (quick-substitute-arguments proc call)
         (if (remove-unused-let call proc)
             t
             (really-simplify-let proc call)))))

;;; Simplify the body of PROC and then try substituting the arguments again.
;;; If all the arguments can be substituted the call node is removed.
;;; CHANGE? indicates that the let node has been moved in the tree and thus
;;; its parent must resimplify.
;;;
;;; SUBSTITUTE-JOIN-ARGUMENTS copies arguments in an attempt to remove
;;; conditionals via constant folding.

(define (really-simplify-let proc call)
  (iterate loop ((change? nil))
    (set (node-simplified? proc) t)
    (simplify-call proc)
    (substitute-arguments proc call)
    (cond ((substitute-join-arguments proc call)
           (loop t))
          ((not (node-simplified? proc))
           (loop change?))
          (else
           (remove-unused-arguments proc call)
           (cond ((or (remove-unused-let call proc)
                      change?)
                  (set (node-simplified? call) nil)
                  t)
                 (else
                  (set (node-simplified? call) t)
                  nil))))))

;;; Replace CALL by the body of its procedure PROC if CALL has no arguments.

(define (remove-unused-let call proc)
  (cond ((null? (call-args call))
         (if (lambda-rest-var proc)
             (walk-refs-safely (lambda (ref)
                                 (replace ref (create-literal-node '())))
                               (lambda-rest-var proc)))
         (replace call (detach (lambda-body proc)))
         t)
        (else
         nil)))

;;; Removed unused arguments to a lambda-node in call position.  Destructively
;;; changes the lists of arguments and variables.  If the lambda-node is
;;; n-ary an explicit call to LIST is added to cons up the list of arguments.

(define (remove-unused-arguments node call)
  (if (and (lambda-rest-var node)
           (not (used? (lambda-rest-var node))))
      (set (cadr (lambda-all-variables node)) nil))
  (iterate loop ((vars (lambda-rest+variables node))
                 (args (call-proc+args call))
                 (exits (call-exits call))
                 (n 2))
    (cond ((null? (cdr vars))
           (remove-let-rest-var node call args))
          ((used? (cadr vars))
           (set (variable-number (cadr vars)) n)
           (set (node-role (cadr args)) (call-arg (fx- n 1)))
           (loop (cdr vars) (cdr args) (fx- exits 1) (fx+ n 1)))
          (else
           (set (cdr vars) (cddr vars)) ; Evil
           (erase-all (cadr args))
           (set (cdr args) (cddr args)) ; Extremely Evil
           (if (fx< 0 exits)
               (set (call-exits call)
                    (fx- (call-exits call) 1)))
           (loop vars args (fx- exits 1) n)))))

;;; Just like REMOVE-UNUSED-ARGUMENTS except that it substitutes arguments
;;; for variables instead of removing them.

(define (quick-substitute-arguments node call)
  (iterate loop ((vars (lambda-rest+variables node))
                 (args (call-proc+args call))
                 (exits (call-exits call))
                 (n 2))
    (cond ((null? (cdr vars))
           nil)
          ((quick-substitute (cadr vars) (cadr args) n)
           (set (cdr vars) (cddr vars)) ; Evil
           (set (cdr args) (cddr args)) ; Extremely Evil
           (if (fx< 0 exits)
               (set (call-exits call)
                    (fx- (call-exits call) 1)))
           (loop vars args (fx- exits 1) n))
          (else
           (loop (cdr vars) (cdr args) (fx- exits 1) (fx+ n 1))))))

(define (quick-substitute var val n)
  (set (variable-number var) n)
  (set (node-role val) (call-arg (fx- n 1)))
  (cond ((substitute? var val)
         (substitute var val t)
         t)
        (else nil)))

;;; Substitute VAL for VAR if VAL is a literal-node, a primop-node, or
;;; a reference to a lexically bound or DEFINEd variable.

(define (substitute? var val)
  (ignore var)
  (or (and (reference-node? val)
           (let ((var (reference-variable val)))
             (or (variable-binder var)
                 (let ((def (get-variable-definition var)))
                   (and def 
                        (or (eq? 'constant (definition-variant def))
                            (eq? 'define (definition-variant def))))))))
      (literal-node? val)
      (primop-node? val)))

;;; Try to substitute any arguments to LAMBDA-PROC that are lambda nodes.
;;; Three different methods are tried.  SUBSTITUTE-LAMBDA? checks that there
;;; is only one reference to the variable and the reference is in call or
;;; exit position or only one call down in the tree.  
;;; PARTIAL-SUBSTITUTE-VARIABLE? and PARTIAL-SUBSTITUTE-LAMBDA? determine
;;; whether it is worth duplicating the argument to do the substitution.

(define (substitute-arguments lambda-proc call-node)
  (walk (lambda (var val)
          (cond ((not (used? var)) nil)  ; VAL may be *EMPTY* if VAR is unused
                ((reference-node? val)
                 (partial-substitute-variable var (reference-variable val)))
                ((not (lambda-node? val))
                 nil)
                ((substitute-lambda? var)
                 (substitute var val t))
                ((partial-substitute-lambda? val)
                 (partial-substitute var val))
                (else
                 (substitute-known-args var val))))
        (lambda-variables lambda-proc)
        (call-args call-node)))

(define (substitute-known-args var val)
  (cond ((and (variable? (lambda-rest-var val))
              (null? (variable-refs (lambda-rest-var val))))
         (let ((len (length (lambda-variables val))))
           (walk-refs-safely (lambda (n)
                               (if (eq? call-proc (node-role n))
                                   (shorten-call-args (node-parent n) len)))
                             var)))))

(define (shorten-call-args call count)
  (if (fx> (length (call-args call)) count)
      (let ((rest (nthcdr (call-proc+args call) count)))
        (walk erase-all (cdr rest))
        (set (cdr rest) '()))))

;;; Two versions of this.  We cannot use the strong one until the compiler
;;; can hoist lambdas back up again.

(define (strong-substitute-lambda? var)
  (null? (cdr (variable-refs var))))

(define (weak-substitute-lambda? var)
  (and (null? (cdr (variable-refs var)))
       (let ((ref (car (variable-refs var))))
         (or (eq? (node-role ref) call-proc)
             (eq? (node-role ref) object-proc)
             (call-exit? ref)
             (eq? (variable-binder var) (node-parent (node-parent ref)))))))

(define substitute-lambda? weak-substitute-lambda?)

;;; Substitute a lambda-node where it is called.

(define (partial-substitute-variable var val)
  (let ((call (lambda-body (variable-binder var))))
    (walk-refs-safely (lambda (ref)
                        (if (eq? call (node-parent ref))
                            (replace ref (create-reference-node val))))
                      var)))

;;; VAL is simple enough to be substituted in more than one location if
;;; it is not n-ary, its call is all leaf-nodes, and its procedure is not
;;; a reference to an integrable value.

(define (partial-substitute-lambda? val)
  (and (or (not (lambda-rest-var val)) 
           (null? (variable-refs (lambda-rest-var val))))
       (every? (lambda (n)
                 (leaf-node? n))
               (call-proc+args (lambda-body val)))
       (primop-node? (call-proc (lambda-body val)))))

;;; Substitute VAL (a lambda-node) for VAR everywhere that it can be
;;; integrated.  References in non-call position need to be consed anyway
;;; so there is no reason not to substitute them as well (?).

(define (partial-substitute var val)  
  (cond ((any? (lambda (ref)
                 (or (eq? (node-role ref) call-proc)
                     (call-exit? ref)))
               (variable-refs var))
         (substitute var val t))))

;;; Removing rest vars from lambdas in call position
;;;
;;; ((LAMBDA (<vars> . X) <body>)
;;;  <vals> RV1 RV2 ... RVN)
;;;
;;; => ((LAMBDA (<vars>) <body>[X/'()]) <vals>) if N = 0
;;;
;;; => ((LAMBDA (<vars>)
;;;       (LIST (LAMBDA (X) <body>) RV1 RV2 ... RVN))
;;;     <vals>)       

(define (remove-let-rest-var proc call args)
  (ignore call)
  (let ((var (lambda-rest-var proc)))
    (set (cadr (lambda-all-variables proc)) nil)
    (cond ((and (not var)
                (null? (cdr args)))
           (return))
          ((not (used? var))
           (walk erase-all (cdr args))
           (set (cdr args) '()))    ; Extremely Evil
          ((null? (cdr args))
           (let ((val (create-literal-node '()))) 
             (substitute var val nil)
             (erase val)))
          (else
           (let ((vals (map detach (cdr args)))
                 (l-proc (get-system-variable '%list))
                 (body (detach (lambda-body proc))))
             (set (cdr args) '())
             (let-nodes ((c1 ((* l-proc) 1 (^ l1) . vals))
                          (l1 (#f (v var)) body))
               (relate lambda-body proc c1)))))))

;;;                 Simplifying Joins
;;;============================================================================

;;; This looks for arguments in a let-node that test one of their arguments
;;; and get called with a known value:
;;;
;;;   (LET ((J (LAMBDA (... X ...)
;;;              ... (IF X ...) ...)))
;;;     ... (J ... <known value> ...) ...)
;;;
;;; If one is found, PARAMETERIZE is called to reduce the procedure to one
;;; that is cheap to copy:
;;; 
;;;   (LET ((...))                    ; parts of the original procedure
;;;     (LET ((J (LAMBDA (... X ...)
;;;                (IF X ...))))      ; all that's left
;;;       ... (J ... <known value> ...) ...)
;;;
;;; If PARAMETERIZE works, the new procedure is substituted for the variable.

;;; Call JOIN-SUBSTITUTE on all variable/value pairs.

(define (substitute-join-arguments lambda-proc call)
  (iterate loop ((vars (lambda-variables lambda-proc))
                 (vals (call-args call))
                 (change? nil))
    (cond ((null? vars) change?)
          ((and (used? (car vars)) ; (CAR VALS) may be *EMPTY* if VAR is unused
                (lambda-node? (car vals))  
                (join-substitute (car vars) (car vals)))
           (loop (cdr vars) (cdr vals) t))
          (else
           (loop (cdr vars) (cdr vals) change?)))))

;;; Get any calls that test arguments in VAL, and then find any calls to VAL
;;; that pass literal nodes to any of those arguments.  If there are any,
;;; try to parameterize VAL and substitute it for VAR.

;;; This code only tries one simple cond call per variable and only one
;;; variable/literal pair per call to VAR. Bug.

(define (join-substitute var val)
  (let ((calls (map get-simple-cond-call (lambda-variables val))))
    (iterate loop ((refs (variable-refs var)))
      (cond ((null? refs) nil)
            ((and (eq? call-proc (node-role (car refs)))
                  (call-and-literal-match calls
                                          (call-args (node-parent (car refs)))))
             => (lambda (call)
                  (cond ((parameterize val call)
                         (walk-refs-safely
                          (lambda (ref) 
                            (if (eq? call-proc (node-role ref))
                                (replace ref (copy-node-tree val))))
                          var)
                         t)
                        (else
                         (loop (cdr refs))))))
             (else (loop (cdr refs)))))))

;;; Find a matching non-false call and literal node argument.

(define (call-and-literal-match calls args)
  (do ((calls calls (cdr calls))
       (args args (cdr args)))
      ((or (null? args)
           (and (car calls)
                (literal-node? (car args))))
       (if (null? calls) nil (car calls)))))

;;; Find any call of the form (IF var ...)

(define (get-simple-cond-call var)
  (and (variable? var)
       (any simple-cond-ref (variable-refs var))))

(define (simple-cond-ref ref)
  (let ((call (node-parent ref)))
    (cond ((or (not (call-node? call))
               (not (fx= 2 (call-exits call))))
           nil)
          (else
           (destructure (((cond? #f #f test? true? t-ref) 
                          (call-proc+args call)))
             (if (and (primop-ref? cond? primop/conditional)
                      (primop-ref? test? primop/test)
                      (primop-ref? true? primop/true?)
                      (eq? ref t-ref))
                 call
                 nil))))))



