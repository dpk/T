(herald (assembler fg_spec t 77))

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

;;; (define-fg (fgname . parameters)
;;;   <RandomSpec>s . <fg-spec>s )
;;;
;;;   <parameter> is (<predicate> <parameter-name>)
;;;   and the <predicate> return false if the parameter passed
;;;   to create the fg was not of the right type.
;;; 
;;; <fg-spec> is one of
;;;  
;;;   (F {S|U} <FixedWidthSpec> <ValueSpec>)      -- fixed width field
;;;   (V {S|U} <VariableWidthSpec> <ValueSpec>)   -- variable width field
;;;   ({0|1} {0|1} ...) -- like (F U width value), msb first
;;;
;;;   (FG <ValueSpec> <ContextSpec>)
;;;      -- value is an FG, should be a symbol
;;;   
;;;   (DEPENDING-ON ... )
;;;      -- see selector comments below.           
;;;
;;;   (MARK <symbol>)
;;;      -- The value of <symbol> is set to the current location counter.
;;; 
;;; <FixedWidthSpec> ::= <fixnum> | <symbol> | ( <expr> )
;;; <VariableWidthSpec> ::= ( <fixnum>s )
;;; <ContextSpec> ::= ( <ContextName> . <number-or-symbol>s ) | 
;;;                   <symbol>
;;;    -- fixed case is as immediate context value, second
;;;       is when context is a parameter
;;; 
;;; 
;;; <ValueSpec> ::= <fixnum> | <symbol> | ( < expr > )
;;;    -- as a feature, an expr may include a (FROM mark label),
;;;       which will yield the displacement, when the expr is evaluated.
;;;
;;; <RandomSpec> ::= (PRINTER ...)  |
;;;                  (LOCAL <var>s) | 
;;;                  (CONTEXT <ContextSpec> )


;;; Runtime support for FG's is in FG.T

;;; An FG represents a parameterized sequence of bits.
;;; The rutime representation of an FG is similar to a structure:
;;; there is a vector of values (the fg parameters, context variables,
;;; local variables, and temps).  An FG expression (named-fg (foo a b) ...)
;;; yields a procedure, that when called, will yield an FG.  All
;;; FGs returned as a result of calling the fg procedure will be of the
;;; same FG-TYPE.  The FG-TYPE contains a vector of contants
;;; (procedures, and the FG-OPS, etc), a printer, a list of indices into
;;; the ops to where subfields are, and a context (type) name.

;;; What the FG-OPS are:  the various field specifiers are compiled
;;; into a little "program", which the various parts of the assembler
;;; are driven from.

;;;; CONTEXT DEFINITION

;;; A context is a set of value associated witha particular subfield 
;;; position in an FG.  When specifying a machine description,
;;; an FG can be specified in a particular context.  The (context ...)
;;; subform in a DEFINE-FG gives the name of the context (for
;;; error checking later), and the names of the elements of the context.  
;;; Someone instantiating that FG must supply values for the context,
;;; and those values are supplied in the form of a list that is
;;; isomorphic to the orginal context specification.

(define context-id car)
(define context-components cdr)
                             
;;; UTILITIES for hacking the FG syntax.

(define (sign-op su)
  (if (eq? su 's) -1 0))

(define (augment-vals vals val)
  (return (cons val vals) (length vals)))

(define (allocate-vars-slot vars)
  (return (cons nil vars) (length vars)))

;;; The var mark is needed because we are multiplexing the vars
;;; list to supply variable names/position information as well
;;; as initial values.

(define *var-mark* (cons '*var-mark* nil))
(define *init-var-mark* (cons '*init-var-mark* nil))

(define (augment-vars vars val)
  (return (cons `(,*var-mark* . ,val) vars) 
          (length vars)))
                
(define (set-initial-value var val vars)
    (iterate loop ((vars vars))
        (cond ((null? vars)
               (error "can't set initial value of ~s in ~s" var vars))
              ((eq? (car vars) var)
               (set (car vars) `(,*init-var-mark* ,var . ,val)))
              ((and (pair? (car vars))
                    (eq? (caar vars) *init-var-mark*)
                    (eq? (cadar vars) var))
               (error "~s already has an initial value ~s"
                      var
                      vars))              
              (else
               (loop (cdr vars))))))

(define (is-the-var? the-var some-var)
    (cond ((eq? the-var some-var) t)
          ((and (pair? some-var)
                (eq? (car some-var) *init-var-mark*)
                (eq? (cadr some-var) the-var))
           some-var)
          (else nil)))

(define (vars-ref vars key)
  (fx- (fx- (length vars)
            (or (pos is-the-var? key vars)
                (error "variable ~s not found in ~s" key vars)))
       1))

;;;; FG DEFINITION PROCESSING
                                
(define (process-define-fg name parameters specs)
    (process-define-fg-1 name parameters specs nil))

(define (process-define-data-fg name parameters specs)
    (process-define-fg-1 name parameters specs t))

(define (process-define-fg-1 name parameters specs data?)
  (receive (locals context printer fg-specs)
           (process-random-specs specs)
    ;; construct vars backwards
    (let* ((bvl   (map (lambda (x) (if (pair? x) (cadr x) x)) parameters))
           (vars (append (reverse locals)
                         (reverse bvl)
                         (reverse (context-components context))))
           (vals '() ))
      (iterate loop ((fg-specs fg-specs)
                     (ops's '())
                     (vars vars)
                     (vals vals)
                     (sf's '()))
        (cond ((null? fg-specs)
               ;; put together the code
               (let ((fgt-name (generate-symbol 'fg-type)))
                  `(let ((,fgt-name 
                          ,(fgt-code printer vars ops's vals context sf's data?)))
                    ,(fg-code name bvl parameters fgt-name context locals vars)))
               )
              (else
               (receive (ops vars vals)
                        (process-fg-spec (car fg-specs) vars vals)
                 (let ((ops's-length (length ops's)))
                   (loop (cdr fg-specs)
                         (append! ops's ops)
                         vars
                         vals
                         ;; collect sf positon information.
                         (cond ((or (eq? (caar fg-specs) 'fg)
                                    (eq? (caar fg-specs) 'fg-named))
                                (cons ops's-length sf's))
                               (else sf's)))))))))))


;;; Collect LOCAL PRINTER and CONTEXT specs out of a define-fg form

(define (process-random-specs specs)
   (iterate loop ((specs specs) (locals '()) (context nil) (printer nil))
     (let ((spec (car specs)))
       (cond ((eq? (car spec) 'local)
              (loop (cdr specs) (cdr spec) context printer))
             ((eq? (car spec) 'printer)
              (loop (cdr specs) locals context (cdr spec)))
             ((eq? (car spec) 'context)
              (loop (cdr specs) locals (cadr spec) printer))
             (else
              (return locals context printer specs))))))


;;; Returns ops for this spec, new vars, and new vals

(define (process-fg-spec spec vars vals)
  (case (car spec)
    ((f) 
     (process-f-spec spec vars vals))
    ((0 1)
     (receive (width value) 
              (bits->fixnum spec)
        (process-f-spec `(f u ,width ,value) vars vals)))
    ((v)
     (process-v-spec spec vars vals))
    ((depending-on)
     (process-d-o-spec spec vars vals))
    ((fg)
     (process-subfg-spec (cdr spec) vars vals nil))
    ((fg-named)
     (process-subfg-spec (cddr spec) vars vals (cadr spec)))
    ((mark)
     (destructure (((#f mark-name) spec))
        (return `(,wop/mark ,(vars-ref vars mark-name))
                vars
                vals)))
    (else
     (error "unrecognized fg spec: ~s" spec))))

;;; Convert a list of bits to a fixnum.

(import t-implementation-env *bits-per-fixnum*)
(define (bits->fixnum bits-in)
  (iterate loop ((l 0) (num 0) (bits bits-in))
     (cond ((null? bits)
            (return l num))
           ((fx>= l *bits-per-fixnum*)
             (error "too many bits~%  (bits->fixnum ~s)" bits-in))
           (else
            (loop (fx+ l 1) (fx+ (fixnum-ashl num 1) (car bits)) (cdr bits))))))
 

(define (process-f-spec spec vars vals)
  (destructure (((#f su w-exp v-exp) spec))
    (receive (vop voc1 vals)
             (fg-value-op v-exp vars vals)
      (receive (wop wopcs vars vals)
               (process-f-width-exp w-exp vars vals)
        (return `(,wop ,(sign-op su) ,@wopcs ,vop ,voc1)
                vars
                vals)))))

(define (process-v-spec spec vars vals)
  (destructure (((#f su options v-exp) spec))
    (receive (vop voc1 vals) (fg-value-op v-exp vars vals)
      (receive (vars var-pos) (allocate-vars-slot vars)
        (receive (vals val-pos) (augment-vals vals options)
          (return `(,wop/var ,(sign-op su)
                             ,var-pos  ;cw-i
                             ,val-pos  ;opt-i
                             ,vop ,voc1)
                  vars
                  vals))))))

;;; DEPENDING-ON selectors

;;; The selector in a D-O is used to calculate the number of bits
;;; needed to represent the field, given the displacement specified
;;; in the D-O, and the width of this field used in computing that 
;;; displacement.   
;;;
;;; The selector is specified as 
;;;
;;;     ( <selector-name> ( <width-name> <min-width> ) <displ-name> )
;;;
;;; <width-name> and <displ-name> are names of variables local to this fg.  
;;; The selector is passed these (TAS figures out initial values) and
;;; must return new values.  The two variables will be set to the final
;;; width and displacement values.  The last form in the D-O is an expression
;;; that will be evaluated to get an fg (or list of them) to use as the
;;; D-O.  That fg is obligated to be exactly as wide as the selector 
;;; computed it would be (that width will be the value of the variable
;;; named <width-name>.
;;;
;;; The returned displacement must be measured from the same spot that
;;; the passed-in displacement was measured from.
;;;
;;; This routine 'wraps' the selector so that its return values will
;;; be available to the fg expression.

;;; Who sets what fields ina D-O spec:
;;; "count" sets the sdf-number slot in VARS to the index in the
;;; SDFS vector of the sdf for the fg.  The sdf is also stored in
;;; the sdf-i slot of VARS.  The mark-i slot of VARS is an index
;;; in the MARKS vector of the mark for the D-O; "count" also
;;; initializes this slot.

(define (process-d-o-spec spec vars vals)
  (destructure (((#f (#f m-name label) 
                     (sel (w-name min) d-name) 
                     fg-expr) 
                 spec))
    (let ((width-i (vars-ref vars w-name))
          (displ-i (vars-ref vars d-name))
          (mark-i  (vars-ref vars m-name)) )
      (receive (vars sdf-i)
               (augment-vars vars 
                `(cons-sdf ,label ,sel ,min '(,width-i . ,displ-i)))
        (receive (vals fg-expr-i)
                 (augment-vals vals (compile-expr fg-expr vars))
          (receive (vars sdf#-i) (allocate-vars-slot vars)
            (return `(,wop/d-o ,sdf#-i ,sdf-i ,mark-i ,fg-expr-i)
                    vars 
                    vals)))))))
             
;;; This used to be called 'subfield-in-context'
;;;
;;; format: (FG <var> <context-exp>) or (FG <expr> <context-exp>)  
;;; <expr> is evaluated at compress time so context is available, and 
;;; displacements are not. 
;;; For (FG <expr> ...), the VAL index of the <expr>-procedure is stored in 
;;; the VAR slot allocated for the subfg
;;;
;;; (FG-NAMED <name> <expr> <context-exp>) - sets local variable <name> 
;;; to the value of <expr>.

(define (process-subfg-spec spec vars vals name)
  (destructure (((fg context) spec))
    (if (and name (symbol? fg))
        (error "2 names for subfield: ~s and ~s" fg name))
    (receive (vop voc1 vals)
             ;; get context guy.
             (cond ((false? context)
                    (receive (vals val-pos)
                             (augment-vals vals "No context given in fg")
                      (return vop/const val-pos vals)))
                   (else
                    (fg-value-op context vars vals)))
       ;; get subfg; process if expr
       (receive (var-index vars vals)
                (cond ((symbol? fg) 
                       (return (vars-ref vars fg) vars vals))
                      (else ; have an <expr> for subfg
                       (receive (vals fg-expr-index)
                                (augment-vals vals (compile-expr fg vars))
                          (cond (name   
                                 (set-initial-value name fg-expr-index vars)
                                 (return (vars-ref vars name) vars vals))
                                (else
                                 (receive (vars var-pos)
                                          (augment-vars vars 
                                                        fg-expr-index)
                                    (return var-pos vars vals)))))))
          (return `(,wop/subfield-ic ,var-index ,vop ,voc1)
                  vars
                  vals)))))

;;; FG processing utilities.

;;; returns <v-op> <v-opcode1> and new <vals>
(define (fg-value-op v-exp vars vals)
  (xcond ((fixnum? v-exp)
          (receive (n-vals val-pos) (augment-vals vals v-exp)
            (return vop/const val-pos n-vals)))
         ((symbol? v-exp)
          (return vop/var (vars-ref vars v-exp) vals))
         ((pair? v-exp)
          (cond ((or (eq? (car v-exp) 'from) (eq? (car v-exp) 'disp))
                 (error "disp/from return not supported")
                 (return vop/disp
                         (vars-ref vars (cadr v-exp))  ; marker
                         (vars-ref vars (caddr v-exp)) ; destination
                         vals))
                (else
                 (receive (n-vals val-pos)
                          (augment-vals vals (compile-expr v-exp vars))
                   (return vop/proc val-pos n-vals)))))))

(define (process-f-width-exp w-exp vars vals)
  (cond ((fixnum? w-exp)
         (return wop/fix `(,w-exp) vars vals))
        ((symbol? w-exp)
         (return wop/@fix `(,(vars-ref vars w-exp)) vars vals))
        (else
         (receive (vals val-pos)
                  (augment-vals vals (compile-expr w-exp vars))
           (receive (vars var-pos)
                    (allocate-vars-slot vars)
             (return wop/proc `(,var-pos ,val-pos) vars vals))))))


;;; Put fg code together; called with information collected by PROCESS- guys.

;;; Construct code for fg-type.

(define (fgt-code pr vars ops vals context sf's data?)
  `(cons-fg-type ,(compile-print-expr `(format stream ,@pr) '(stream) vars)
                 ',ops
                 (vector ,@(map (lambda (x)
                                  (cond ((and (pair? x)
                                              (neq? (car x) 'lambda))
                                         `',x)
                                        (else x)))
                                (reverse! vals)))
                 ',(context-id context)
                 ',(fixup-sf's sf's)
                 ,(length (context-components context))
                 ',data?))
                              
;;; Construct code for fg object itself.

(define (fg-code name bvl parameters type-var-name context locals vars)
  `(object 
     (named-lambda ,name ,bvl
        ,(cond ((any? pair? parameters)
                `(let (,@(map list bvl parameters))
                   (and ,@bvl 
                        ,(fg-code-1 bvl type-var-name context locals vars))))
               (else
                (fg-code-1 bvl type-var-name context locals vars))))
     ((get-fg-type self) ,type-var-name)))

(define (fg-code-1 bvl type-var-name context locals vars)
   `(cons-fg ,type-var-name
             (vector ,@(append (map (lambda (()) ''())
                                    (context-components context))
                               bvl
                               (map (lambda (v)
                                       (hack-initial-value v vars))
                                    locals)
                               (make-var-slot-code vars)
                               ))))

(define (hack-initial-value var vars)
    (cond ((any (lambda (some-var) (is-the-var? var some-var)) vars)
           => (lambda (v) (cond ((pair? v) (cddr v))
                                (else ''()))))
          (else
           ''())))

;;; Horrible horrible.  We keep track of where in an FG the subfields are
;;; so that CONTEXTIFY need not scan the fg.  sf's is a backward list of 
;;; positions in the fg-ops lists, this take succesive differences so 
;;; that contextify has an even easier time.
;;; (fixup-sf's '(23 19 3))  => (3 16 4)

(define (fixup-sf's sf's)
  (do ((prev sf's (cdr prev))
       (cur (cdr sf's) (cdr cur)))
      ((null? cur) (reverse! sf's))
    (set (car prev) (fx- (car prev) (car cur)))))
                                            
;;; As the PROCESS- guys build of the list of things in the FG-VARS vector,
;;; some things are marked as needing to be evaluated by wrapping them
;;; with (*var-mark* ...);  MAKE-VAR-SLOT-CODE takes the marks out, and
;;; puts in quotes.  Note this results ina reversed list, and the input
;;; is only processed up to the first symbol.   This is because VARS
;;; starts out with the context names, parameter names, and local variable
;;; names in it so the PROCESS- guys can compile references to those things.
;;; BUT! there are no return for the local and context vars when the fg 
;;; is made, but there are parameter return, so blah blah.

(define (make-var-slot-code vars)
  (iterate loop ((l vars) (var-slots '()))
     (cond ((null? l) 
            var-slots)
           ((null? (car l)) 
            (loop (cdr l) (cons ''() var-slots)))
           ((and (pair? (car l)) 
                 (eq? (caar l) *var-mark*))
            (loop (cdr l) (cons (cdar l) var-slots)))
           (else
            var-slots))))
