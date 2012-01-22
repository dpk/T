(herald (front_end inf_files)
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

;;; An INF file contains declaration information that has been inferred from
;;; a source file.

;;;                     SHAPE => FILE
;;;========================================================================

;;; Write out the declaration information from SHAPE into a file.  The
;;; following things are written out:
;;;   a) Comexes for primops defined in the file.
;;;   b) Parents and arguments for parameterized primops defined in the file.
;;;   c) A list of environment records.
;;;   d) The source code for the comexes in part a) in case the file is
;;;      used on a different machine than the one it was produced on.

(define (write-support-file shape filename)
  (with-open-ports ((output (open (information-filename filename) 'dump)))
    (set-encoder output shape-encoder)
    (receive (made new)
             (partition-list primop.constructed? (shape-primops shape))
      (write output (if (null? new)
                        '() 
                        (cons (if *compile-primops?*
                                  (compile-support-primops new)
                                  nil)
                              (map primop.id new))))
      (write output (map (lambda (p)
                           `(,(primop.id p) . ,(primop.arglist p)))
                         made))
      (write output (new-environment-table->list (shape-new-env shape)))
      (write output (map primop->executable new))
      t)))

(define (compile-support-primops primops)
  (orbit-primop-compile (primops->source-code primops) orbit-env))

;;; Put all non-local definitions from TABLE into a list.

(define (new-environment-table->list table)
  (let ((exp '()))
    (table-walk table
                (lambda (name variable)
                  (let ((def (variable-definition variable)))
                    (if (not def)
                        (bug '"new environment variable ~S has no def" variable))
                    (if (not (memq? 'local (definition-data def)))
                        (push exp (cons name def))))))
    exp))

;;;                FILE => TABLE
;;;========================================================================

;;; Reading definition tables back from files.  The files are cached in
;;; DEFINITION-TABLES to aviod unnecessary work.

(define definition-tables (make-tree-table 'definition-tables))

;;; Read the information file in if it hasn't already been done.

(define (get-definition-table name)
  (let* ((file (module-name->filename name))
         (time (file-write-date (information-filename file)))
         (old (table-entry definition-tables name)))
    (cond ((and old (= (car old) time))
           (cdr old))
          (else
           (let ((defs (read-definitions-from-file name file)))
             (set (table-entry definition-tables name) (cons time defs))
             defs)))))

;;; Retrieve the definitions.  This requires two readings of the file if
;;; the primop comexes in it are not suitable for the current system.

(define (read-definitions-from-file name filename)
  (noise "Getting early bindings from ~S~%" (filename-name filename))
  (cond ((maybe-read-definitions name filename nil)
         => identity)
        (else
         (orbit-warning "primops in ~A are not compiled for this system.~%"
                        (filename->string filename))
         (let ((primops (load-and-evaluate-primops filename)))
           (maybe-read-definitions name filename primops)))))

(define (maybe-read-definitions name filename primops)
  (with-open-ports ((input (open (information-filename filename) 'retrieve)))
    (set-decoder input shape-decoder)
    (let ((comex+ids (read input)))
      (cond ((null? comex+ids)
             (resurrect-constructed-primops (read input))
             (resurrect-definition-table (read input) name '()))
            ((or primops
                 (install-primop-comex comex+ids))
             => (lambda (primops)
                  (resurrect-constructed-primops (read input))
                  (resurrect-definition-table (read input) name primops)))
            (else nil)))))

(define (resurrect-constructed-primops data)
  (walk (lambda (stuff)
          (let ((primop (remake-primop (car stuff) (cdr stuff))))
            (set (table-entry primop-table (primop.variant-id primop)) primop)))
        data))

;;; Actually put the definitions and primops into a table.

(define (resurrect-definition-table defs name primops)
  (let ((table (make-definition-table name)))
    (walk (lambda (p)
            (add-primop table p))
          primops)
    (walk (lambda (p)
            (set (table (car p)) (cdr p)))
          defs)
    table))

(define (install-primop-comex comex+ids)
  (let ((comex (car comex+ids))
        (ids (cdr comex+ids)))
    (cond ((not (and comex (installable-comex? comex)))
           nil)
          (else
           (instantiate-comex comex orbit-env)
           (map (lambda (id)
                   (table-entry primop-table id))
                ids)))))

;;; Comexes are not yet labeled with their target system.

(define (installable-comex? c)
  c)

;;; Load the primop source code and compile and install them.

(define (load-and-evaluate-primops filename)
  (with-open-ports ((input (open (information-filename filename) 'retrieve)))
    (set-decoder input shape-no-primops-decoder)
    (read input)   ; Ignore comex
    (read input)   ; Ignore constructed primops
    (read input)   ; Ignore definitions
    (map (lambda (code)
           (let ((primop (eval code orbit-env)))
             (set (table-entry primop-table (primop.id primop)) primop)
             primop))
         (read input))))

;;; Compiling the primop source code in an INF file.
;;; This needs to be given a complete file-spec including the extension.

(define (compile-primop-source file-spec)
  (let ((filename (->filename file-spec)))
    (receive (names constructed definitions sources)
             (load-for-primop-compilation filename)
      (cond ((null? sources)
             (orbit-warning "No primops to compile in ~A"
                            (filename->string filename)))
            (else
             (let* ((source (primop-executables->source-code names sources))
                    (comex (orbit-primop-compile source orbit-env)))
               (dump-with-primop-comex filename
                                       (cons comex names)
                                       constructed
                                       definitions
                                       source))))
      (return))))

(define (load-for-primop-compilation filename)
  (with-open-ports ((input (open filename 'retrieve)))
    (set-decoder input shape-re-decoder)
    (let* ((names       (cdr (read input)))
           (constructed (read input))
           (definitions (read input))
           (source      (read input)))
      (return names constructed definitions source))))

(define (dump-with-primop-comex filename comex constructed definitions source)
  (with-open-ports ((output (open filename 'dump)))
    (set-encoder output shape-re-encoder)
    (write output comex)
    (write output constructed)
    (write output definitions)
    (write output source)
    (return)))

;;;           Dumping and Retrieving Definition Records
;;;============================================================================

;;; Definitions, primops, and comexes need to be dismantled and rebuilt.

;;; Dumping

(define (shape-encoder x)
  (cond ((primop?     x)
         (if (and (primop.constructed? x)
                  (not (primop.variant-id x)))
             (return 'constructed-primop constructed-primop-accessors '())
             (return 'primop primop-accessors '())))
        ((definition? x) (return 'definition '() definition-accessors))
        ((comex?      x) (return 'comex      '() comex-accessors))
        (else
         (return nil nil nil))))

(define definition-accessors
  (list definition-data
        definition-variant
        definition-type
        definition-value))

(define comex-accessors (stype-selectors comex-stype))

(define primop-accessors
  (list any-primop-id))

(define constructed-primop-accessors
  (list primop.id primop.arglist))

;;; Retreiving

(define (shape-decoder x)
  (case x
    ((definition)         (return make-definition definition-accessors))
    ((primop)             (return lookup-primop   '()))
    ((constructed-primop) (return remake-primop   '()))
    ((comex)              (return make-comex      comex-accessors))
    (else                 (return nil             nil))))

(define (shape-no-primops-decoder x)
  (case x
    ((definition)         (return make-definition definition-accessors))
    ((primop)             (return false           '()))
    ((constructed-primop) (return false           '()))
    ((comex)              (return make-comex      comex-accessors))
    (else                 (return nil             nil))))

;;; Primops are stored in a single global table.  Ugh.

(define (lookup-primop name)
  (let ((primop (table-entry primop-table name)))
    (cond ((not primop)
           (bug '"cannot find primop ~S" name))
          (else
           primop))))

(define (remake-primop name args)
  (let ((primop (table-entry primop-table name)))
    (cond ((not primop)
           (bug '"cannot find primop ~S" name))
          ((null? args)
           primop)
          (else
           (construct-primop primop args)))))

;;; Reading and writing inf-files to compile their primops.
;;; Primops are retrieved as PSEUDO-PRIMOP structures so that they are not
;;; introduced into the global primop table.

(define (shape-re-decoder x)
  (case x
    ((definition)         (return make-definition      definition-accessors))
    ((primop)             (return pseudo-lookup-primop '()))
    ((constructed-primop) (return pseudo-remake-primop '()))
    ((comex)              (return make-comex           comex-accessors))
    (else                 (return nil                  nil))))

(define (pseudo-lookup-primop name)
  (let ((primop (make-pseudo-primop)))
    (set (pseudo-primop-constructed? primop) nil)
    (set (pseudo-primop-id           primop) name)
    (set (pseudo-primop-arglist      primop) nil)
    primop))

(define (pseudo-remake-primop name args)
  (let ((primop (make-pseudo-primop)))
    (set (pseudo-primop-constructed? primop) t)
    (set (pseudo-primop-id           primop) name)
    (set (pseudo-primop-arglist      primop) args)
    primop))

(define (shape-re-encoder x)
  (cond ((pseudo-primop? x)
         (if (pseudo-primop-constructed? x)
             (return 'constructed-primop pseudo-constructed-primop-accessors '())
             (return 'primop pseudo-primop-accessors '())))
        ((definition? x) (return 'definition '() definition-accessors))
        ((comex?      x) (return 'comex      '() comex-accessors))
        (else
         (return nil nil nil))))

(define-structure-type pseudo-primop
  constructed?
  id
  arglist
  )

(define pseudo-primop-accessors
  (list pseudo-primop-id))

(define pseudo-constructed-primop-accessors
  (list pseudo-primop-id pseudo-primop-arglist))



