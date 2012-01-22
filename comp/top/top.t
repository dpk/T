(herald (orbit_top top))

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
    
(define bogus-filename (->filename 'anonymous))

(define (orbit exp . env)
  (let ((env (if env (car env) (repl-env))))
    (bind ((*noise-flag* nil)
           (*debug-flag* nil)
           (*noise+error*    (error-output))
           (*noise+terminal* null-port)
           (*noise-stream*   null-port))
      (receive (comex #f)
               (compile `(,syntax/lambda () ,exp)
                        standard-early-binding-env
                        (env-syntax-table env)
                        bogus-filename
                        '(anonymous))
        (instantiate-comex comex env)))))

(define (compile exp support syntax filename h)
  (front-init support
              (lambda ()
                (generate-init 
                 (lambda () 
                   (assemble-init 
                    (lambda ()
                      (really-compile exp syntax filename h))))))))

(define (really-compile exp syntax filename h)
  (receive (tree infex)
           (make-code-tree+support `(,syntax/lambda () ,exp) syntax)
    (receive (a b c)
             (analyze tree)
      (generate tree)
      (let ((comex (create-comex filename h a b c (assemble))))
        (if (not *debug-flag*) (erase-all tree))
        (return comex infex)))))

(define (cl exp . debug?)
  (let ((debug? (if (null? debug?) nil (car debug?))))
    (cond ((not (procedure? exp))
           (real-cl exp debug?))
          ((disclose exp)
           => (lambda (exp) (real-cl exp debug?)))
          (else
           (cl (error "cannot get source code for ~S" exp) debug?)))))

(define (real-cl exp debug?)
  (bind ((*noise-flag* t)
         (*debug-flag* debug?)
         (*assembly-comments?* t)
         (*noise+error*    (error-output))
         (*noise+terminal* (terminal-output))
         (*noise-stream*   (terminal-output)))
    (cl-compile `(,syntax/lambda () ,exp)
                base-early-binding-env
                (orbit-syntax-table)
                bogus-filename
                '(cl))))

(define (cl-compile exp support syntax filename h)
  (front-init support
              (lambda ()
                (generate-init 
                 (lambda () 
                   (assemble-init 
                    (lambda ()
                      (really-compile exp syntax filename h)
                      (quicklist))))))))

(define (make-node-tree exp)
  (bind ((*debug-flag* nil)
         (*noise-flag* nil)
         (*noise+error*    (error-output))
         (*noise+terminal* (terminal-output))
         (*noise-stream*   (terminal-output)))
    (front-init standard-early-binding-env
                (lambda ()
                  (receive (tree supex)
                           (make-code-tree+support `(,syntax/lambda () ,exp)
                                                   standard-syntax-table)
                    (ignore supex)
                    tree)))))

(lset *object-file-extension* 'o)
(lset *information-file-extension* 'i)
(lset *noise-file-extension* 'n)

(define (compile-file file-spec)
  (comfile-bind file-spec file-spec really-comfile))

(define comfile compile-file)

(define (comfile2 in-file-spec out-file-spec)
  (comfile-bind in-file-spec out-file-spec really-comfile))

(define (totally-comfile in-file-spec out-file-spec read-table syntax support)
  (comfile-bind in-file-spec
                out-file-spec
                (lambda (in-filename out-filename)
                  (really-totally-comfile in-filename out-filename
                                          read-table syntax support))))

(define (create-support in-file-spec . out-file-spec)
  (comfile-bind in-file-spec
                (if (null? out-file-spec) in-file-spec (car out-file-spec))
                (lambda (in-filename out-filename)
                  (receive (exp support syntax #f)
                           (read-file in-filename)
                    (front-init support
                      (lambda ()
                        (receive (tree infex)
                          (make-code-tree+support `(,syntax/lambda () ,exp)
                                                  syntax)
                          (erase-all tree)
                          (write-support-file infex out-filename))))))))

(define (comfile-bind in-file-spec out-file-spec cont)
  (let ((in-filename (->filename in-file-spec)) 
        (out-filename (->filename out-file-spec)))
    (with-open-ports ((noise-stream
                       (open (filename-with-type out-filename
                                                 *noise-file-extension*)
                             '(out))))
      (bind ((*debug-flag* nil)
             (*noise-flag* nil)
             (*noise+error* (make-broadcast-port noise-stream (error-output)))
             (*noise+terminal* (make-broadcast-port noise-stream 
                                                    (terminal-output)))
             (*noise-stream* noise-stream))
        (cont in-filename out-filename)))))

(define (really-comfile in-filename out-filename)
    (receive (exp support syntax h)
             (read-file in-filename)
      (receive (comex infex)
               (compile exp support syntax in-filename h)
        (write-support-file infex out-filename)
        (write-object-file comex out-filename)
        t)))

(define (really-totally-comfile in-filename out-filename read-table syntax support)
  (receive (exp #f #f h)
           (really-read-file in-filename read-table nil)
    (receive (comex infex)
             (compile exp support syntax in-filename h)
      (write-support-file infex out-filename)
      (write-object-file comex out-filename)
      t)))

(define (write-object-file comex filename)
  (write-comex-to-file (filename-with-type filename *object-file-extension*)
                       comex))
                           
;;; This isn't used anywhere
;;;(define-simple-switch orbit-macro-definition-env locale? user-env)
;;;(define tc-macro-definition-env orbit-macro-definition-env)

(define orbit-syntax-table
  (make-simple-switch 'orbit-syntax-table
                      true?
                      (env-syntax-table user-env)))

(define tc-syntax-table orbit-syntax-table)

;;; Bizarro new interface

(define (make-compiler id)
  (let ((syntax standard-syntax-table)
        (read standard-read-table)
        (bindings standard-early-binding-env))
    (object (lambda (from . to)
              (totally-comfile from
                               (if (null? to) from (car to))
                               read syntax bindings))
      ((compiler-syntax-table      self) syntax)
      ((compiler-read-table        self) read)
      ((compiler-early-binding-env self) bindings)
      (((setter compiler-syntax-table) self new)
       (set syntax new))
      (((setter compiler-read-table) self new)
       (set read new))
      (((setter compiler-early-binding-env) self new)
       (set bindings new))
      ((identification self) id))))

(define-settable-operation compiler-syntax-table)
(define-settable-operation compiler-read-table)
(define-settable-operation compiler-early-binding-env)

(define (make-early-binding-locale super name)
  (make-definition-env super name))

(define (make-empty-early-binding-locale name)
  (make-definition-env false name))

(define (load-early-bindings file-spec . early-binding-env)
  (let ((table (reload-support file-spec)))
    (instantiate-definition-table (if (null? early-binding-env)
                                      standard-early-binding-env
                                      (car early-binding-env))
                                  table)))

;;; Reading the file in

(define (read-file filename)
  (really-read-file filename nil t))

(define (really-read-file filename read-table herald?)
  (with-open-ports ((input (open-source filename (source-file-extension))))
    (let ((name (port-truename input)))
      (format *noise+terminal* "~%;Beginning compilation on ")
      (if (fx<= (fx+ (hpos *noise+terminal*) (print-width name))
                (line-length *noise+terminal*))
          (format *noise+terminal* "~A~2%" name)
          (format *noise+terminal* "~%; ~A~%" name)))
    (let* ((first (read input))
           (herald-obj (cond ((and (pair? first)
                                   (eq? (car first) 'herald))
                              (parse-herald (cadr first) (cddr first)))
                             (herald?
                              (error "file ~S has no herald form"
                                     (filename->string filename)))
                             (else nil))))
      (bind (((port-read-table input) 
                (cond (read-table => identity)
                      ((herald-read-table herald-obj)
                       (eval (herald-read-table herald-obj) user-env))
                      (else
                       standard-read-table))))
        (iterate loop ((forms '()) 
                       (read-form (if herald-obj (read input) first)))
          (cond ((not (eof? read-form))
                 (loop (cons read-form forms) (read input)))
                (herald?
                 (return `(,syntax/lambda () . ,(reverse! forms))
                         (if (herald-environment herald-obj)
                             (eval (herald-environment herald-obj) 
                                   user-env)
                             standard-early-binding-env)
                         (if (herald-syntax-table herald-obj)
                             (eval (herald-syntax-table herald-obj) 
                                   user-env)
                             (orbit-syntax-table))
                         (cdr first)))
                (else
                 (return `(,syntax/lambda () . ,(reverse! forms))
                         nil nil
                         (if herald-obj
                             (cdr first) 
                             (list (filename-name filename)))))))))))

(define (open-source filename extension)
  (or (maybe-open filename '(in))
      (maybe-open (filename-with-type filename extension) '(in))
      (open filename '(in))))

(lset *modules* (make-table '*modules*))

(define (orbit-vax-setup directory)
  (set *object-file-extension* 'vo)
  (set *information-file-extension* 'vi)
  (set *noise-file-extension* 'vn)
  (set (table-entry *modules* 'constants) `(,directory vconstants))
  (set (table-entry *modules* 'primops)   `(,directory vaxprimops))
  (set (table-entry *modules* 'arith)     `(,directory vaxarith))
  (set (table-entry *modules* 'low)       `(,directory vaxlow))
  (orbit-setup directory)
  nil)

(define (orbit-m68-setup directory)
  (set *object-file-extension* 'mo)
  (set *information-file-extension* 'mi)
  (set *noise-file-extension* 'mn)
  (set (table-entry *modules* 'constants) `(,directory mconstants))
  (set (table-entry *modules* 'primops)   `(,directory m68primops))
  (set (table-entry *modules* 'arith)     `(,directory m68arith))
  (set (table-entry *modules* 'low)       `(,directory m68low))
  (orbit-setup directory)
  nil)

(define (orbit-setup directory)
  (set (table-entry *modules* 'base)       `(,directory base))
  (set (table-entry *modules* 'locations)  `(,directory locations))
  (set (table-entry *modules* 'carcdr)     `(,directory carcdr))
  (set (table-entry *modules* 'predicates) `(,directory predicates))
  (set (table-entry *modules* 'open)       `(,directory open))
  (set (table-entry *modules* 'aliases)    `(,directory aliases))
  (set (table-entry *modules* 'genarith)    `(,directory genarith))
  t)

(define (module-name->filename name)
  (->filename (cond ((table-entry *modules* name)
                     => identity)
                    ((and (pair? name)
                     (table-entry *modules* (car name)))
                     => (lambda (n)
                          (cons n (cdr name))))
                    (else name))))

;;; FE/TOP
(define (reload-support module-name)
  (set (table-entry definition-tables module-name) '#f)
  (get-definition-table module-name))


    

