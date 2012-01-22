(herald load (env tsys))

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

;;; globaL table of loaded modules
;;; each env has a list of instantiated modules

;;;; LOAD and friends.

;;; The main confusion to surmount before understanding this code
;;; is that when you ask to load a file, there are about five
;;; different filenames potentially involved, and the trick is to
;;; keep them all straight and to check consistency at the right
;;; places.
;;; These names are:
;;;   (a) The name appearing in the source file's HERALD.
;;;   (b) The name by which the compiler found the source file.
;;;   (c) The name of the file the compiler wrote.
;;;   (d) The name by which the file was found at load time.
;;;   (e) The name given in the REQUIRE form (this is obsolescent).
;;; (Also note that in cases (b), (c), and (d), there are really
;;; two file names involved, the "given name" and the "true name";
;;; but the current filename facility doesn't make that distinction
;;; yet.)


;;; Entry points for LOAD

(define-simple-switch load-noisily? boolean? t)
(define-simple-switch source-file-extension symbol? 't)
(define-simple-switch print-load-message? boolean? t)

;;; Someday make the ENV-OPTION be not optional.

(define (load spec . env-option)
  (load-aux load            spec env-option t   (load-noisily?)))

(define (load-if-present spec . env-option)
  (load-aux load-if-present spec env-option nil (load-noisily?)))

(define (load-noisily spec . env-option)
  (load-aux load-noisily    spec env-option t   t))

(define (load-quietly spec . env-option)
  (load-aux load-quietly    spec env-option t nil))

(define (load-quietly-if-present spec . env-option)
  (load-aux load-quietly-if-present spec env-option nil nil))

(define (load-silently spec . env-option)
  (bind (((print-load-message?) nil)
         ((print-env-warnings?) nil))
    (load-aux load-silently spec env-option t nil)))

(define (load-silently-if-present spec . env-option)
  (bind (((print-load-message?) nil)
         ((print-env-warnings?) nil))
    (load-aux load-silently-if-present spec env-option nil nil)))

;;; Deal with defaulting of second arg.  T has no &optionals.

(lset +load-noisily?+ t)

(define (load-aux load spec env-option complain-if-missing? noisily?)
  (bind ((+load-noisily?+ noisily?))
    (let ((env (cond ((null? env-option) nil)
                     (else
                      (cond ((not (null? (cdr env-option)))
                             ;; There ought to be some clean way to do
                             ;; optional arguments in t.
                             (error "too many arguments~%  ~s"
                                    `(,(identification load)
                                       ,spec
                                       ,@env-option))))
                      (car env-option)))))
      (load-file spec env complain-if-missing?))))

;;; Fills in an extension if one wasn't supplied, and loads the file.
;;; If the SPEC is a string then no extension is filled in.

(define (load-file filespec env complain?)
  (with-open-ports ((port (open-default-filename filespec complain?)))
    (cond ((port? port)
           (if (iob? port) (set (iob-id port) (port-truename port)))
           (load-port port (or env (repl-env))))
          (else nil))))

(lset *load-level* 0)

(define (load-port port env)
  (let ((name (port-name port)))
    (print-load-message name env (loaded? (port-name port) env))
    (bind ((*load-level* (fx+ *load-level* 1)))
      (let* ((ftype (filename-type (->filename name)))
             (ftype (if (string? ftype) 
                        (string->symbol (string-upcase ftype))
                        ftype))
            ;++ this will become (read port)
             (form  (cond ((eq? ftype (object-file-type (local-machine)))
                           (let ((comex (read-comex port)))
                             (set (loaded-modules port) comex)
                             comex))
                          (else (read port)))))
;        (check-compatibility port herald env)
        (if (comex? form) 
            (instantiate-comex form env name)
            (instantiate-source port form env))))))
                                                             
;++ INSTANTIATE-COMEX and INSTANTIATE-SOURCE (bad name) will
;++ have the same interface eventually and LOAD-PORT will be
;++ cleaned up.



(define (instantiate-comex comex env . id)
  (let* ((id     (if (null? id) nil (car id)))
         (h      (vref (comex-objects comex) 1))
         (herald (parse-herald (car h) (cdr h)))
	 (version (comex-module-name comex)))
    (if (not (and (number? version) (= version version-number)))
	(error "Obsolete object file ~s" (herald-filename herald)))
    (receive (unit code) (install-comex comex env)
      (set (weak-table-entry code-unit-table code) unit)
      (add-to-population code-population code)
      (if id (set (loaded-file env id) (AUGMENT-UNIT unit ID)))
      (run-compiled-code unit env))))

(define (instantiate-source port form env)
  (receive (h port)
           (cond ((and (pair? form) (eq? (car form) 'herald))
                  (let ((h (parse-herald (cadr form) (cddr form))))
                    (cond ((herald-read-table h)
                           => (lambda (rt)
                                (set (port-read-table port)
                                     (eval rt env)))))
                    (return h port)))
                 (else
                  ;++(warning "file ~S has no HERALD form.~%"
                  ;++         (port-name port))
                  (return (make-default-herald (port-name port))
                          (cons-port form port))))
    (check-compatibility port h env)
    (let ((unit (standard-compile-port port (get-target-syntax h env) h)))
      (if (port-name port) (set (loaded-file env (port-name port)) (AUGMENT-UNIT unit (PORT-NAME PORT))))
      (run-compiled-code unit env))))


;;; Random utilities for above.

;;; Given a compiled code object, return the environment into which
;;; it believes it would like to be loaded, by evaluating the ENV
;;; clause from the object's HERALD structure.
;++ Note: This is slightly changed from 2.9.  The loader tries to
;++       load the code into one of ENV, TARGET-ENV, or REPL-ENV
;++       in that order.

(define (get-target-syntax herald env)
  (cond ((herald-syntax-table herald)
         => (lambda (st) (eval st env)))
        (else
         (env-syntax-table env))))

;;; Modules

;;; A module is an object which represents a partially evaluated
;;; expression.  When a module is instantiated its free variables
;;; are bound in the environment in which the module is instantiated.
;;; A module can be instantiated in more than one environment in
;;; which case the pure part of the module is shared.

(define loaded-modules
  (let ((table (make-weak-table 'loaded-module-table)))
    (object (lambda (port)
              (let ((entry (weak-table-entry table (port-name port))))
                (return (car entry) (cdr entry))))
      ((setter self)
       (lambda (port module)
         (set (weak-table-entry table (port-name port))
              (cons (file-write-date port) module)))))))

(define (loaded? name env)
  (let ((id (cond ((string? name) name)
                  ((filename? name) (filename->string name))
                  ((port? name)     (port-name name))
                  (else
                   (error "invalid identifier for file - ~a" name)))))
    (true? (loaded-file env (FILENAME->STRING (EXPAND-FILENAME (->FILENAME id)))))))

;;; Check to see if the module is loaded into the environment
;;; and if so check to see that it hasn't been modified.

(define (open-default-filename filespec complain?)
  (let ((open (if complain? open maybe-open))
        (fname (GET-DEFAULT-FILENAME filespec)))
     (OPEN FNAME 'IN)))

(define (get-default-filename filespec)
   (let* ((fname  (->filename filespec))
          (ftype  (filename-type fname))
          (src    (filename-with-type fname (source-file-extension)))
          (bin    (filename-with-type fname (object-file-type (local-machine)))))
     (cond ((or (string? filespec) (not (null? ftype)))
            fname)
           ((file-exists? bin)
            (xcase (load-out-of-date-action)
               ((binary) bin)
               ((source) src)
               ((newer)  (if (and (file-exists? src) (file-newer? src bin))
                             src
                             bin))
               ((recompile) (if (and (file-exists? src) (file-newer? src bin))
                                (if (maybe-comfile src bin) bin src)
                                bin))
               ((warn) (if (and (file-exists? src) (file-newer? src bin))
                           (if (print-load-message?)
                               (let ((msg-port (standard-output)))
                                  (comment-indent msg-port (fx* *load-level* 2))
                                  (format msg-port "~&Warning: ~a has changed since it was last compiled~%" src)
                                  (force-output msg-port))))
                       bin)
               ((query) (if (and (file-exists? src) (file-newer? src bin)
                                 (yes-or-no? "~&File ~a is out of date.  Recompile " src))
                             (if (maybe-comfile src bin) bin src)
                             bin))))
           (else src))))
          
(define (maybe-comfile src bin)
   (cond ((maybe-open bin '(out))  ;; hack!
          => (lambda (port)
                (close port)
                (comfile src)
                '#t))
         (else (if (print-load-message?)
                   (let ((msg-port (standard-output)))
                     (comment-indent msg-port (fx* *load-level* 2))
                     (format msg-port "~&Can't compile ~a~%" (filename->string src))
                     (force-output msg-port)))
               '#f)))


(define-simple-switch load-out-of-date-action
                      (lambda (sym)
                         (case sym
                            ((binary)    t)    
                            ((source)    t)
                            ((query)     t)
                            ((recompile) t)
                            ((WARN)      T)
                            ((NEWER)     T)
                            (else        nil)))
                      'warn)

;++ where does this belong?

(define (yes-or-no? fmt . args)
  (iterate loop ()
    (apply prompt (standard-output) `(,fmt " (Y/N)? ") args)
    (let ((val (read (standard-input))))
      (case val
        ((yes y) '#t)
        ((no  n) '#f)
        (else
         (format t ";** Invalid response (~a)." val)
         (loop))))))

(define (check-compatibility port herald env) (no-value))

(comment
  (define (check-compatibility port herald env)
    (let ((pname (->filename (port-name port)))
          (hname (herald-filename herald)))
      (cond ((not (filenames-compatible? pname hname))
             (warning (list "(HERALD ~s ...)~%"
                            ";**~12tdoesn't match (LOAD ~s ...)~%")
                      hname
                      pname))))
    (let ((target (herald-environment herald)))
      (if target
          (let ((target-env (eval target env)))
            (if (neq? target-env env)
                (warning (list "(HERALD ... (ENV ~a) ...)~%"
                               ";**~12tdoesn't match (LOAD ... ~a)~%")
                         target-env
                         env)))))
    (no-value))
 )
;;; The screw case for ancient REQUIRE's is F1 = FOO and F2 =
;;; "~bar/foo.t"

(define (filenames-compatible? f1 f2)
  (or (null? f1)
      (null? f2)
      (and (alikev? (filename-name f1) (filename-name f2))
           (let ((d1 (filename-dir  f1))
                 (d2 (filename-dir  f2)))
             (or (null? d1)
                 (null? d2)
                 (alikev? d1 d2))))
      (and (null? (filename-dir f1))
           (string? (filename-name f1)))
      (and (null? (filename-dir f2))
           (string? (filename-name f2)))))

(define (print-load-message name env reloading?)
  (cond ((print-load-message?)
         (let ((out (standard-output)))  ; foo
           (comment-indent out (fx* *load-level* 2))
           (format out "~a ~a into ~a~%"
                   (if reloading? "Reloading " "Loading ")
                   name
                   ;; Hack to make message more concise
                   (or (print-info env) env))
           (force-output out))))
  (no-value))

;;; Print value loaded.  Called by STANDARD-COMPILE-port.

(define (load-print vals)
  (cond (+load-noisily?+
         (let ((out (standard-output)))
           (walk (lambda (val)
                   (cond ((not (repl-wont-print? val))
                          (print (or (identification val) val) out)
                          (space out)))
                   (no-value))
                 vals)
           (force-output out)))))

;++ flush REQUIRE this later.

;;; (*REQUIRE id spec env) - calls to this result from expansions
;;;  of REQUIRE forms.
;;; Make this smarter some day.

;++ where is this used?
;(define *module-population* (make-population '*module-population*))

(define *base-support-env* t-implementation-env)
(define *standard-support-env* standard-env)

(define (*require id filespec env)
   (let ((fname (EXPAND-FILENAME (GET-DEFAULT-FILENAME filespec))))
      (cond ((AND (loaded? fname env)
                  (SAME-AS-BEFORE? FNAME ENV))
             (cond ((print-load-message?)
                    (let ((msg-port (standard-output)))
                      (comment-indent msg-port (fx* *load-level* 2))
                      (format msg-port "Already loaded ~a~%" (FILENAME->STRING fname))
                      (force-output msg-port))))
             (undefined-value "File already loaded"))
            (else
             (load-file (FILENAME->STRING fname) env t)))))

(define-operation (unit-write-date unit)
   0)

(define (augment-unit unit id)
   (let ((write-date (if (null? id) 0 (file-write-date id))))
      (join (object nil ((unit-write-date self) write-date))
            unit)))

;; This code duplicates some of LOADED?, but rather than hack LOADED? I separated this out
;; since I don't know what else depends on LOADED?.

(define (same-as-before? name env)
   (let* ((fname (expand-filename
                    (->filename
                     (cond ((string? name)   name)
                           ((filename? name) (filename->string name))
                           ((port? name)     (port-name name))
                           (else
                            (error "invalid identifier for file - ~a" name))))))
          (id (filename->string fname))
          (unit (loaded-file env id)))
       (cond ((> (file-write-date fname) (unit-write-date unit))
              (cond ((print-load-message?)
                     (let ((msg-port (standard-output)))
                       (comment-indent msg-port (fx* *load-level* 2))
                       (format msg-port "~a has changed since it was loaded~%" id)
                        (force-output msg-port))))
              '#f)
             (else '#t))))


