(herald tsystem (env tsys))

;;;; T Configuration file

;;; ---------- Utilities for systems


;;; Fix file names are "<system-name>FIX<edit-number>.T" in the
;;; system directory.

(define (load-fix-file system env)
  (let* ((name (format nil "~a~a~a"
                       (string-downcase! (symbol->string (system-%name system)))
                       (if (experimental?) "xfix" "fix")
                       (link-edit system)))
         (fname (make-filename nil (the-t-system-directory) name nil)))
    (load-quietly-if-present fname env)))

;;; Init file names are "<system-name>_init<edit-number>.T" in the
;;; system directory.

(define (load-init-file system env)
  (let* ((name  (format nil "~ainit" (system-name system)))
         (fname (make-filename nil (the-init-file-directory) name nil)))
    (load-quietly-if-present fname env)))

;;; System initialization stuff

;;; Environment initialization.  Make a "Standard environment,"
;;; i.e. a fresh environment which has copies of all the "released"
;;; system bindings in it.

(define standard-env (make-empty-locale 'standard-env))
(define standard-syntax-table (env-syntax-table standard-env))

(define (initialize-standard-env)
  (export-tsys standard-env)
  (*define standard-env 'standard-syntax-table (env-syntax-table standard-env))
  ;++ flush at 3.1
  (*define standard-env '*standard-syntax-table* (env-syntax-table standard-env))
    ;++ gross hack to prevent crawl from blowing out.  what to do?
    (*lset standard-env '*obj* nil)
    (no-value))

;;; Create a user environment inferior to the standard environment.
;;; The variable USER-ENV will be defined in the standard
;;; environment to be the new environment.

(define user-env     (make-inferior-locale standard-env 'user-env))

(define (initialize-t-system system)
  (set *z?* '#f)
  (set *top-level* standard-top-level)
  (boot-adjust-initial-units)
  (initialize-local-fs)
  (initialize-local-system)
  (initialize-standard-env)
    ;++ temporary grossness
  (*define tvm-env      '*standard-env* standard-env)
  (*define standard-env '*standard-env* standard-env)
  (*define tvm-env      '*scratch-env*  user-env)
  (*define standard-env '*scratch-env*  user-env)
  (load-fix-file system t-implementation-env)
  (set (fancy-symbol-printing?) t)
  (set (repl-env) user-env))

(define (re-initialize-t-system system)
  (initialize-local-fs)
  (initialize-local-system)
  (load-fix-file system t-implementation-env)
  (initialize-repl user-env))

(define t-system
  (create-system 't (fx/ version-number 10) (fx-rem version-number 10) 4
                 initialize-t-system
                 re-initialize-t-system
                 (lambda (system)
                   (or (load-init-file system user-env)
                       ;++ temp until 3.1
                       (load-quietly-if-present
                        (make-filename nil (the-init-file-directory) 'init nil)
                        user-env)))
                 "Copyright (C) 1988 Yale University"
                 '()))

(define (version . arg)
  (if (null? arg) t-system (car arg)))

;;; Utility to load Orbit and Scheme and then suspend the system.

(define (load-and-suspend-system filespec . hack)
  (if hack (gc))
  (load '(build oload) t-implementation-env)
  ((*value orbit-env 'load-orbit))
  (let ((suspend-env (make-locale t-implementation-env 'suspend-env)))
    (*define t-implementation-env 'suspend-env suspend-env)
    (load '(link lp_table) suspend-env)
    (case (machine-type (local-machine))
       ((apollo) (load '(link aem68suspend) suspend-env))
       ((sun)    (load '(link sunsuspend) suspend-env))
       ((hp)     (load '(link hpsuspend) suspend-env))
       ((vax/unix)    (load '(link unvaxsuspend) suspend-env)))
     (load '(link suspend) suspend-env)
    (*define t-implementation-env 'system-suspend (*value suspend-env 'system-suspend)))
    (load '(tscheme scheme) t-implementation-env)
  (gc)                  
  (if hack (set (process-global task/area-limit) (area-limit *old-space*)))
  (system-suspend filespec nil))


;;; Standard top level, etc.

(lset *TOP-LEVEL-GREETING* "T Top level")

(define (STANDARD-TOP-LEVEL)
;++    (reset-stack-guard)
  (set *z?* nil)
  (t-breakpoint *top-level-greeting*))

(define (T-RESET)
  (set *top-level* standard-top-level)
  (**reset** nil))

;;; End of basic system initialization sequence.

;;; Control falls from here either into other embedded systems or
;;; into (*TOP-LEVEL*).
