(herald vm_system (env tsys))

;;; This switch is modified by the suspender when a 
;;; system is released.

(define-simple-switch experimental? boolean? t)

;;; Accessors for system and task global values.
;++ These are redefined here because the compiler can't
;++ produce the closed compiled forms.

(define PROCESS-GLOBAL
  (let ((slot (lambda (offset)
                (fx-ashr (fx- (fx+ (fx-negate %%task-header-offset) offset) 4) 2))))
    (object (lambda (offset)
              (extend-elt (current-task) (slot offset)))
      ((setter self)
       (lambda (offset val)
         (set (extend-elt (current-task) (slot offset)) val))))))

(define SYSTEM-GLOBAL
  (let ((slink-offset (lambda (n) (fx- (fx-ashr (fx+ n 3) 2) 1))))
    (object (lambda (offset)
              (extend-elt (gc-pair->extend *the-slink*)
                          (slink-offset offset)))
      ((setter self)
       (lambda (offset val)
         (set (extend-elt (gc-pair->extend *the-slink*)
                          (slink-offset offset)) val))))))

;;; ---------- Processor

(define-operation (processor-type processor))
(define-predicate mc68000-processor?)
(define-predicate vax-processor?)

;;; ---------- Operating system

(define-operation (object-file-type      os))
(define-operation (information-file-type os))
(define-operation (noise-file-type       os))
(define-operation (os-type os))
(define-operation (machine-type os))
(define-operation (page-size os))
(define-predicate aegis-os?)
(define-predicate unix-os?)
(define-predicate vms-os?)

;;; ---------- Embedded systems

(lset *embedded-systems* '())

(define (system-present? name)                 ;++ useful?
  (any? (lambda (system)
          (eq? (system-name system) name))
        *embedded-systems*))

(define-operation (system-name          system))
(define-operation (major-version        system))
(define-operation (minor-version        system))
(define-operation (link-edit            system))
(define-operation (build-date           system))
(define-operation (initialize           system))
(define-operation (re-initialize        system))
(define-operation (configure-system     system))
(define-operation (set-system-edit      system)) ;??
(define-operation (system-modules       system))

(define-structure-type system
                       %name
                       %major
                       %minor
                       %link-edit
                       %build-date
                       %note
                       %initializer
                       %re-initializer
                       %configurer       ; local modifications
                       %modules          ; source tree
  (((major-version self)        (system-%major self))
   ((minor-version self)        (system-%minor self))
   ((link-edit self)            (system-%link-edit self))
   ((build-date self)           (system-%build-date self))
   ((set-system-edit self n)    (set (system-%link-edit self) n))
   ((initialize        self)    ((system-%initializer self) self))
   ((re-initialize     self)    ((system-%re-initializer self) self))
   ((configure-system self)     ((system-%configurer self) self))
   ((system-name self)          (system-%name self))
   ((system-modules self)       (system-%modules self))
   ((print self port)
    (format port "~&~a~a ~s.~s (~s) ~a/~a  ~a~%"
            (system-%name  self)
            (if (experimental?) " (Beta)" "")
            (system-%major self)
            (system-%minor self)
            (system-%link-edit  self)
            (processor-type (local-processor))
            (os-type (local-os))
            (system-%note self)))))

(set (system-%major          (stype-master system-stype)) 3)
(set (system-%minor          (stype-master system-stype)) 0)
(set (system-%link-edit      (stype-master system-stype)) 0)
(set (system-%note           (stype-master system-stype)) "")
(set (system-%initializer    (stype-master system-stype)) true)
(set (system-%re-initializer (stype-master system-stype)) true)
(set (system-%configurer     (stype-master system-stype)) true)

(define (create-system name major minor link
                       init re-init config note modules)
  (let ((sys  (make-system)))
    (set (system-%name           sys) name)
    (set (system-%major          sys) major)
    (set (system-%minor          sys) minor)
    (set (system-%link-edit      sys) link)
    (set (system-%initializer    sys) init)
    (set (system-%re-initializer sys) re-init)
    (set (system-%configurer     sys) config)
    (set (system-%note           sys) note)
    (set (system-%modules        sys) modules)
    (push *embedded-systems* sys)
    sys))


;;; ---------- The Virtual Machine

;;; The VM must be the first embedded system.

(define t-copyright-notice "Copyright (C) 1986 Yale University")

(define vm-system
  (create-system "Virtual Machine" 1 0 2
                 initialize-virtual-machine
                 re-initialize-virtual-machine
                 true
                 t-copyright-notice
                 '()))

(define (INITIALIZE-VIRTUAL-MACHINE system)
  (ignore system)
  ;; Set the break-level to 0 so that the second level prompt will
  ;; be printed if an error occurs during re-initialization.
  ;; During initialization this is redundant.
  (set *break-level*    0)
  (if (gc-present?) (initialize-areas)))

(define (re-initialize-virtual-machine system)
  (ignore system)
  (initialize-condition-system)
  (initialize-standard-ports)
  (re-initialize object-hash)
  (if (gc-present?) (initialize-areas)))

;;; Both the loader and file system stuff must be available
;;; before executing BOOT-ADJUST-INITIAL-UNITS.

(define (REALLY-INITIALIZE-SYSTEMS INITIALIZER)
  (print (car *embedded-systems*) (standard-output))
  (let ((systems (reverse *embedded-systems*)))
    (walk1 initializer systems)
    (walk1 configure-system systems))
  (no-value))

(define (INITIALIZE-SYSTEMS)
  (really-initialize-systems initialize))

(define (RE-INITIALIZE-SYSTEMS)
  (really-initialize-systems re-initialize))

;++ where should this stuff go?
;(define tvm-env      (make-base-environment 'tvm-env))
;(define t-system-env (make-locale tvm-env 't-system-env))
;(define t-implementation-env t-system-env)

(define t-implementation-env
  (make-base-environment 't-implementation-env))
(define tvm-env      t-implementation-env)
(define t-system-env tvm-env)

(initialize-condition-system)
(initialize-weak-alist-pool)
