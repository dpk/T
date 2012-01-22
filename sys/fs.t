(herald fs (env tsys))

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


;;;; File systems and filename translation


;;; ---------- File system types:

;;; Convert symbol pname to case preferred by file system types.

(define-operation (->preferred-case fstype string)
  (string-downcase string))             ; lower

(define-operation (special-symbols fstype) '(\/\/ \/ \. \.. \~))


;;; Called if logical name spec was a string.

(define-operation (massage-dir-string fstype dir)
  (string-append dir "/"))

(define-operation (massage-dir-pair fstype dir)
  (with-buffers ((buf))
    (let* ((1st  (car dir))
           (rest (cond ((memq? (car dir) (special-symbols fstype))
                        (display 1st buf)
                        (cdr dir))
                       (else dir))))
      (iterate loop ((l rest))
        (cond ((null? l)
               (buffer->string buf))
              (else
               (display (->preferred-case fstype (symbol->string (car l))) buf)
               (write-char buf #\/)
               (loop (cdr l))))))))

;;; Called if ln spec was a symbol not in logical name table.

(define-operation (massage-logical-name fstype ln)
  (string-append (->preferred-case fstype (symbol->string ln)) "/"))

(define-operation (massage-gen-part fstype gen)
  (ignore fstype gen)
  "")

(define-operation (query-fs-names fstype fs)
  (read-fs-names-from 
   (make-filename fs 
                  (the-t-system-directory) 
                  'localfs 
                  't)))

;;; Utility for QUERY-FS-NAMES

(define (read-fs-names-from filename)
  (with-open-ports ((port (maybe-open filename 'in)))
    (cond ((and port (graphic? (peekc port)))
           (let ((probe (read port)))
             (cond ((pair? probe) probe)
                   (else (list probe)))))
          (else nil))))     ; Assume () = false

;;; Initialize the local file system names.

(define (initialize-local-fs)
  (let ((fs (make-local-fs)))
    (set (local-fs) fs)
    (walk (lambda (name)
            (set-fs-name fs name))
          (query-fs-names (fs-type fs) fs))
    fs))

;;; Parse a filespec string.

;++ These are need for parsing strings

(define-operation (parse-filespec fstype fs string)
  (make-filename fs nil string nil))

;;; FS type table (?)

(define fs-type-table (make-table 'fs-type-table))

;;; Anonymous 

(define anonymous-fs?
  (object (lambda (fs) (eq? (fs-type fs) anonymous-fs?))
    ((parse-filespec self fs string)
     (ignore fs)
     (string->filename string #\/ #\.))
    ((print self port)
     (format port "#{File-system-type~_Anonymous}"))))

;;; Aegis

(define aegis-fs?
  (let ((specials '(\/\/ \/ \. \\ \~)))
    (object (lambda (fs) (eq? (fs-type fs) aegis-fs?))
      ((special-symbols self) specials)
      ((massage-logical-name self ln)
       (let ((ln-string (string-downcase (symbol->string ln))))
         (cond ((memq? ln specials)
                ln-string)
               (else 
                (string-append "~" ln-string "/")))))
      ((parse-filespec self fs string)
       (ignore fs)
       (string->filename string #\/ #\.)) 
      ((print self port)
       (format port "#{File-system-type~_AEGIS}")))))

(define (make-aegis-fs names)
  (make-fs aegis-fs? names))

;;; UNIX

(define unix-fs?
  (let ((specials '(\/\/ \/ \. \.. \$)))
    (object (lambda (fs) (eq? (fs-type fs) unix-fs?))
      ((special-symbols self) specials)
      ((massage-logical-name self ln)
       (let ((ln-string (symbol->string ln)))
         (cond ((memq? ln specials)
                ln-string)
               (else 
                (string-append "$" (string-upcase ln-string) "/")))))
      ((query-fs-names self fs)
       (or (read-fs-names-from (make-filename fs nil "/etc/sysname" nil))
           (read-fs-names-from
            (make-filename fs
                           (the-T-system-directory)
                          'localfs 
                          't))))
      ((parse-filespec self fs string)
       (ignore fs)
       (string->filename string #\/ #\.)) 
      ((print self port)
       (format port "#{File-system-type~_UNIX}")))))

(define (make-unix-fs names)
  (make-fs unix-fs? names))

;;; VMS

(define vms-fs?
  (object (lambda (fs) (eq? (fs-type fs) vms-fs?))
    ((massage-dir-pair fstype dir)
     (with-buffers ((buf))
       (write-char buf #\[)
       (display (car dir) buf)
       (iterate loop ((l (cdr dir)))
         (cond ((null? l)
                (write-char buf #\])
                (buffer->string buf))
               (else
                (write-char buf #\.)
                (display (car l) buf)
                (loop (cdr l)))))))
    ((massage-logical-name self ln)
     (string-append (symbol->string ln) ":"))
    ((->preferred-case self string) string)
    ((massage-gen-part self gen)
     (cond ((fixnum? gen) (format nil ".~S" gen))
           ((eq? gen 'newest) ".0")
           (else nil)))
    ((parse-filespec self fs string)
     (ignore fs)
     (string->filename string #\/ #\.)) 
    ((print self port)
     (format port "#{File-system-type~_VMS}"))))

(define (make-vms-fs names)
  (make-fs vms-fs? names))

;;; ---------- File systems:

;;; File systems are instantiations of file system types.

(define-operation (fs-type fs))         ; Return an fs-type object for the fs
(define-operation (fs-name fs))         ; Return a symbol naming the fs

(define-settable-operation (logical-name fs ln))
(define set-logical-name (setter logical-name))

(define-settable-operation (fs-maybe-open-proc fs))
(define set-fs-maybe-open-proc (setter fs-maybe-open-proc))

(define-operation (set-fs-name fs newname))

(define-operation (maybe-open-filename fs filename mode))

(define-operation (fs-parse-filespec fs string)
  (make-filename fs nil string nil))

(define-predicate file-system?)

(define (make-fs fstype names)          ;++ Note internet domain(s) also?
  (let ((name   (car names))
        (access (lambda (fs file mode)
                  (ignore fs)
                  (maybe-open-port file mode))))
    (let ((ln-table (make-table `(logical-names ,name))))
      (let ((fs (object nil
                  ((fs-type self) fstype)
                  ((logical-name self ln)
                   (table-entry ln-table ln))
                  ((set-logical-name self ln def)
                   (set (table-entry ln-table ln) def))
                  ((maybe-open-filename self filename mode)
                   (access self filename mode))
                  ((fs-maybe-open-proc self) access)
                  ((set-fs-maybe-open-proc self val)
                   (set access val))
                  ((fs-name self)
                   (if (null? name) 'anonymous name))
                  ((set-fs-name self newname)
                   (cond ((not (memq? newname names))
                          (push names newname)
                          (set (table-entry fs-table newname) self)
                          (if (null? name) (set name newname)))))
                  ((fs-parse-filespec self string)
                   (parse-filespec fstype self string))
                  ((file-system? self) t)
                  ((print self port)
                   (format port "#{File-system~_~s}" 
                           (fs-name self))))))
        (walk (lambda (name)
                (set (table-entry fs-table name) fs))
              names)
        fs))))


;;; Map file-system names to corresponding file-system-objects.

(define fs-table (make-table 'fs-table))

(define-predicate filename?)
(define-operation (filename->string filename))
(define-operation (filename-fs   filename))
(define-operation (filename-dir  filename))
(define-operation (filename-leaf filename))
(define-operation (filename-name filename))
(define-operation (filename-type filename))
(define-operation (filename-generation filename))


;;; ---------- MAKE-FILENAME

(define (make-filename fs ln name . optionals)
  (let ((type (car optionals))
        (gen (cadr optionals))
        (cached-string nil))
    (object nil
            ((maybe-open self mode)
             (maybe-open-filename (->fs fs) self mode))
            ((filename->string self)
             (or cached-string
                 (set cached-string
                      (let ((z (resolve-logical-name (->fs fs)
                                                     ln
                                                     '())))
                        (fs-filename->string (fs-type (car z))
                                             (car z)
                                             (cdr z)
                                             name
                                             type
                                             gen)))))
            ((filename-fs self) fs)
            ((filename-dir self) ln)
            ((filename-leaf self)
             (let ((z (resolve-logical-name (->fs fs) ln '())))
               (fs-filename-leaf (fs-type (car z))
                                 (car z)
                                 name
                                 type
                                 gen)))
            ((filename-name self) name)
            ((filename-type self) type)
            ((filename-generation self) gen)
            ((filename? self) t)
            ((print self port)
             (bind ((*write-symbol* plain-write-symbol))
               (format port "#[Filename~_~S~_~S~_~S"
                       (if (file-system? fs) (fs-name fs) fs) ln name)
               (if (or type gen)
                   (format port "~_~S" type))
               (if gen
                   (format port "~_~S" gen))
               (writec port #\])))
            ((display self port)
             (writes port (filename->string self))))))

(define (->fs thing)
  (cond ((null? thing) (local-fs))
        ((and (symbol? thing) (table-entry fs-table thing)))
        ((file-system? thing) thing)
        (else 
         (if (and (neq? thing 'anonymous) (neq? thing 'yale-ring))
             (warning "unknown file system - ~S - using (LOCAL-FS)~%" thing))
         (local-fs))))

(define (make-filename-for-read key port rt)
  (ignore key)
  (let* ((l (read-to-right-bracket port #\] rt))
         (n (length l)))
    (cond ((or (fx< n 3) (fx> n 5))
           (read-error port "illegal filename syntax - ~S" l))
          (else
           (apply make-filename l)))))

(define (->filename obj)
  (cond ((filename? obj) obj)
        ((string? obj)
         (fs-parse-filespec (local-fs) obj))
        ((symbol? obj)
         (make-filename nil nil obj nil))
        ((and (proper-list? obj)
              (not (null? obj))
              (not (null? (cdr obj))))
         (apply make-filename nil obj))
        (else
         (->filename (error "can't coerce to filename~%  (~S ~S)"
                            '->filename obj)))))

(define (filespec? obj)
  (or (filename? obj)
      (string? obj)     ;should check syntax
      (symbol? obj)
      (and (proper-list? obj)
           (not (null? (cdr obj)))      ;(cdr '()) => ()
           (destructure (((dir name type gen . z) obj)) ;(cdr '()) => ()
             (and (null? z)
                  (or (null? dir) (symbol? dir) (string? dir))
                  (or (symbol? name) (string? name))
                  (or (null? type) (symbol? type) (string? type))
                  (or (integer? gen) (null? gen)))))))

(define (filename-equal? n1 n2)
  (let ((foo (lambda (x y)
               (or (eq? x y)
                   (and (string? x) (string? y) (string-equal? x y))))))
    (and (eq? (filename-fs   n1) (filename-fs   n2))
         (or (and (foo (filename-dir  n1) (filename-dir  n2))
                  (foo (filename-name n1) (filename-name n2))
                  (foo (filename-type n1) (filename-type n2)))
             (string-equal? (filename->string n1)
                            (filename->string n2))))))

;;; Handy utilities.

(define (filename-with-type filename type)
  (make-filename (filename-fs filename)
                 (filename-dir filename)
                 (filename-name filename)
                 type
                 (filename-generation filename)))

;++ how about ->defaulted-filename.
(define (->filename-with-defaults name fs dir . rest)
  (let* ((fname   (expand-filename name))
         (xfs     (filename-fs   fname))
         (xdir    (filename-dir  fname))
         (xname   (filename-name fname))
         (xtype   (filename-type fname))
         (xgen    (filename-generation fname))
         (type    (car rest))
         (gen     (cadr rest)))
    (let ((fs   (if xfs   xfs   fs))
          (dir  (if xdir  xdir  dir))
          (type (if xtype xtype type))
          (gen  (if xgen  xgen  gen)))
      (make-filename fs dir xname type gen))))

;;; ---------- FS-FILENAME->STRING

;;; Synthesize a string naming a given file in file-system-native syntax.

(define-operation (fs-filename->string self fs ln name type gen)
  (string-append (namestring-dir-part fs ln)
                 (namestring-name-part fs name)
                 (namestring-type-part fs type)
                 (namestring-gen-part fs gen)))

(define-operation (fs-filename-leaf self fs name type gen)
  (string-append (namestring-name-part fs name)
                 (namestring-type-part fs type)
                 (namestring-gen-part fs gen)))

;;; A directory component may be one of:
;;; - null, for the current working directory;
;;; - a symbol, for a logical name;
;;; - a list a component pathnames; or
;;; - a string, for the actual name of a directory (e.g. "<F.T.X.SYS>").

(define-operation (namestring-dir-part fs ln)
  (cond ((null? ln) "")
        ((string? ln)
         (massage-dir-string (fs-type fs) ln))
        ((symbol? ln)
         (massage-logical-name (fs-type fs) ln))
        ((list? ln)
         (massage-dir-pair (fs-type fs) ln))
        (else
         (namestring-dir-part
          fs
          (error "ill-formed directory spec~%  (~S~_~S~_~S~_...)"
                 'filename->string fs ln)))))

;;; The name part may be either a string or a symbol.
;;; [Note: for VMS, we should probably truncate to 9 characters.]

(define (namestring-name-part fs name)
  (cond ((symbol? name)
         (->preferred-case (fs-type fs) (symbol->string name)))
        ((string? name)
         name)
        (else
         (namestring-name-part
          fs
          (error "ill-formed filename spec~%  (~S~_~S~_...~_~S~_...)"
                 'filename->string fs name)))))

(define (namestring-type-part fs type)
  (cond ((null? type) "")
        ((symbol? type)
         (let ((fstype (fs-type fs)))
           (string-append "." (->preferred-case fstype (symbol->string type)))))
        ((string? type)
         (string-append "." type))
        (else
         (namestring-type-part
          fs
          (error "ill-formed filename type~%  (~S~_~S~_...~_~S)"
                 'filename->string fs type)))))

(define (namestring-gen-part fs gen)
  (cond ((null? gen) "")
        ((and (or (symbol? gen)
                  (fixnum? gen))
              (massage-gen-part (fs-type fs) gen)))
        (else
         (namestring-gen-part
          fs
          (error "ill-formed filename generation~%  (~S~_~S~_...~_~S)"
                 'filename->string fs gen)))))

;;; ---------- Logical names

;;; Logical names internal to T.  The value of a logical name must
;;; be a pair (file-system . logical-name).  LOGICAL-NAME is a symbol,
;;; a list, or a search path.

(define (resolve-logical-name fs ln circle)
  (let ((z (cons fs ln)))
    (cond ((not (symbol? ln)) z)
          (else
           (cond ((mem? (lambda (x y)
                          (and (eq? (car x) (car y)) (eq? (cdr x) (cdr y))))
                        z
                        circle)
                  ;; Lose!  We've tried to get this one before.
                  (error "circular logical name definitions: ~S" circle))
                 (else
                  (let ((probe (logical-name fs ln)))
                    (cond ((null? probe)
                           ;; Let someone else resolve the logical name.
                           z)
                          ((pair? probe)
                           (resolve-logical-name (car probe)
                                                 (cdr probe)
                                                 (cons z circle)))
                          (else
                           (resolve-logical-name (car z)
                                                 probe
                                                 (cons z circle)))))))))))

(set (file-system-present?) '#t)

;;; Local file system


(define (make-local-fs)
  (let* ((os   (os-type (local-os)))
         (type (cond ((eq? os 'aegis) aegis-fs?)
                     ((eq? os 'unix)  unix-fs?)
                     ((eq? os 'vms)   vms-fs?)
                     (else
                      (error "unknown operating system ~a" os)))))
    (make-fs type '())))
  
(define-simple-switch local-fs 
                      file-system?)


