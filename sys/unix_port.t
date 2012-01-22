(herald unix_port
        (env tsys (osys vm_port) (osys buffer)))

;;; The Unix interface to the file system.


;;; Unix stat blocks

;++ make this a foreign structure someday
(define (make-stat-block)
  (make-bytev 64))

(lset *stat-block-pool* (make-pool 'stat-block-pool make-stat-block 1 bytev?))

(define-integrable (allocate-stat-block)
  (obtain-from-pool *stat-block-pool*))

(define-integrable (release-stat-block obj)
  (return-to-pool *stat-block-pool* obj))

(comment
;++ test this and decide if it is worth using?
(define-local-syntax (with-pathnames specs . body)
  (cond ((every? valid-spec? specs)
         `(let (,@(map (lambda (spec)
                         (cond ((atom? (cdr spec))
                                `(,(car spec)
                                   (get-string-buffer-of-size 128))
                               (unix-expand-path
                                (string->asciz! (filename->string
                                                (->filename ,(cdr spec))))
                                ,(car spec)))
                               (else
                                (syntax-error
                                 "illegal spec~%  ~S"
                                 `(with-stat-blocks ,specs . ,body)))))
                       specs))
            (unwind-protect
              (block ,@body)
              ,@(map (lambda (spec)
                       `(release-string-buffer ,(car spec)))
                     specs))))
        (syntax-error "illegal spec~%  ~S"
                      `(with-stat-blocks ,specs . ,body))))
)

(define-local-syntax (with-stat-blocks specs . body)
  (cond ((every? valid-spec? specs)
         `(let (,@(map (lambda (spec)
                         (cond ((atom? (cdr spec))
                                `(,(car spec) (allocate-stat-block)))
                               (else
                                (syntax-error
                                 "illegal spec~%  ~S"
                                 `(with-stat-blocks ,specs . ,body)))))
                       specs))
            (unwind-protect
              (block ,@body)
              ,@(map (lambda (spec)
                       `(release-stat-block ,(car spec)))
                     specs))))
        (syntax-error "illegal spec~%  ~S"
                      `(with-stat-blocks ,specs . ,body))))



(define (file-attributes filespec)
  (let* ((path (->pathname filespec))
         (stat-block (allocate-stat-block)))
    (cond ((fx> 0 (unix-stat path stat-block))  
           (release-string-buffer path)
           (return nil nil nil nil))
          (else
           (release-string-buffer path)
           (return t
                   (st_mtime stat-block)
                   (st_size  stat-block)
                   (st_mode  stat-block))))))

(define-foreign unix-stat (stat (in rep/string)
                                (in rep/extend))
                rep/integer)

(define (FILE-PROBE filespec)
  (with-open-ports ((iob (maybe-open filespec 'inquire)))
    (if iob (expand-filename iob) '#f)))

(define (expand-filename filespec)
  (with-open-ports ((iob (maybe-open filespec 'inquire)))
    (if iob (->filename (port-truename iob)) (->filename filespec))))

(define port-truename
  (lambda (iob)
    (let* ((buf (get-string-buffer-of-size 120))
           (filespec (iob-id iob))                        
           (str (string->asciz! 
                 (cond ((string?   filespec) filespec)
                       ((not (file-system-present?))
                        (error "Filespecs must be strings in VM."))
                       ((filename? filespec) (filename->string filespec))
                       (else
                        (filename->string (->filename filespec)))))))
      (set (string-length buf) 120)
      (unix-expand-path str buf)
      (set (string-length buf) (string-posq #\null buf))
      (let ((val (copy-string buf)))
        (release-string-buffer buf)
        val))))

;;; EXPAND-FILENAME (internal-routine) - move to fs_parse
;;; Returns an expanded Unix filename.

;;; If the file exists FILE-PROBE returns the truename of the
;;; file, otherwise, it returns false.

;(define (file-probe fname)
;  (let ((fname (->pathname fname)))
;    (with-stat-blocks ((stat-block))
;      (cond ((fx> 0 (unix-stat fname stat-block))
;             nil)
;            (else (->filename fname))))))

;+++ bogus version for the moment, doesn't handle networks.
(define (file-move from to)
  (let ((from (->pathname from))
        (to   (->pathname to)))
    (check-status (unix-file-link from to))
    (check-status (unix-file-unlink from))
    (release-string-buffer from)
    (release-string-buffer to)
    (no-value)))

(define-foreign unix-file-link (link (in rep/string)
                                         (in rep/string))
                rep/integer)

(define (file-delete fname)
  (let ((path (->pathname fname)))
    (if (fx> 0 (unix-file-unlink path))
        (local-os-error nil))
    (release-string-buffer path)
    (no-value)))


(define-foreign r-unix-unlink
  (unlink (in rep/string filename))
  rep/integer)

(define-integrable (unix-file-unlink filename)
  (r-unix-unlink (string->asciz! (copy-string filename))))

(define-unimplemented (FILE-TRUNCATE iob SIZE))

;;; In the next five calls SPEC can be either a filespec or an iob.
;;; If they cannot be implemented they should return nil.

(define (FILE-CREATION-DATE spec)
  (receive (dtc #f #f #f)
           (file-attributes spec)
    dtc))

;++ internal time??
(define (file-write-date spec)
  (receive (status dtu #f #f)
           (file-attributes spec)
    (if status dtu (file-write-date (error "File not found ~S" spec)))))

(define (FILE-USED-DATE SPEC)
  (receive (#f #f dtu #f)
           (file-attributes spec)
    dtu))


(define (file-newer? fname1 fname2)
  (> (file-write-date fname1) (file-write-date fname2)))

;++ is this useful? as is?
(define (file-length fname)
  (with-stat-blocks ((stat-block))
    (let ((path (->pathname fname)))
      (cond ((fx> 0 (unix-stat path stat-block))
             (local-os-error nil))
            (else
             (release-string-buffer path)
             (st_size stat-block))))))

(define (file-directory? fname)
  (with-stat-blocks ((stat-block))
    (let ((path (->pathname fname)))
      (cond ((fx> 0 (unix-stat path stat-block))
             (release-string-buffer path)
             nil)
          (else
           (release-string-buffer path)
           (fxN= 0 (fixnum-logand (st_mode stat-block) #o040000)))))))

;;; Working directory

(define working-directory
  (object (lambda ()
		  (let* ((buf (make-string 1024))
			 (val (unix-getwd buf)))
			(cond ((fx= 0 val)
			       (error "~a" buf))
			      (else
			       (set (string-length buf)
				    (string-posq #\null buf))
			       (->filename buf)))))
    ((setter self)
     (lambda (xpath)
       (let ((path (->pathname xpath)))
         (cond ((fx> 0 (unix-chdir path))
                (local-os-error nil))
               (else path)))))))


(define-foreign r-unix-chdir
  (chdir (in rep/string dirname))
  rep/integer)

(define-integrable (unix-chdir dirname)
  (r-unix-chdir (string->asciz! (copy-string dirname))))

(define-foreign unix-getwd (getwd (in rep/string))
                rep/integer)

(define (home-directory)
  (unix-getenv "HOME"))

(define-unimplemented (naming-directory))

(comment
 (define NAMING-DIRECTORY
  (let ((name (->pathname (copy-string "."))))
    (object (lambda () name)
      ((setter self)
       (lambda (filespec)
         (cond ((file-directory? filespec)
                (set name (->filename filespec))
                (no-value))
               (else
                (error "filespec ~s must be a directory" filespec))))))))
)
;++ Someday, the following ought to be settable.


(define-foreign r-unix-getenv
  (getenv (in rep/string name))
  rep/pointer)

(define (unix-getenv name)
  (let ((val (r-unix-getenv (string->asciz! name))))
       (if (fx= val 0)
	   '#f
	   (asciz->string val))))


;;; Returns the login name as a string.
(define (user-name)
  (let ((val (unix-getlogin)))
    (cond ((fx= val 0) nil)
          (else (asciz->string val)))))

(define-foreign unix-getlogin (getlogin)
                rep/pointer)
