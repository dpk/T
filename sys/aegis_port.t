(herald aegis_port
        (env tsys (osys vm_port) (osys buffer)))

;;; The Aegis interface to the file system.

;;; Where possible we use IOS calls rather than say NAME calls,
;;; so that T takes advantage of Extensible ports.
;;; Random file routines

;;; Returns true if the file exists, otherwise false.

;;; If the file exists VM-FILE-PROBE returns the truename of the
;;; file, otherwise, it returns false.

(define (FILE-PROBE filespec)
  (with-open-ports ((iob (maybe-open filespec 'inquire)))
    (if iob (expand-filename iob) '#f)))

(define (expand-filename filespec)
  (with-open-ports ((iob (maybe-open filespec 'inquire)))
    (if iob (->filename (port-truename iob)) (->filename filespec))))

(define port-truename
  (lambda (iob)
    (let ((out-buf (get-string-buffer-of-size name_$pnamelen_max)))
      (receive (out-len status)
               (ios_$inq_name (iob-channel iob) out-buf nil nil)
        (check-status status)
        (set (string-length out-buf) out-len)
        (let ((val (copy-string out-buf)))
          (release-string-buffer out-buf)
          val)))))

(define-foreign ios_$inq_name
  ("IOS_$INQ_NAME" (in     rep/integer-16-u port-id)
                 (ignore rep/string       out-name)
                 (out    rep/integer-16-u out-len)
                 (out    rep/integer      status))
                ignore)

(define (FILE-MOVE FROM TO)
  (let* ((from   (open-port from 'inquire))
         (to     (->pathname to))
         (to-len (pathname-length to)))
    (check-status (ios_$change_name from to to-len nil))
    (close-port from)
    (release-buffer from)
    (no-value)))

(define-foreign ios_$change_name
  ("IOS_$CHANGE_NAME" (in  rep/integer-16-u port-id)
                    (in  rep/string       new-name)
                    (in  rep/integer-16-u new-namelength)
                    (out rep/integer      status))
                ignore)

(define (FILE-DELETE FILESPEC)
  (let ((iob (open-port filespec 'inquire)))
    (ios_$delete (iob-channel iob) nil)
    (close-port iob)
    (release-buffer iob))
  (no-value))

(define-foreign ios_$delete
  ("IOS_$DELETE" (in  rep/integer-16-u port-id)
               (out rep/integer      status))
                ignore)

(define (FILE-TRUNCATE iob SIZE)
  (%vm-write-buffer iob)
  ;++ seek to size position
  (check-status (ios_$truncate (iob-channel iob) nil))
  (no-value))

(define-foreign ios_$truncate
  ("IOS_$TRUNCATE" (in  rep/integer-16-u port-id)
                 (out rep/integer      status))
                ignore)

;;; In the next five calls SPEC can be either a filespec or an iob.
;;; If they cannot be implemented they should return nil.

(define (FILE-CREATION-DATE spec)
  (receive (dtc #f #f #f)
           (file-attributes spec)
    dtc))

(define (FILE-WRITE-DATE SPEC)
  (receive (#f dtm #f #f)
           (file-attributes spec)
    dtm))

(define (FILE-USED-DATE SPEC)
  (receive (#f #f dtu #f)
           (file-attributes spec)
    dtu))

(define (FILE-NEWER? SPEC1 SPEC2)
  (fx> (file-write-date spec1)
       (file-write-date spec2)))

;++ is this useful? as is?
(define (FILE-LENGTH SPEC)
    (receive (#f #f #f blocks)
             (file-attributes spec)
      (fx* 1024 blocks)))

(define (file-attributes spec)
  (let ((iob (if (iob? spec) spec (open-port spec 'inquire))))
    (receive (dtc dtm dtu blocks status) (iob-attributes iob)
      (check-status status)
      (cond ((not (iob? spec))
             (close-port iob)
             (release-buffer iob)))
      (return dtc dtm dtu blocks))))

(define (iob-attributes iob)
  (receive (dtc dtm dtu blocks status)
           (ios_$inq_file_attr (iob-channel iob) nil nil nil nil nil)
    (check-status status)
    (return dtc dtm dtu blocks status)))

(define-foreign ios_$inq_file_attr
  ("IOS_$INQ_FILE_ATTR" (in   rep/integer-16-u port-id)
                      (out  rep/integer      dt-created)
                      (out  rep/integer      dt-modified)
                      (out  rep/integer      dt-used)
                      (out  rep/integer      blocks)
                      (out  rep/integer      status))
    ignore)

(define-unimplemented (FILE-DIRECTORY? FILESPEC))

(define-foreign ios_$inq_type_uid
  ("IOS_$INQ_TYPE_UID" (in     rep/integer-16-u port-id)
                     (ignore rep/extend       type-uid)
                     (out    rep/integer      status))
    ignore)


;;; Working directory

(define WORKING-DIRECTORY
  (let ((buf (make-string name_$pnamelen_max)))
    (object (lambda ()
              (defer-interrupts
               (receive (length status)
                        (name_$get_wdir buf nil nil)
                 (cond ((fxN= 0 status) '#f)
                       (else
                        (set (string-length buf) length)
                        (->filename buf))))))
      ((setter self)
       (lambda (filespec)
         (let* ((path (->pathname filespec))
                (len  (pathname-length path)))
           (check-status (name_$set_wdir path len nil))
           (no-value)))))))

;++ change these to ios calls
(define-foreign name_$get_wdir
  ("NAME_$GET_WDIR" (ignore rep/string       name)
                  (out    rep/integer-16-u name-length)
                  (out    rep/integer      status))
                ignore)

(define-foreign name_$set_wdir
  ("NAME_$SET_WDIR" (in  rep/string       name)
                  (in  rep/integer-16-u name-length)
                  (out rep/integer      status))
                ignore)

(define NAMING-DIRECTORY
  (let ((buf (make-string name_$pnamelen_max)))
    (object (lambda ()
              (defer-interrupts
               (receive (length status)
                        (name_$get_ndir buf nil nil)
                (cond ((fxN= 0 status) '#f)
                      (else
                       (set (string-length buf) length)
                       (->filename buf))))))
      ((setter self)
       (lambda (filespec)
         (let* ((path (->pathname filespec))
                (len   (pathname-length path)))
           (check-status (name_$set_ndir path len nil))
           (no-value)))))))

;++ change these to ios calls
(define-foreign name_$get_ndir
  ("NAME_$GET_NDIR" (ignore rep/string       name)
                  (out    rep/integer-16-u name-length)
                  (out    rep/integer      status))
                ignore)

(define-foreign name_$set_ndir
  ("NAME_$SET_NDIR" (in  rep/string       name)
                  (in  rep/integer-16-u name-length)
                  (out rep/integer      status))
                ignore)

(define-unimplemented (home-directory))
