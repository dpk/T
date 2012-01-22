(herald aegis_vmport
  (env tsys
       (osys aegis_fault)
       (osys buffer)))

;;; The Aegis interface to I/O routines.

;;; This file contains the virtual machine I/O interface to the
;;; aegis operating system.  Where possible we use IOS calls rather
;;; than say NAME calls, so that T takes advantage of Extensible
;;; ports.

;;; Used on startup.

(define (%VM-BOOT-WRITE-TTY sym)
  (vfmt "%A%." sym (symbol-length sym)))

(define-foreign vfmt ("VFMT_$WRITE2" (in rep/string)
                                   (in rep/extend)
                                   (in rep/integer))
        ignore)

;;; Z system i/o

(define-set-of ios_$put_get_opts_t
               ios_$cond_opt
               ios_$preview_opt
               ios_$partial_record_opt
               ios_$no_rec_bndry_opt)

(define-constant %%standard-input  0)
(define-constant %%standard-output 1)
(define-constant %%error-input     2)
(define-constant %%error-output    3)

;;; Input

(define-constant aegis-eof     #x01270009)   ; ios_$end_of_file
(define-constant aegis-no-data #x0127003E)   ; ios_$get_conditional_failed

(define (%VM-READ-BUFFER IOB BLOCK?)
  (let ((get_opts (fx-ior (if block? ios_$cond_opt 0)
                          (if (iob-interactive? iob)
                              0
                              ios_$no_rec_bndry_opt))))
    (receive (length status) (ios_$get (iob-channel iob)
                                       get_opts
                                       (iob-buffer iob)
                                       (max-buffer-length iob)
                                       nil)
;++    (format t "~&sts= ~x len= ~d~%" status length)
      (cond ((fx= 0 status)
             (set (iob-offset iob) 0)
             (set (iob-limit iob) length)
             (set (iob-eof-flag? iob) nil)
             length)
            ((fx= aegis-no-data status)
             nil)
            ((fx=  aegis-eof status)
             (set (iob-eof-flag? iob) t)
             eof)
            (else
             (local-os-error status))))))

(define-foreign ios_$get
  ("IOS_$GET" (in     rep/integer-16-u  port-id)
            (in     rep/integer-16-u  put-get-options)
            (ignore rep/extend        buffer)
            (in     rep/integer       bufmax)
            (out    rep/integer       status))
    rep/integer)

;;; Block input

(DEFINE (%VM-READ-PARTIAL-BLOCK IOB LOC)
  (receive (length status)
           (ios_$get-partial (iob-channel iob)
                             (if (iob-interactive? iob)
                                 0
                                 ios_$no_rec_bndry_opt)
                             loc
                             (string-length loc)
                             nil)
;    (format t "~&partial: sts= ~x len= ~d~%" status length)
    (cond ((fx= 0 status)
           (set (iob-eof-flag? iob) nil)
           length)
          ((fx=  aegis-eof status)
           (set (iob-eof-flag? iob) t)
           eof)
          (else
           (local-os-error status)))))

(define-foreign ios_$get-partial
  ("IOS_$GET" (in     rep/integer-16-u  port-id)
            (in     rep/integer-16-u  put-get-options)
            (ignore rep/string        buffer)
            (in     rep/integer       bufmax)
            (out    rep/integer       status))
    rep/integer)


;;; Output

(define (%VM-WRITE-BUFFER iob)
  (cond ((fx> (iob-offset iob) 0)
         (let ((status (ios_$put (iob-channel iob)
                                 0
                                 (iob-buffer iob)
                                 (iob-offset iob)
                                 nil)))
           (cond ((fx= 0 status)
                  (set (iob-offset iob) 0))
                 (else
                  (local-os-error status))))))
  (no-value))

(define-foreign ios_$put
  ("IOS_$PUT" (in     rep/integer-16-u  port-id)
            (in     rep/integer-16-u  put-get-options)
            (ignore rep/extend        buffer)
            (in     rep/integer       buffer-size)
            (out    rep/integer       status))
    ignore)

;;; Newline is system dependent because of OS's like VMS.

(define-integrable (%vm-newline iob)
  (vm-write-char iob #\newline))

;;; Block output

(define (%VM-WRITE-BLOCK IOB LOC)
  (check-status (ios_$put-string (iob-channel iob)
                                 0
                                 loc
                                 (string-length loc)
                                 '#f))
  (no-value))

(define-foreign ios_$put-string
  ("IOS_$PUT" (in     rep/integer-16-u  port-id)
            (in     rep/integer-16-u  put-get-options)
            (ignore rep/string        buffer)
            (in     rep/integer       buffer-size)
            (out    rep/integer       status))
    ignore)

(define ios_$illegal_operation #x01270002)

(define (%VM-FORCE-OUTPUT IOB)
  (let ((status (ios_$force_write_file (iob-channel iob) nil)))
    (cond ((or (fx= status 0) (fx= status ios_$illegal_operation))
           (no-value))
          (else
           (local-os-error status)))))

(define-foreign ios_$force_write_file
  ("IOS_$FORCE_WRITE_FILE" (in  rep/integer-16-u port-id)
                         (out rep/integer      status))
                ignore)

;;; The rest of this file doesn't have to be implemented to get
;;; the Z System booted.

;;; Pathnames

;;; Pathnames are system dependent objects used for naming files
;;; internally.  They should not be accessible outside of this file.

;;; FILESPEC    -   Something that ->FILENAME can handle
;;; FILENAME    -   The result of  ->FILENAME
;;; PATHNAME    -   A string in the LOCAL-OS format
;;; (->PATHNAME filespec) => pathname

(define-constant name_$pnamelen_max 256)

(define-constant pathname? string?)

(define-constant pathname-length string-length)

;;; ->PATHNAME provides the VM with a portable interface to the
;;; various file systems.

(define (->pathname filespec)
  (cond ((pathname? filespec) filespec)
        ((not (file-system-present?))
         (vm-error "Filespecs must be strings in VM."))
        ((filename? filespec) (filename->string filespec))
        (else
         (filename->string (->filename filespec)))))

;;; Random Aegis definitions

(define-set-of ios_$open_options_t
               ios_$no_open_delay_opt
               ios_$write_opt
               ios_$unregulated_opt
               ios_$position_to_eof_opt
               ios_$inquire_only_opt
               )

(define-enumerated ios_$create_mode_t
                   ios_$no_pre_exist_mode
                   ios_$preserve_mode
                   ios_$recreate_mode
                   ios_$truncate_mode
                   ios_$make_backup_mode
                   ios_$loc_name_only_mode
                   )

;;; Aegis UID's

(define-constant (MAKE-UID_$T)
  (make-bytev 8))

(define-constant UID_$NIL (make-uid_$t))

;;; File opening and closing.
;++ What about pads, sockets, etc.
;++ There should be a population of all open IOB's.

;++ What should the buffer size be?

(define default-buffer-size 1024)

(define (%vm-open-file caller fd modespec size)
  (let* ((path (->pathname (if (iob? fd) (iob-id fd) fd)))
         (len  (pathname-length path))
         (mode (mode->iob-mode caller fd modespec))
         (size (if (iob-mode? mode iob/inquire) 0 size)))
    (receive (port-id status)
             (cond ((iob-mode? mode iob/read)
                    (ios_$open path len 0 nil))
                   ((iob-mode? mode iob/write)
                    (ios_$create path
                                 len
                                 uid_$nil
                                 ios_$truncate_mode
                                 ios_$write_opt
                                 nil
                                 nil))
                   ((iob-mode? mode iob/inquire)
                    (ios_$open path len ios_$inquire_only_opt nil))
                   ((iob-mode? mode iob/append)
                    (ios_$create path
                                 len
                                 uid_$nil
                                 ios_$preserve_mode
                                 (fx-xor ios_$write_opt
                                         ios_$position_to_eof_opt)
                                 nil
                                 nil))
                   (else
                    (unsupported-mode-error caller fd modespec)))
      (cond ((fxN= 0 status) nil)
            (else
             (let ((iob (get-i/o-buffer %buffer-pool fd port-id mode size)))
          ;++     (set (table-entry open-port-table iob) (object-hash iob))
               iob))))))

(define-foreign ios_$create
  ("IOS_$CREATE" (in  rep/string       name)
               (in  rep/integer-16-u namelength)
               (in  rep/extend       type-uid)
               (in  rep/integer-16-u create-mode)
               (in  rep/integer-16-u open-options)
               (out rep/integer-16-u port-id)
               (out rep/integer      status))
    ignore)

(define-foreign ios_$open
  ("IOS_$OPEN" (in  rep/string       name)
             (in  rep/integer-16-u namelength)
             (in  rep/integer-16-u open-options)
             (out rep/integer      status))
    rep/integer)

(define (%vm-close-file iob)
  (check-status (ios_$close (iob-channel iob) nil)))

(define-foreign ios_$close
  ("IOS_$CLOSE" (in  rep/integer-16-u port-id)
              (out rep/integer      status))
    ignore)

;;; File mapping

(define-enumerated ms_$conc_mode_t
                   ms_$nr_xor_1w
                   ms_$cowriters)

(define-enumerated ms_$acc_mode_t
                   ms_$r
                   ms_$rx
                   ms_$wr
                   ms_$wrx
                   ms_$riw)

; do area's need UID's

(define (%VM-MAP-AREA FILESPEC SIZE)
  (let* ((path (->pathname filespec))
         (len  (pathname-length path))
         (size (fx* size 4)))
    (if (not (file-exists? path))
        (close-port (open-port path 'out)))
    (receive (begin length status)
             (ms_$mapl path len 0 size ms_$nr_xor_1w ms_$wr -1 nil nil)
      (ignore length)
      (check-status status)
      (create-area filespec begin size nil))))

(define-foreign ms_$mapl
  ("MS_$MAPL" (in  rep/string       name)
            (in  rep/integer-16-u namelength)
            (in  rep/integer      start)
            (in  rep/integer      desired-length)
            (in  rep/integer-16-u concurency)
            (in  rep/integer-16-u access)
            (in  rep/integer-8-s  extend)
            (out rep/integer      length)
            (out rep/integer      status))
  rep/address)

(define (%VM-MAKE-AREA FILESPEC SIZE)
  (let* ((path (->pathname filespec))
         (len  (pathname-length path))
         (size (fx* size 4)))
    (receive (begin status)
             (ms_$crmapl path len 0 size ms_$nr_xor_1w nil)
      (check-status status)
      (create-area filespec begin size nil))))

(define-foreign ms_$crmapl
  ("MS_$CRMAPL" (in  rep/string       name)
              (in  rep/integer-16-u namelength)
              (in  rep/integer      start)
              (in  rep/integer      desired-length)
              (in  rep/integer-16-u concurency)
              (out rep/integer      status))
  rep/address)

(define (%VM-UNMAP-AREA AREA)
  (check-status (ms_$unmap (area-begin area) (area-size area) nil))
  (no-value))

(define-foreign ms_$unmap
  ("MS_$UNMAP" (in  rep/integer address)
             (in  rep/integer length)
             (out rep/integer status))
  ignore)

;(define-unimplemented (VM-REMAP-AREA AREA))


;;; Foreign Procedures - see foreign.doc.

;;; The syntax to define a foreign procedure is:
;;;
;;; (define-foreign name (aegis_name (IN     rep0 name)
;;;                                  (IN/OUT rep1 name)
;;;                                  (OUT    rep2 name)
;;;                                  ...)
;;;                return-rep)
;;;
;;; We return multiple values starting with the returned value and
;;; then the out paramaters in order, ignoring the return value
;;; if indicated.

(define-constant kg_$name_string 32)
               
(define (make-foreign-procedure symbol)
  (let* ((s (symbol->string symbol))
         (pad-length (fx- kg_$name_string
                          (fixnum-min (string-length s)
                                      kg_$name_string)))
         (xeno (make-foreign symbol))
         (addr (kg_$lookup (string-append
                            s
                            (string-fill (make-string pad-length)
                                         #\space)))))
    (cond ((fxn= addr 0)
           (set (mref-integer xeno 4) addr)
           xeno)
          (else
           (error "KG-lookup on foreign procedure ~s failed" symbol)))))

;++ rep/address should be made to work correctly (cons a xenoid).

(define-foreign kg_$lookup 
    ("KG_$LOOKUP" (in rep/string))
    rep/address)


;;; Load an Aegis binary file.


;;; From /us/ins/loader.ins.pas .

(define-foreign pm_$load
  ("PM_$LOAD"  (in rep/string)         ;;; In Name: Univ Name_$Pname_T
             (in rep/integer-16-u)   ;;; In Len:  Integer;
             (in rep/integer-16-u)   ;;; In Opts: PM_$Loader_Opts;
             (in rep/integer-16-u)   ;;; In  N_Sects:  Integer;
             (ignore rep/extend)     ;;; Out Info:     Univ PM_$Load_Info;
             (out rep/integer))      ;;; Out Status: Status_T
  ignore)

(define-set-of pm_$opts
               pm_$copy_proc
               pm_$install
               pm_$no_unresolveds
               pm_$load_global
               pm_$install_sections)

(define (load-foreign file)
  (let ((out (standard-output) ))
    (bind ((*load-level* (fx+ *load-level* 1)))
      (comment-indent out (fx* *load-level* 2))
      (format out "Loading foreign ~s~%" file)
      (let* ((fname (->filename file))
             (type  (filename-type fname))
             (file  (->pathname (if (null? type)
                                    (filename-with-type fname 'bin)
                                    fname)))
             (status (pm_$load file
                               (string-length file)
                               pm_$install
                               0
                               (make-bytev 8)    ; 4+4+0*(32+4+4) = 8
                               0)))
        (if (fxN= 0 status)
            (error "Couldn't load ~S~%**~10t~a"
                   file
                   (local-os-error-message status)))))))
