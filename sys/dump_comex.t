(herald dump_comex
        (env tsys (osys table) (osys inf_vector) (osys dump)))

;;; This file is seperate because dump is not included in the VM;
;;; whereas, load is.

(define (comex-encoder obj)
  (if (comex? obj)
      (return 'comex '() (stype-selectors comex-stype))
      (return nil nil nil)))

;++ flush this
(define (write-comex-to-file spec comex)
  (with-open-ports ((port (open spec 'dump)))
    (set-encoder port comex-encoder)
    (write port comex)))

(define (write-comex port comex)
  (let ((things    (make-infinite-vector 60 false 'dump-vector))
        (status    (make-dump-status))
        (delimits  '())
        (encoder   (lambda (x) (ignore x) (return nil nil nil)))
        (objects   (make-table 'dumped-objects)))
     (set-encoder port comex-encoder)
     (push delimits (dump-status-count status))
     (vectorize comex things status objects encoder)
     (dump-port port things status objects (reverse! (cons -1 delimits)))
     (recycle things)
     (recycle objects)
     (no-value)))
