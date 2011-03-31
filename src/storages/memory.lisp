(in-package :cl-cache)
(annot:enable-annot-syntax)

@export
(defclass memory-storage (storage)
     ((hash-table :initform (make-hash-table :test #'equal)
                  :reader hash-table-of)))

(defmethod load-cache (key (storage memory-storage))
  (let ((cell (gethash key (hash-table-of storage))))
    (if cell
        (let ((expire (car cell))
              (value (cdr cell)))
          (if (and (not (never-expire-p expire))
                   (< expire (get-universal-time)))
              (values nil nil)
              (values value t)))
        (values nil nil))))

(defmethod store-cache (key value expire (storage memory-storage))
  (setf (gethash key (hash-table-of storage))
        (cons expire value))
  value)

(defmethod delete-cache (key (storage memory-storage))
  (remhash key (hash-table-of storage)))

(defmethod clear-cache ((storage memory-storage))
  (clrhash (hash-table-of storage)))
