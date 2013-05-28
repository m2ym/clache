(in-package :clache)
(use-syntax :annot)

#|

File Store
----------

TODO

|#

@export
(defclass memory-store (store)
  ((hash-table :initform (make-hash-table :test #'equal)
               :reader hash-table-of)))

(defmethod load-cache (key (store memory-store))
  (let ((cell (gethash key (hash-table-of store))))
    (if cell
        (let ((expire (car cell))
              (value (cdr cell)))
          (if (and (not (never-expire-p expire))
                   (<= expire (get-universal-time)))
              (values nil nil)
              (values value t)))
        (values nil nil))))

(defmethod store-cache (key value (store memory-store) expire)
  (when expire
    (setf expire (+ (get-universal-time) expire)))
  (setf (gethash key (hash-table-of store))
        (cons expire value))
  value)

(defmethod delete-cache (key (store memory-store))
  (remhash key (hash-table-of store)))

(defmethod clear-cache ((store memory-store))
  (clrhash (hash-table-of store)))
