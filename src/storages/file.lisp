(in-package :cl-cache)
(annot:enable-annot-syntax)

@export
(defclass file-storage (storage)
     (@annot.slot:required
      (directory :reader directory-of)))

(defun cache-path (key storage)
  (merge-pathnames (md5-hex-string key)
                   (directory-of storage)))

(defmethod load-cache (key (storage file-storage))
  (let ((path (cache-path key storage)))
    (if (probe-file path)
        (let* ((cell (cl-store:restore path))
               (expire (car cell))
               (value (cdr cell)))
          (if (and (not (never-expire-p expire))
                   (< expire (get-universal-time)))
              (values nil nil)
              (values value t)))
        (values nil nil))))

(defmethod store-cache (key value expire (storage file-storage))
  (cl-store:store (cons (+ (get-universal-time) expire)
                        value)
                  (cache-path key storage)))

(defmethod delete-cache (key (storage file-storage))
  (let ((path (cache-path key storage)))
    (when (probe-file path)
      (delete-file path))))

(defmethod clear-cache ((storage file-storage))
  (dolist (file (fad:list-directory (directory-of storage)))
    (delete-file file)))
