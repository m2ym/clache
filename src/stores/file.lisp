(in-package :clache)
(use-syntax :annot)

#|

File Store
----------

TODO

|#

@export
(defclass file-store (store)
     (@annot.slot:required
      (directory :reader directory-of)))

(defun cache-path (key store)
  (merge-pathnames (md5-hex-string (cache-key-to-string key))
                   (directory-of store)))

(defmethod load-cache (key (store file-store))
  (let ((path (cache-path key store)))
    (if (probe-file path)
        (let* ((cell (cl-store:restore path))
               (expire (car cell))
               (value (cdr cell)))
          (if (and (not (never-expire-p expire))
                   (<= expire (get-universal-time)))
              (values nil nil)
              (values value t)))
        (values nil nil))))

(defmethod store-cache (key value (store file-store) expire)
  (when expire
    (setf expire (+ (get-universal-time) expire)))
  (cl-store:store (cons expire value)
                  (cache-path key store))
  value)

(defmethod delete-cache (key (store file-store))
  (let ((path (cache-path key store)))
    (when (probe-file path)
      (delete-file path))))

(defmethod clear-cache ((store file-store))
  (dolist (file (fad:list-directory (directory-of store)))
    (delete-file file)))
