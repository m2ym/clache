(in-package :cl-cache)
(annot:enable-annot-syntax)

@export
(defparameter *default-storage* nil)

@export
(defun getcache (key &optional (storage *default-storage*))
  (check-storage storage)
  (load-cache key storage))

@export
(defun setcache (key value &optional expire (storage *default-storage*))
  (check-storage storage)
  (check-expire expire)
  (store-cache key value expire storage))

@export
(defun (setf getcache) (value key &optional expire (storage *default-storage*))
  (check-storage storage)
  (check-expire expire)
  (store-cache key value expire storage))

@export
(defun remcache (key &optional (storage *default-storage*))
  (check-storage storage)
  (delete-cache key storage))

@export
(defun clrcache (&optional (storage *default-storage*))
  (check-storage storage)
  (clear-cache storage))

@export
(defmacro with-cache ((key &key expire (storage '*default-storage*))
                      &body body)
  (alexandria:once-only (key expire storage)
    (alexandria:with-gensyms (value exists-p)
      `(multiple-value-bind (,value ,exists-p)
           (getcache ,key ,storage)
         (if ,exists-p
             ,value
             (let ((,value (progn ,@body)))
               (setcache ,key ,value ,expire ,storage)
               ,value))))))

@export
(annot:defannotation cache ((key &key expire (storage '*default-storage*))
                            function-definition-form)
    (:arity 2)
  (annot.util:progn-form-replace-last
   (lambda (function-definition-form)
     (annot.util:replace-function-body
      (lambda (body)
        `(with-cache (,key :expire ,expire :storage ,storage)
           ,@body))
      function-definition-form))
   function-definition-form))
