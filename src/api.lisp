(in-package :cl-cache)
(annot:enable-annot-syntax)

@export
(defparameter *default-storage* (make-instance 'memory-storage))

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
(annot:defannotation cache (args function-definition-form)
    (:arity 2)
  (annot.util:progn-form-replace-last
   (lambda (function-definition-form)
     (annot.util:replace-function-body
      (lambda (name lambda-list body)
        @ignore lambda-list
        (let* ((vars (loop for arg = (car args)
                           while (and arg
                                      (symbolp arg)
                                      (not (keywordp arg)))
                           collect (pop args)))
               (key `(list ',name ,@vars))
               (expire (getf args :expire))
               (storage (getf args :storage '*default-storage*)))
          `(with-cache (,key :expire ,expire :storage ,storage)
             ,@body)))
      function-definition-form))
   function-definition-form))
