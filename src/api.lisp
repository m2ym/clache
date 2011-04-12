(in-package :cl-cache)
(use-syntax annot-syntax)

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
(defmacro with-cache ((key &optional expire (storage '*default-storage*))
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
@annotation (:arity 2)
(defmacro cache ((keyargs &optional expire (storage '*default-storage*))
                 function-definition-form)
  (replace-function-body
   (lambda (name lambda-list body)
     @ignore lambda-list
     (let ((key `(list ',name ,@(if (listp keyargs)
                                     keyargs
                                     (list keyargs)))))
       `(with-cache (,key ,expire ,storage)
          ,@body)))
   function-definition-form))
