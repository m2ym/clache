(in-package :cl-cache)
(use-syntax annot-syntax)

#|

API
---

### Keys

Any object can be a key if the object can be converted into a string
properly by using CACHE-KEY-TO-STRING.

### Values

As same as keys, any object can be a value. However, types that can be
used as values can be limited by storages.

### Expiration Time

Expiration time is a duration in seconds for keeping caches. If
expiration time is NIL, the cache will never expired.

### Storages

A storage is abstract data structure that can be accessed only via
cache protocol. Any storage must implement cache protocol.

|#

@export
(defparameter *default-storage* (make-instance 'memory-storage)
  "Default cache storage. This will be used by GETCACHE,
SETCACHE, REMCACHE, CLRCACHE, and WITH-CACHE. Setting this special
variable will effect on these functions and macros.")

@export
(defun getcache (key &optional (storage *default-storage*))
  "Try to retrieve a cache value indicated by KEY from STORAGE and
return values of the cache value and a boolean whether the cache
exists in STORAGE. The cache value will be NIL if such the cache
doesn't exist or has been expired. For example, (getcache
\"not-existed-cache\") will return NIL, NIL."
  (check-storage storage)
  (load-cache key storage))

@export
(defun setcache (key value &optional expire (storage *default-storage*))
  "Store a cache VALUE with KEY into STORAGE. EXPIRE is a keep time in
seconds. If EXPIRE is NIL, the cache will never expired. The return
value is VALUE that has been stored."
  (check-storage storage)
  (check-expire expire)
  (store-cache key value expire storage))

@export
(defun (setf getcache) (value key &optional expire (storage *default-storage*))
  "Same as SETCACHE. See SETCACHE docstring."
  (check-storage storage)
  (check-expire expire)
  (store-cache key value expire storage))

@export
(defun remcache (key &optional (storage *default-storage*))
  "Remove a cache indicated by KEY from STORAGE. If the cache has
been successfully removed, this function returns T, otherwise returns
NIL."
  (check-storage storage)
  (delete-cache key storage))

@export
(defun clrcache (&optional (storage *default-storage*))
  "Remove all caches from STORAGE. The return value is undefined."
  (check-storage storage)
  (clear-cache storage))

@export
(defmacro with-cache ((key &optional expire (storage '*default-storage*))
                      &body body)
  "If a cache indicated by KEY doesn't exist or has been expired, this
just returns the cache value without evaluating BODY. Otherwise, this
evaluates BODY and stores the evaluated value into STORAGE with KEY
and EXPIRE. KEY must be a form indicating a key of the cache.

Example:

    (defun f (x)
      (with-cache (x)
        (very-complex-computation x)))"
  (once-only (key expire storage)
    (with-gensyms (value exists-p)
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
  "Annotation for caching functions with their arguments. This should
be used with CL-ANNOT. KEYARGS is a parameter or a list of parameters
for making a cache key. See also WITH-CACHE docstring.

Example:

    @cache ((x y z))
    (defun f (x y z)
      ...)"
  (replace-function-body
   (lambda (name lambda-list body)
     @ignore lambda-list
     (let ((key `(list ',name ,@(if (listp keyargs)
                                     keyargs
                                     (list keyargs)))))
       `(with-cache (,key ,expire ,storage)
          ,@body)))
   function-definition-form))
