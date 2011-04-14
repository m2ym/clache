(in-package :cl-cache)
(use-syntax annot-syntax)

#|

API
---

Glossary
--------

### Caches

A cache is a triple of a key, a value, and an expiration time.

### Cache Keys

Any object can be used as a cache key if the object can be converted
into a string properly by using CACHE-KEY-TO-STRING.

### Cache Values

Same as cache keys, any object can be used as a cache value. However,
a type of cache values can be limited by storages. So you have to be
careful what storage are you using.

### Expiration Time

An expiration time describes how long caches live in seconds. If an
expiration time is NIL, such caches will never be expired: persistent
cache.

### Cache Existence

If a cache is stored in a storage and has not yet been expired or a
persitent cache, we express the cache exists in the storage.

### Storages

Storage is an abstract layer of maintaining caches. You can access
storages via API.

### Default Storage

|#

@export
(defparameter *default-storage* (make-instance 'memory-storage)
  "A default cache storage. This will be used by GETCACHE,
SETCACHE, REMCACHE, CLRCACHE, and WITH-CACHE. Setting this special
variable will effect on them.")

@export
(defun getcache (key &optional (storage *default-storage*))
  "Retrieve a cache value from STORAGE indicated by KEY and return
values of the cache value and a boolean whether the cache exists in
STORAGE. The cache value will be NIL if such the cache doesn't
exist. For example, (getcache \"not-existed-cache\") will return NIL,
NIL."
  (check-storage storage)
  (load-cache key storage))

@export
(defun setcache (key value &optional expire (storage *default-storage*))
  "Store a cache VALUE into STORAGE with KEY and EXPIRE. EXPIRE is an
expiration time in seconds. If EXPIRE is NIL, the cache will never be
expired. The return value is VALUE that has been stored."
  (check-storage storage)
  (check-expire expire)
  (store-cache key value expire storage))

@export
(defun (setf getcache) (value key &optional expire (storage *default-storage*))
  "Same as SETCACHE."
  (check-storage storage)
  (check-expire expire)
  (store-cache key value expire storage))

@export
(defun remcache (key &optional (storage *default-storage*))
  "Remove a cache from STORAGE indicated by KEY. If the cache has been
successfully removed, this function returns T, otherwise returns NIL."
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
  "If a cache indicated by KEY exists, this just returns the cache
value without evaluating BODY. Otherwise, this evaluates BODY and
stores the evaluated value into STORAGE with KEY and EXPIRE. KEY is a
form that an evaluated value indicates the cache key.

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
for making a cache key. See also WITH-CACHE.

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
