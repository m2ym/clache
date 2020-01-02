(in-package :clache)
(use-syntax :annot)

#|

API
---

### Caches

A cache is a triple of a key, a value, and an expiration time.

### Cache Keys

Any object can be used as a cache key if the object can be converted
into a string properly by using CACHE-KEY-TO-STRING.

### Cache Values

Same as cache keys, any object can be used as a cache value. However,
a type of cache values can be limited by stores. So you have to be
careful what store are you using.

### Expiration Time

An expiration time describes how long caches live in seconds. If an
expiration time is NIL, such caches will never be expired: persistent
cache.

### Cache Existence

If a cache is stored in a store and has not yet been expired or a
persitent cache, we express the cache exists in the store.

### Stores

Store is an abstract layer of maintaining caches. You can access
stores via API.

|#

@export
(defun getcache (key store)
  "Retrieve a cache value from STORE indicated by KEY and return
values of the cache value and a boolean whether the cache exists in
STORE. The cache value will be NIL if such the cache doesn't
exist. For example, (getcache \"not-existed-cache\") will return NIL,
NIL."
  ;@type (store store)
  (load-cache key store))

@export
(defun setcache (key value store &key expire)
  "Store a cache VALUE into STORE with KEY and EXPIRE. EXPIRE is an
expiration time in seconds. If EXPIRE is NIL, the cache will never be
expired. The return value is VALUE that has been stored."
  ;@type (store store)
  ;@type (expire expire)
  (store-cache key value store expire))

@export
(defun (setf getcache) (value key store &key expire)
  ;@type (store store)
  ;@type (expire expire)
  (store-cache key value store expire))

@export
(defun remcache (key store)
  "Remove a cache from STORE indicated by KEY. If the cache has been
successfully removed, this function returns T, otherwise returns NIL."
  ;@type (store store)
  (delete-cache key store))

@export
(defun clrcache (store)
  "Remove all caches from STORE. The return value is undefined."
  ;@type (store store)
  (clear-cache store))

@export
(defmacro with-cache ((key &key store expire) &body body)
  "If a cache indicated by KEY exists, this just returns the cache
value without evaluating BODY. Otherwise, this evaluates BODY and
stores the evaluated value into STORE with KEY and EXPIRE. KEY is a
form that an evaluated value indicates the cache key.

Example:

    (defun f (x)
      (with-cache (x *store*)
        (very-complex-computation x)))"
  (once-only (key store expire)
    (with-gensyms (value exists-p)
      `(multiple-value-bind (,value ,exists-p)
           (getcache ,key ,store)
         (if ,exists-p
             ,value
             (let ((,value (progn ,@body)))
               (setcache ,key ,value ,store :expire ,expire)
               ,value))))))

@export
(defmacro with-inline-cache ((key &key expire (test 'equal) weakness) &body body)
  "Same as WITH-CACHE, except that an inline memory store will be used
as a cache store. TEST is a function to test hash table keys of the
memory store. WEAKNESS specifies the hash table is weak-hash-table or
not."
  (let* ((hash-table-form `(trivial-garbage:make-weak-hash-table :test (quote ,test) :weakness ,weakness))
         (store-form `(make-instance 'memory-store :hash-table ,hash-table-form)))
    `(with-cache (,key :store (load-time-value ,store-form) :expire ,expire)
       ,@body)))

@export
@annotation (:arity 2)
(defmacro cache ((keyargs &key store expire) function-definition-form)
  "Annotation for caching functions with their arguments. This should
be used with CL-ANNOT. KEYARGS is a form or a list of form for making
a cache key.  To make cache keys distinct as to the function, you may
add a keyword or a symbol into KEYARGS. See also WITH-CACHE.

Example:

    @cache ((:f x y z))
    (defun f (x y z)
      ...)
    
    ;; Remove a cache of F
    (remcache '(:f 1 2 3))"
  (replace-function-body
   (lambda (name lambda-list body)
     @ignore name
     @ignore lambda-list
     (let ((key (if (listp keyargs)
                    `(list ,@keyargs)
                    keyargs)))
       `(with-cache (,key :store ,store :expire ,expire)
          ,@body)))
   function-definition-form))
