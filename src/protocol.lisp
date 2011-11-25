(in-package :clache)
(use-syntax :annot)

#|

Protocol
--------

TODO

|#

@export
(defclass store ()
  ()
  (:documentation "An abstract class of stores. All stores must
inherit from this class."))

(deftype expire ()
  "Type for expiration time."
  '(or null integer))

@export
(defgeneric load-cache (key store)
  (:documentation "Try to retrieve a cache indicated by KEY from STORE
and return values of the cache value and a boolean whether the cache
exists in STORE. The cache value should be NIL if such the cache
doesn't exist or has been expired."))

@export
(defgeneric store-cache (key value store expire)
  (:documentation "Store a cache VALUE with KEY into STORE. EXPIRE is
a keep time in seconds. If EXPIRE is NIL, the cache will never
expired. This function should return the value that has been
stored."))

@export
(defgeneric delete-cache (key store)
  (:documentation "Remove a cache indicated by KEY from STORE. If the
cache has been successfully removed, this function should return T,
otherwise should return NIL."))

@export
(defgeneric clear-cache (store)
  (:documentation "Remove all caches from STORE. Any object can be
returned."))

@export
(defgeneric cache-key-to-string (key)
  (:documentation "This function converts any type of KEY into
string. This should be an injective function, meaning this should not
lose the information about key.")
  (:method (key) (object-to-string key)))

@export
(defun never-expire-p (expire)
  "Return T if EXPIRE represents caches will never be expired."
  (eq expire nil))
