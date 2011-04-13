(defpackage cl-cache
  (:nicknames :cache)
  (:use :cl)
  (:import-from :alexandria
                :once-only
                :with-gensyms)
  (:import-from :annot
                :annotation)
  (:import-from :annot.util
                :replace-function-body)
  (:import-from :syntax
                :use-syntax)
  (:import-from :syntax-annot
                :annot-syntax))

