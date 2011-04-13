(defpackage cl-cache-test
  (:use :cl
        :cl-test-more)
  (:import-from :cl-cache
                :symbol-fqn
                :object-to-string
                :md5-hex-string
                :getcache
                :setcache
                :remcache
                :clrcache
                :with-cache
                :cache
                :memory-storage
                :file-storage))
