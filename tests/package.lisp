(defpackage clache-test
  (:use :cl
        :cl-test-more)
  (:import-from :clache
                :symbol-fqn
                :object-to-string
                :md5-hex-string
                :getcache
                :setcache
                :remcache
                :clrcache
                :with-cache
                :cache
                :memory-store
                :file-store))
