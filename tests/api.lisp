(in-package :clache-test)

(defun test-storage-via-api (storage)
  (format t "Running ~A test suite~%" (type-of storage))
  (let ((*default-storage* storage))
    ;; Basic tests

    (clrcache storage)

    (is (multiple-value-list (getcache "none" storage))
        '(nil nil)
        "Getting non-existed cache should return nil, nil")

    (is (setcache "foo" 1 storage)
        1
        "Setting cache should return its value")

    (is (multiple-value-list (getcache "foo" storage))
        '(1 t)
        "Getting cache that has been set should return value, t")

    (is (setf (getcache "bar" storage) 2)
        2
        "Setting cache by setf should also return its value")

    (is (multiple-value-list (getcache "bar" storage))
        '(2 t)
        "Getting cache that has been set by setf should also return value, t")

    (is (setcache "bar" 3 storage)
        3
        "Overwriting cache should return its value")

    (is (multiple-value-list (getcache "bar" storage))
        '(3 t)
        "Getting cache that has been overwrote should return value, t")

    (is (remcache "none" storage)
        nil
        "Removing non-existed cache should return nil")

    (is (remcache "foo" storage)
        t
        "Removing existed cache should return t")

    (is (multiple-value-list (getcache "foo" storage))
        '(nil nil)
        "Getting removed cache should return nil, nil")

    (clrcache storage)

    (is (multiple-value-list (getcache "bar" storage))
        '(nil nil)
        "Getting cleared (removed) cache should return nil, nil")

    (is (with-cache ("foo" :store storage) 1)
        1
        "with-cache with non-existed cache should evalute its body")

    (is (with-cache ("foo" :store storage) (error "MUST NOT BE ERROR"))
        1
        "with-cache with existed cache shouldn't evalute its body")

    (is-error (with-cache ("none" :store storage) (error "SHOULD BE ERROR"))
              error
              "with-cache with non-existed cache should evalute its body even if it reports an error")

    (is (multiple-value-list (getcache "none" storage))
        '(nil nil)
        "Getting non-existed cache that hasn't set by with-cache because of evaluation errors should return nil, nil")

    ;; Expiration tests

    (clrcache storage)

    (is (setcache "foo" 1 storage :expire 2)
        1
        "Setting cache with expiration time should return its value")

    (sleep 0.5)

    (is (multiple-value-list (getcache "foo" storage))
        '(1 t)
        "Getting existed cache that hasn't yet been expired should return value, t")

    (sleep 2)

    (is (multiple-value-list (getcache "foo" storage))
        '(nil nil)
        "Getting existed cache that has already been expired should return nil, nil")

    (is (setf (getcache "foo" storage) 1)
        1
        "Setting cache by setf with expiration time should return its value")

    (sleep 0.5)

    (is (multiple-value-list (getcache "foo" storage))
        '(1 t)
        "Getting existed cache that has been set by setf and hasn't yet been expired should return value, t")

    (sleep 2)

    (is (multiple-value-list (getcache "foo" storage))
        '(nil nil)
        "Getting existed cache that has been set by setf and has been expired should return nil, nil")

    (setcache "bar" 1 storage)

    (is (setcache "bar" 2 storage :expire 2)
        2
        "Overwriting never-expire cache with expiration time should return its value")
    
    (sleep 0.5)

    (is (multiple-value-list (getcache "bar" storage))
        '(2 t)
        "Getting existed cache that has been overwrote with expiration time and hasn't yet been expired should return value, t")

    (sleep 2)
    
    (is (multiple-value-list (getcache "bar" storage))
        '(nil nil)
        "Getting existed cache that has been overwrote with expiration time and has been expired should return nil, nil")

    (setcache "bar" 2 storage :expire 2)

    (is (setcache "bar" 3 storage)
        3
        "Overwriting will-expire cache with never-expiration time should return its value")

    (sleep 3)
    
    (is (multiple-value-list (getcache "bar" storage))
        '(3 t)
        "Getting existed cache that has been overwrote with never-expiration time and has been expired should return value, t")

    (is (with-cache ("foo" :expire 2 :store storage) 1)
        1
        "with-cache with non-existed cache and expiration-time should evalute its body")

    (sleep 0.5)

    (is (with-cache ("foo" :expire 2 :store storage) (error "MUST NOT BE ERROR"))
        1
        "with-cache with existed cache that hasn't yet been expired doesn't evalute its body should return value")

    (sleep 2)

    (is (with-cache ("foo" :expire 2 :store storage) 2)
        2
        "with-cache with existed cache that has been expired evalutes its body should return value")

    (is-error (with-cache ("none" :expire 2 :store storage) (error "SHOULD BE ERROR"))
              error
              "with-cache with non-existed cache and expiration-time should evalute its body even if it reports an error")

    ;; TODO key tests
    ;; TODO CLOS tests

    ;; Cleanup
    (clrcache storage)))

(defun test-all-storages-via-api ()
  (let ((cache-dir #p"/tmp/clache-test/"))
    (ensure-directories-exist cache-dir)
    (dolist (storage (list (make-instance 'memory-store)
                           (make-instance 'file-store
                              :directory cache-dir)))
      (test-storage-via-api storage))))

(test-all-storages-via-api)
