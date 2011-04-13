(in-package :cl-cache-test)

(defun test-storage-via-api (storage)
  (format t "Running ~A test suite~%" (type-of storage))
  (let ((*default-storage* storage))
    ;; Basic tests

    (clrcache)

    (is (multiple-value-list (getcache "none"))
        '(nil nil)
        "Getting non-existed cache should return nil, nil")

    (is (setcache "foo" 1)
        1
        "Setting cache should return its value")

    (is (multiple-value-list (getcache "foo"))
        '(1 t)
        "Getting cache that has been set should return value, t")

    (is (setf (getcache "bar") 2)
        2
        "Setting cache by setf should also return its value")

    (is (multiple-value-list (getcache "bar"))
        '(2 t)
        "Getting cache that has been set by setf should also return value, t")

    (is (setcache "bar" 3)
        3
        "Overwriting cache should return its value")

    (is (multiple-value-list (getcache "bar"))
        '(3 t)
        "Getting cache that has been overwrote should return value, t")

    (is (remcache "none")
        nil
        "Removing non-existed cache should return nil")

    (is (remcache "foo")
        t
        "Removing existed cache should return t")

    (is (multiple-value-list (getcache "foo"))
        '(nil nil)
        "Getting removed cache should return nil, nil")

    (clrcache)

    (is (multiple-value-list (getcache "bar"))
        '(nil nil)
        "Getting cleared (removed) cache should return nil, nil")

    (is (with-cache ("foo") 1)
        1
        "with-cache with non-existed cache should evalute its body")

    (is (with-cache ("foo") (error "MUST NOT BE ERROR"))
        1
        "with-cache with existed cache shouldn't evalute its body")

    (is-error (with-cache ("none") (error "SHOULD BE ERROR"))
              error
              "with-cache with non-existed cache should evalute its body even if it reports an error")

    (is (multiple-value-list (getcache "none"))
        '(nil nil)
        "Getting non-existed cache that hasn't set by with-cache because of evaluation errors should return nil, nil")

    ;; Expiration tests

    (clrcache)

    (is (setcache "foo" 1 2)
        1
        "Setting cache with expiration time should return its value")

    (sleep 0.5)

    (is (multiple-value-list (getcache "foo"))
        '(1 t)
        "Getting existed cache that hasn't yet been expired should return value, t")

    (sleep 2)

    (is (multiple-value-list (getcache "foo"))
        '(nil nil)
        "Getting existed cache that has already been expired should return nil, nil")

    (is (setf (getcache "foo" 2) 1)
        1
        "Setting cache by setf with expiration time should return its value")

    (sleep 0.5)

    (is (multiple-value-list (getcache "foo"))
        '(1 t)
        "Getting existed cache that has been set by setf and hasn't yet been expired should return value, t")

    (sleep 2)

    (is (multiple-value-list (getcache "foo"))
        '(nil nil)
        "Getting existed cache that has been set by setf and has been expired should return nil, nil")

    (setcache "bar" 1)

    (is (setcache "bar" 2 2)
        2
        "Overwriting never-expire cache with expiration time should return its value")
    
    (sleep 0.5)

    (is (multiple-value-list (getcache "bar"))
        '(2 t)
        "Getting existed cache that has been overwrote with expiration time and hasn't yet been expired should return value, t")

    (sleep 2)
    
    (is (multiple-value-list (getcache "bar"))
        '(nil nil)
        "Getting existed cache that has been overwrote with expiration time and has been expired should return nil, nil")

    (setcache "bar" 2 2)

    (is (setcache "bar" 3)
        3
        "Overwriting will-expire cache with never-expiration time should return its value")

    (sleep 3)
    
    (is (multiple-value-list (getcache "bar"))
        '(3 t)
        "Getting existed cache that has been overwrote with never-expiration time and has been expired should return value, t")

    (is (with-cache ("foo" 2) 1)
        1
        "with-cache with non-existed cache and expiration-time should evalute its body")

    (sleep 0.5)

    (is (with-cache ("foo" 2) (error "MUST NOT BE ERROR"))
        1
        "with-cache with existed cache that hasn't yet been expired doesn't evalute its body should return value")

    (sleep 2)

    (is (with-cache ("foo" 2) 2)
        2
        "with-cache with existed cache that has been expired evalutes its body should return value")

    (is-error (with-cache ("none" 2) (error "SHOULD BE ERROR"))
              error
              "with-cache with non-existed cache and expiration-time should evalute its body even if it reports an error")

    ;; TODO key tests
    ;; TODO CLOS tests

    ;; Cleanup
    (clrcache)))

(defun test-all-storages-via-api ()
  (let ((cache-dir #p"/tmp/cl-cache-test/"))
    (ensure-directories-exist cache-dir)
    (dolist (storage (list (make-instance 'memory-storage)
                           (make-instance 'file-storage
                              :directory cache-dir)))
      (test-storage-via-api storage))))

(test-all-storages-via-api)
