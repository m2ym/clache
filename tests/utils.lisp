(in-package :cl-cache-test)

(is (symbol-fqn 'foo)
    "CL-CACHE-TEST:FOO"
    "symbol-fqn")

(is (object-to-string 'foo)
    "CL-CACHE-TEST:FOO"
    "object-to-string for symbol")

(is (object-to-string 123)
    "123"
    "object-to-string for otherwise")

(is (md5-hex-string "foo")
    "acbd18db4cc2f85cedef654fccc4a4d8"
    "md5-hex-string")
