(in-package :clache-test)

(is (symbol-fqn 'foo)
    "CLACHE-TEST:FOO"
    "symbol-fqn")

(is (object-to-string 'foo)
    "CLACHE-TEST:FOO"
    "object-to-string for symbol")

(is (object-to-string 123)
    "123"
    "object-to-string for otherwise")

(is (md5-hex-string "foo")
    "acbd18db4cc2f85cedef654fccc4a4d8"
    "md5-hex-string")
