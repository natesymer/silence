#!../dist/build/silence/silence -f

(import "../stdlib.sl")

(let! 'example-dylib (dlopen "example.dylib"))

(func 'gettype (a) ((foreign example-dylib "showtype") a))
(func 'print-number (a) ((foreign example-dylib "print_number") a))
(func 'strlen (a) ((foreign example-dylib "get_strlen") a))
(func 'test-from-str () ((foreign example-dylib "from_str_test")))

;; foreign import a snoc procedure from C-land
(func 'snoc (lst v) ((foreign example-dylib "snoc_test") lst v))

;; foreign import and call a snoc procedure that
;; returns a snoc procedure from C-land
(let! 'snoc2 ((foreign example-dylib "make_c_procedure")))

(print "Testing FFI...\n")
(println (show (gettype 1)))
(println (show (gettype 'atomsarecool)))
(print-number (/ 2 5))
(println (show (strlen "thisss")))
(println (show (snoc2 '(1) 'asdf)))
(println (show (snoc '(1) 'asdf)))
(println (show (snoc '() 2)))
(println (test-from-str))

(free example-dylib)