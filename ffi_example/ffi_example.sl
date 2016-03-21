#!../dist/build/silence/silence -f

(import "../stdlib.sl")

(let! 'example-dylib (dlopen "example.dylib"))

(func 'gettype (a) ((foreign example-dylib "showtype") a))
(func 'print-number (a) ((foreign example-dylib "print_number") a))
(func 'strlen (a) ((foreign example-dylib "get_strlen") a))
(func 'snoc (lst v) ((foreign example-dylib "snoc_test") lst v))

(print "Testing FFI...\n")
(println (show (gettype 1)))
(println (show (gettype 'atomsarecool)))
(print-number (/ 2 5))
(println (show (strlen "thisss")))
(println (show (snoc '(1) 'asdf)))

(free example-dylib)