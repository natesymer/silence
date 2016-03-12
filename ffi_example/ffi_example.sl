#!../dist/build/silence/silence -f

(import "../stdlib.sl")

(func 'testffi (a) ((foreign "example.dylib" 'testFFI) a))
  
(print "Testing FFI...\n")
(display (testffi 1))