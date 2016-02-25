; fibonacci sequence implementation
(func 'fib (n)
  (if (= n 0) 0
    (if (= n 1) 1
      (+ (fib (- n 1)) (fib (- n 2))))))
(print (to-str (fib 10)))
(print "\n")