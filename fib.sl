; fibonacci sequence implementation
(define 'fib (n)
  (if (= n 0) 0
    (if (= n 1) 1
      (+ (fib (- n 1)) (fib (- n 2))))))
(display (fib 10))