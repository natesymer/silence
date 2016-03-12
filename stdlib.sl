; stdlib.sl
; written by Nathaniel Symer, 2.25.16
;
; Standard library for Silence.

; define functions
(let! 'func
  (lambda! (name args body) 
    (let-parent!
      (evaluate name) ; TODO: avoid extra evaluate calls
      (mk-lambda args body))))

; TODO: let form for binding in a given scope
; eg (let 'a "str" (print a))
; doesn't work: ; (func 'let (k v b) ((mk-lambda '(k) 'b) v))

(func 'newline () (print "\n"))
(func 'display (a) (begin (print (show a)) (newline)))
(func 'println (a) (begin (print a) (newline)))

; Logical operations

(func '! (a) (if a #f #t))
(func '&& (a b) (if a (if b #t #f) #f))
(func '|| (a b) (if a #t (if b #t #f)))

; numerical functions

; TODO: implement ^ based on the following haskell:
; (^/) :: Rational -> Rational -> Rational
; x ^/ (num :% den) = (root den x) ^ num
;   where root n number = toRational $ 10 ** (((log number') / log 10) / n') -- TODO: stop using (**)
;           where number' = fromRational number
;                 n' = fromInteger n
;(func 'nroot (n a) (^ a (/ (denominator n) (numerator n))))

; (func '^ (x n) ) ;; TODO: implement me!
(let! 'ln (log (exp 1)))


; Higher order functions

; map (not tail recursive!)
(func 'map (f xs)
  (if (null? xs)
    '()
    (cons
      (f (car xs))
      (map f (cdr xs)))))

(func 'filter (f xs)
  (if (null? xs)
    '()
    (if (f (car xs))
      (cons (car xs) (filter f (cdr xs)))
      (filter f (cdr xs)))))

(func 'foldr (f z xs)
  (if (null? xs)
    z
    (f (car xs) (foldr f z (cdr xs)))))

(func 'foldl (f z xs)
  (if (null? xs)
    z
    ((let 'zp (f z (car xs)))
      (foldl f zp (cdr xs)))))

; List operations

(func 'last (lst)
  (if (null? (cdr lst))
    (car lst)
    (last (cdr lst))))

; TODO: append, init, uncons

; ; (func 'uncons (lst cara cdra succ fail)
; ;   (if (null? lst)
; ;     fail
; ;     (let 'x (car lst)
; ;       (let 'xs (cdr lst)
; ;         succ))))
; ;
; ; ((uncons '(1 2 3 4) 'x 'xs)
; ;   '(begin
; ;     (display x)
; ;     (display xs))
; ;   '(print "null list!\n"))
; ; ;
; ; ; ((uncons '(1 2 3 4) 'x 'xs)
; ; ;   ((uncons xs '_ '_)
; ; ;     (print "at least two\n")
; ; ;     (print "singleton\n"))
; ; ;   (print "null\n"))