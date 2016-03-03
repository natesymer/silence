; stdlib.sl
; written by Nathaniel Symer, 2.25.16
;
; Standard library for Silence.

;(let! 'evaluate (lambda (a) a))

; define functions
(let! 'func
  (lambda! (name args body) 
    (let-parent!
      (evaluate name) ;; name isn't evaluated for some reason (use haskell @const $ return . head@)
      (mk-lambda args body))))

; due to the way the semantics work,
; arguments of procedures are evaluated
; unless the evaluation flag isn't set.
; This implementation of @func@ uses
; the form @lamba@ which sets the evaluation
; flag to @#t@. The argument, @a@ is
; therefore evaluated.
(func 'evaluate (a) a)

(func! 'let (k v))

; display a value. Similar to Haskell's @show@.
(func 'display (a) (begin (print (to-str a)) (print "\n") ))

; negate a "bool". The semantics of @if@ are
; that any non-false value is true.
(func 'not (a) (if a #f #t))

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
      
; list operations
(func 'last (lst)
  (if (null? (cdr lst))
    (car lst)
    (last (cdr lst))))

; TODO: append, init, uncons

; (func 'uncons (lst cara cdra succ fail)
;   (if (null? lst)
;     fail
;     (let 'x (car lst)
;       (let 'xs (cdr lst)
;         succ))))
;
; ((uncons '(1 2 3 4) 'x 'xs)
;   '(begin
;     (display x)
;     (display xs))
;   '(print "null list!\n"))
; ;
; ; ((uncons '(1 2 3 4) 'x 'xs)
; ;   ((uncons xs '_ '_)
; ;     (print "at least two\n")
; ;     (print "singleton\n"))
; ;   (print "null\n"))