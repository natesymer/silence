#!../dist/build/silence/silence -f

(import "../stdlib.sl")

;; Basic example that communicates with Postgresql.

(let! 'postgres-sl (dlopen "postgres-sl.dylib"))

(func 'connect () ((foreign postgres-sl "connect_pg")))
(func 'query (conn sql) ((foreign postgres-sl "query_pg") conn sql))


(func 'printpair (p) (begin (print (show (car p))) (print ": ") (println (cdr p)))) 

(with connect (lambda (pgconn)
  (map (map printpair) (query pgconn "select * from department"))))

(free postgres-sl)