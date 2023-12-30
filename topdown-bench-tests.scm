(load "load.scm")
(load "topdown-bench.scm")
(load "threads.scm")

(define thunk
  (lambda ()
    (time-test
     (synthesize
      'append 2 '(xs ys)
      '(((append '() '()) ())
        ((append '(a) '(b)) (a b))
        ((append '(g) '(h)) (g h))
        ((append '(c d) '(e f)) (c d e f))
        ((append '(w x y z) '(1 2 3 4)) (w x y z 1 2 3 4))
        )
      ;;'(expansion-count . 50000)
      )
     '(((if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))))

(define setup-thunk
  (lambda (i)
    (random-seed (+ (random-seed) i))
    (thunk)))

(printf "ONE RUN\n")
(run-benchmark 10 thunk)
(printf "\n\n\n")
(printf "THREE // RUNS\n")
(run-benchmark 10 (lambda () (parallel-exec 3 setup-thunk)))
