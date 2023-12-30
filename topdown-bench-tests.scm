(load "load.scm")
(load "topdown-bench.scm")
(load "threads.scm")

(set! DEBUG-THREADS #t)

(define thunk
  (lambda ()
    (time
     (synthesize
      'append 2 '(xs ys)
      '(((append '() '()) ())
        ((append '(a) '(b)) (a b))
        ((append '(g) '(h)) (g h))
        ((append '(c d) '(e f)) (c d e f))
        ((append '(w x y z) '(1 2 3 4)) (w x y z 1 2 3 4))
        )
      '(expansion-count . 50000)
      ))))

(define setup-thunk
  (lambda (i)
    ;; this seed is problematic, something seems wrong...
    ;;(random-seed (modulo (+ (random-seed) i) (expt 2 32)))
    (random-seed (modulo (+ 100000 (random-seed) (* 5 i)) (expt 2 32)))
    thunk))

(printf "ONE RUN\n")
(run-benchmark 10 thunk)
(printf "\n\n\n")
(printf "THREE // RUNS\n")
(run-benchmark 10 (lambda () (parallel-exec 3 setup-thunk)))
