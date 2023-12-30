(load "load.scm")
(load "threads.scm")

(set! DEBUG-THREADS #t)

(define setup-thunk
  (lambda (i)
    (random-seed (+ 100000 (* 5 i)))
    (lambda ()
      (synthesize
       'append 2 '(xs ys)
       '(((append '() '()) ())
         ((append '(a) '(b)) (a b))
         ((append '(g) '(h)) (g h))
         ((append '(c d) '(e f)) (c d e f))
         ((append '(w x y z) '(1 2 3 4)) (w x y z 1 2 3 4))
         )
       '(expansion-count . 50000)))))

(let loop ((i 1))
  (if (> i 10)
      'done
      (begin
        (printf "//-~a\n" i)
        (time-test
         (parallel-exec i setup-thunk)
         '(((if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))
        (loop (+ i 1)))))

