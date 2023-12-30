;; inspired by https://web.archive.org/web/20170626072601/http://www.ccs.neu.edu/events/wand-symposium/talks/mitchfest-09-dybvig.pdf
(define (parallel-exe n thunk)
  (let ((m (make-mutex))
        (c (make-condition))
        (done #f))
    (with-mutex m
      (let loop ((i 0))
        (when (< i n)
          (fork-thread
           (lambda ()
             (random-seed (+ 1000 (* 5 i)))
             (let ((result (thunk)))
               (with-mutex m
                 (unless done
                   (set! done result)
                   (condition-signal c))))))
            (loop (+ i 1))))
      (condition-wait c m))
    done))

(printf "//-exe 10\n")
(time (parallel-exe 10 (lambda () (let loop ((i 0)) (if (= i 1000000000) 'done (loop (+ i 1))))))) ;; 1.8s
(printf "//-exe 3\n")
(time (parallel-exe 3  (lambda () (let loop ((i 0)) (if (= i 1000000000) 'done (loop (+ i 1))))))) ;; 1.4s

#;
(let ()
  (define thunk
    (lambda ()
      (synthesize
       'append 2 '(xs ys)
       '(((append '() '()) ())
         ((append '(a) '(b)) (a b))
         ((append '(g) '(h)) (g h))
         ((append '(c d) '(e f)) (c d e f))
         ((append '(w x y z) '(1 2 3 4)) (w x y z 1 2 3 4))
         )
       '(expansion-count . 50000))
      )
    )
  (time-test
   (parallel-exe 3 thunk)
   '(((if (null? xs) ys (cons (car xs) (append (cdr xs) ys)))))))
