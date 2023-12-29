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
                   (when done (condition-signal c)))))))
            (loop (+ i 1))))
      (condition-wait c m))
    done))

(define thunk
  (lambda ()
    (synthesize
     'append 2 '(xs ys)
     '(((append '() '()) ())
       ((append '(a) '(b)) (a b))
       ((append '(g) '(h)) (g h))
       ((append '(c d) '(e f)) (c d e f))
       ((append '(w x y z) '(1 2 3 4)) (w x y z 1 2 3 4))
       ))))

(time-test
 (parallel-exe 10 thunk)
 '(((if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))
