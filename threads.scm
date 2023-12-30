(load "load.scm")

;; inspired by slide 10 of https://web.archive.org/web/20170626072601/http://www.ccs.neu.edu/events/wand-symposium/talks/mitchfest-09-dybvig.pdf

(define (parallel-exe-no-engines n thunk)
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
                 (printf "now done ~a\n" i)
                 (set! done result)
                 (condition-signal c)))))
            (loop (+ i 1))))
      (condition-wait c m)
      done)))

(define (parallel-exe-engines n thunk)
  (let ((m (make-mutex))
        (c (make-condition))
        (done #f))
    (with-mutex m
      (let loop ((i 0))
        (when (< i n)
          (fork-thread
           (lambda ()
             (random-seed (+ 10000 (* 5 i)))
             (let eng-loop ((eng (make-engine thunk)))
               (eng 800
                    (lambda (ticks value)
                      (with-mutex m
                        (if done
                            (printf "already done ~a\n" i)
                            (begin
                              (printf "done thanks to ~a\n" i)
                              (set! done value)
                              (condition-signal c)))))
                    (lambda (new-eng)
                      (if (with-mutex m done)
                          (with-mutex m (printf "stopping engine ~a\n" i))
                          (eng-loop new-eng)))))))
          (loop (+ i 1))))
      (condition-wait c m)
      done)))

(define (test-// t parallel-exe)
  (printf "//-exe 10 ~a\n" t)
  (time (parallel-exe 10 (lambda () (let loop ((i 0)) (if (= i 100000000) 'done (loop (+ i 1))))))) ;; 1.8s
  (printf "//-exe 3 ~a\n" t)
  (time (parallel-exe 3  (lambda () (let loop ((i 0)) (if (= i 100000000) 'done (loop (+ i 1))))))) ;; 1.4s

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
         '(expansion-count . 50000)
         )
        )
      )
    (printf "//-3 for synthesizer ~a\n" t)
    (time-test
     (parallel-exe 3 thunk)
     '(((if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))
    (printf "//-10 for synthesizer ~a\n" t)
    (time-test
     (parallel-exe 10 thunk)
     '(((if (null? xs) ys (cons (car xs) (append (cdr xs) ys)))))))
  )

(test-// "WITHOUT ENGINES" parallel-exe-no-engines)
(test-// "WITH ENGINES" parallel-exe-engines)
