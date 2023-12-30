;; inspired by slide 10 of https://web.archive.org/web/20170626072601/http://www.ccs.neu.edu/events/wand-symposium/talks/mitchfest-09-dybvig.pdf

(define DEBUG-THREADS #f)

(define (debug-printf . args)
  (if DEBUG-THREADS
      (apply printf args)
      'ok))

(define (parallel-exec n setup-thunk)
  (let ((m (make-mutex))
        (c (make-condition))
        (done #f))
    (let ((dec-and-signal-if-last
           (lambda ()
             (set! n (- n 1))
             (when (= n 0) (condition-signal c)))))
      (with-mutex m
        (let loop ((i 0))
          (when (< i n)
            (fork-thread
             (lambda ()
               (let ((thunk (setup-thunk i)))
                 (let eng-loop ((eng (make-engine thunk)))
                   (eng 50000
                        (lambda (ticks value)
                          (with-mutex m
                            (if done
                                (debug-printf "already done ~a\n" i)
                                (begin
                                  (debug-printf "done thanks to ~a\n" i)
                                  (set! done value)))
                            (dec-and-signal-if-last)))
                        (lambda (new-eng)
                          (if (with-mutex m done)
                              (with-mutex m
                                (debug-printf "stopping engine ~a\n" i)
                                (dec-and-signal-if-last))
                              (eng-loop new-eng))))))))
            (loop (+ i 1))))
        (condition-wait c m)
        done))))
