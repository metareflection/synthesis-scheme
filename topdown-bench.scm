(define (mean ds)
  (exact->inexact (/ (apply + ds) (length ds))))
(define (square x) (* x x))
(define (std-dev ds u)
  (sqrt (/ (apply + (map (lambda (x) (square (- x u))) ds)) (length ds))))
(define (stats r)
  (let* ((ds (map time-second r))
         (u (mean ds))
         (s (std-dev ds u)))
    (list u s (apply min ds) (apply max ds))))

(define (benchmark n thunk)
  (let ((count 1))
    (map
     (lambda (i)
       (printf "RUN ~a\n" count)
       (set! count (+ count 1))
       (let ((start (current-time)))
         (let* ((r (thunk))
                (t (time-difference (current-time) start)))
           (list t r))))
     (iota n))))

(define (run-benchmark n thunk)
  (let* ((r (benchmark n thunk))
         (s (stats (map car r)))
         (c (length (filter not (map cadr r)))))
    (printf "~a\n" (cons c s))
    s))
