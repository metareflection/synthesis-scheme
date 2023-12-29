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
  (map
   (lambda (i)
     (let ((start (current-time)))
       (thunk)
       (time-difference (current-time) start)))
   (iota n)))

(define r
  (benchmark
   10
   (lambda ()
     (time-test
       (synthesize
        'append 2 '(xs ys)
        '(((append '() '()) ())
          ((append '(a) '(b)) (a b))
          ((append '(g) '(h)) (g h))
          ((append '(c d) '(e f)) (c d e f))
          ((append '(w x y z) '(1 2 3 4)) (w x y z 1 2 3 4))
          ))
       '(((if (null? xs) ys (cons (car xs) (append (cdr xs) ys)))))))))

(printf "~a\n" (stats r))
