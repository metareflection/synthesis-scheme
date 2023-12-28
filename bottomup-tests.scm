(test
  (let-values
      (((p piecess)
        (bottomup ops
         '(a b)
         (compute-io* (lambda (a b) (+ (* a a) b)) 'f '((1 2) (3 4) (2 1))))))
    (piece-expr p))
  '(+ (* a a) b))

(test
  (fill-bottomup
   'f
   '(a b)
   (compute-io* (lambda (a b) (+ (* a a) b)) 'f '((1 2) (3 4) (2 1))))
  '(((+ (* a a) b))))

(test
  (fill-bottomup
   'f
   '(?rec l)
   '(((f 0 '(c)) 1) ((f 1 '(b c)) 2) ((f 2 '(a b c)) 3)))
  '(((add1 ?rec))))

(test
  (fill-bottomup
   'f
   '(x)
   '(((f 0) 0))
   '(base-case? . #t)
   )
  '((0)))

(test
  (fill-bottomup
   'f
   '(x)
   '(((f 0) 0)))
  '((x)))

(test
  (fill-bottomup
   'f
   '(?rec l)
   '(((f '(1 1) '(1 1)) (1 1 1 1))))
  '(((append l l))))

(test
  (fill-bottomup
   'f
   '(n)
   '(((f 1) 3)))
  '(((add1 (add1 n)))))

(test
  (let-values
      (((p piecess)
        (bottomup ops
         '(x)
         '(((f 3) 6)
           ((f 1) 2)))))
    (list p piecess))
  '((piece (expr . (* 2 x)) (size . 3) (deno 6 2))
    (()
     ((piece (expr . x) (size . 1) (deno 3 1))
      (piece (expr . (quote ())) (size . 1) (deno () ()))
      (piece (expr . 6) (size . 1) (deno 6 6))
      (piece (expr . 2) (size . 1) (deno 2 2)))
     ((piece (expr . (add1 x)) (size . 2) (deno 4 2))
      (piece (expr . (add1 6)) (size . 2) (deno 7 7))
      (piece (expr . (add1 2)) (size . 2) (deno 3 3)))
     ((piece (expr . (cons x x)) (size . 3) (deno (3 . 3) (1 . 1)))
      (piece (expr . (cons x '())) (size . 3) (deno (3) (1)))
      (piece (expr . (cons x 6)) (size . 3) (deno (3 . 6) (1 . 6)))
      (piece (expr . (cons x 2)) (size . 3) (deno (3 . 2) (1 . 2)))
      (piece (expr . (cons '() x)) (size . 3) (deno (() . 3) (() . 1)))
      (piece (expr . (cons '() '())) (size . 3) (deno (()) (())))
      (piece (expr . (cons '() 6)) (size . 3) (deno (() . 6) (() . 6)))
      (piece (expr . (cons '() 2)) (size . 3) (deno (() . 2) (() . 2)))
      (piece (expr . (cons 6 x)) (size . 3) (deno (6 . 3) (6 . 1)))
      (piece (expr . (cons 6 '())) (size . 3) (deno (6) (6)))
      (piece (expr . (cons 6 6)) (size . 3) (deno (6 . 6) (6 . 6)))
      (piece (expr . (cons 6 2)) (size . 3) (deno (6 . 2) (6 . 2)))
      (piece (expr . (cons 2 x)) (size . 3) (deno (2 . 3) (2 . 1)))
      (piece (expr . (cons 2 '())) (size . 3) (deno (2) (2)))
      (piece (expr . (cons 2 6)) (size . 3) (deno (2 . 6) (2 . 6)))
      (piece (expr . (cons 2 2)) (size . 3) (deno (2 . 2) (2 . 2)))
      (piece (expr . (+ 6 x)) (size . 3) (deno 9 7))
      (piece (expr . (+ 2 x)) (size . 3) (deno 5 3))
      (piece (expr . (+ 2 6)) (size . 3) (deno 8 8))
      (piece (expr . (* x x)) (size . 3) (deno 9 1))
      (piece (expr . (* 6 x)) (size . 3) (deno 18 6))
      (piece (expr . (* 6 6)) (size . 3) (deno 36 36))
      (piece (expr . (* 2 x)) (size . 3) (deno 6 2))
      (piece (expr . (* 2 6)) (size . 3) (deno 12 12))
      (piece (expr . (* 2 2)) (size . 3) (deno 4 4))))))

(test
  (fill-bottomup
   'f
   '(n)
   '(((f 0) 0)
     ((f 1) 100)
     ((f 2) 200)))
  '(((* 100 n))))

(test
  (fill-bottomup
   'app
   '(f x)
   '(((app (lambda (x) (+ x 1)) 2) 3)
     ((app (lambda (x) (* x 2)) 3) 6)
     ((app - 4) -4)))
  '(((f x))))

(test
  (fill-bottomup
   'm
   '(?rec f xs)
   '(((m '(3 2) (lambda (x) (+ x 1)) '(3 2 1)) (4 3 2))
     ((m '(2) (lambda (x) (+ x 1)) '(2 1)) (3 2))
     ((m '() (lambda (x) (+ x 1)) '(1)) (2))
     ((m '(6 3) (lambda (x) (* x 3)) '(3 2 1)) (9 6 3))
     ((m '(3) (lambda (x) (* x 3)) '(2 1)) (6 3))
     ((m '() (lambda (x) (* x 3)) '(1)) (3))))
  '(((cons (f (car xs)) ?rec))))

(test
  (fill-bottomup
   'f
   '(?rec p xs)
   '(((f '(4 2) even? '(3 4 2 1)) (4 2))
     ((f '(2) even? '(4 2 1)) (4 2))
     ((f '() even? '(2 1)) (2))
     ((f '() even? '(1)) ())
     ((f '(2 1) (lambda (x) (not (= x 0))) '(3 0 2 1 0)) (3 2 1))
     ((f '(2 1) (lambda (x) (not (= x 0))) '(0 2 1 0)) (2 1))
     ((f '(1) (lambda (x) (not (= x 0))) '(2 1 0)) (2 1))
     ((f '() (lambda (x) (not (= x 0))) '(1 0)) (1))
     ((f '() (lambda (x) (not (= x 0))) '(0)) ()))
   '(conditional? . #t))
  '(((if (p (car xs)) (cons (car xs) ?rec) ?rec))))
