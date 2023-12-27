(test
  (hole? '_.0)
  #t)

(test
  (hole? 'a)
  #f)

(test
  (my-eval
   '(letrec ((foo
              (lambda (n)
                (if (= n 0)
                    1
                    _.0))))
      (foo 0))
   )
  1)

(test
  (my-eval
   '(letrec ((foo
              (lambda (n)
                (if (= n 0)
                    1
                    _.0))))
      (foo 10))
   )
  HOLE)

(test
  (lambda-params '(lambda (x) body))
  '(x))

(test
  (lambda-body '(lambda (x) body))
  'body)

(test
  (my-eval '2)
  2)

(test
  (my-eval 2)
  2)

(test
  (my-eval '(+ 1 2))
  3)

(test
  (my-eval '(+ 1 (+ 2 3)))
  6)

(test
  (my-eval '(quote (hello world)))
  '(hello world))

(test
  (my-eval '((lambda (x) (+ x 1)) 2))
  3)

(test
  (my-eval '(if #t 1 2))
  1)

(test
  (my-eval '(assert (= (+ 1 2) 3)))
  #t)

(test
  (my-eval '(let ((x (+ 1 2)) (y (+ 3 4))) (+ x y)))
  10)

(define ycombinator-term
  '(lambda (fun)
     ((lambda (F)
        (F F))
      (lambda (F)
        (fun (lambda (x) ((F F) x)))))))

(define factorial-term
  '(lambda (factorial)
       (lambda (n)
         (if (= n 0)
             1
             (* n (factorial (- n 1)))))))

(test
  (my-eval
   (list (list ycombinator-term factorial-term) 6)
   )
  720)

(test
  (my-eval
   '(letrec ((factorial
              (lambda (n)
                (if (= n 0)
                    1
                    (* n (factorial (- n 1)))))))
      (factorial 6))
   )
  720)

(test
  (define-lambda '(define f (lambda (x) x)))
  '(lambda (x) x))

(test
  (define-lambda '(define (f x) x))
  '(lambda (x) x))
