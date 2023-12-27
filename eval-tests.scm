(test
  (lambda-params '(lambda (x) body))
  '(x))

(test
  (lambda-body '(lambda (x) body))
  'body)

(test
  (eval-expr '2 global-env)
  2)

(test
  (eval-expr 2 global-env)
  2)

(test
  (eval-expr '(+ 1 2) global-env)
  3)

(test
  (eval-expr '(+ 1 (+ 2 3)) global-env)
  6)

(test
  (eval-expr '(quote (hello world)) global-env)
  '(hello world))

(test
  (eval-expr 'x (list (add-binding 'x 3)))
  3)

(test
  (eval-expr '((lambda (x) (+ x 1)) 2) global-env)
  3)

(test
  (eval-expr '(if #t 1 2) global-env)
  1)

(test
  (eval-expr '(assert (= (+ 1 2) 3)) global-env)
  #t)

(test
  (eval-expr '(let ((x (+ 1 2)) (y (+ 3 4))) (+ x y)) global-env)
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
  (eval-expr
   (list (list ycombinator-term factorial-term) 6)
   global-env)
  720)

(test
  (eval-expr
   '(letrec ((factorial
              (lambda (n)
                (if (= n 0)
                    1
                    (* n (factorial (- n 1)))))))
      (factorial 6))
   global-env)
  720)

(test
  (define-lambda '(define f (lambda (x) x)))
  '(lambda (x) x))

(test
  (define-lambda '(define (f x) x))
  '(lambda (x) x))
