(test
  (all-subsets '(0 1))
  '((0 1) (0) (1) ()))

(test
  (find-io-recursive-pairs recursion-pattern-list 'append ex-io-append ex-io-append)
  '((((append '(1 2 3) '(x y z)) (1 2 3 x y z))
     ((append '(2 3) '(x y z)) (2 3 x y z)))
    (((append '(2 3) '(x y z)) (2 3 x y z))
     ((append '(3) '(x y z)) (3 x y z)))
    (((append '(3) '(x y z)) (3 x y z))
     ((append '() '(x y z)) (x y z)))))

(test
    (find-io-recursive-triples recursion-pattern-list 0 'flatten ex-io-flatten ex-io-flatten)
    '((((flatten '((1 2) (3 4) (5 6))) (1 2 3 4 5 6))
       ((flatten '(1 2)) (1 2))
       ((flatten '((3 4) (5 6))) (3 4 5 6)))
      (((flatten '(((a b)) c d e)) (a b c d e))
       ((flatten '((a b))) (a b))
       ((flatten '(c d e)) (c d e)))))

(test
  (find-io-recursive-pairs recursion-pattern-nat 'factorial ex-io-factorial ex-io-factorial)
  '((((factorial 6) 720) ((factorial 5) 120)))
  )

(test
  (variadic-to-list 'product* ex-io-product)
  '(((product* (list)) 1)
    ((product* (list 3)) 3)
    ((product* (list 3 5)) 15)
    ((product* (list 2 4 5)) 40)
    ((product* (list 4 5)) 20)))

(test
  (play-computer-check-variadic 'product* 'l 'product ex-io-product)
  '(begin
     (define product*
       (lambda (l)
         (cond [(null? l) 1] [else (* (car l) (product* (cdr l)))])))
     (define product (lambda l (product* l)))))

(test
  (variadic-to-list 'sentence* ex-io-sentence)
  '(((sentence* (list)) ())
  ((sentence* (list 'hello)) (hello))
  ((sentence* (list 'hello 'my)) (hello my))
  ((sentence* (list 'hello 'my 'name)) (hello my name))
  ((sentence* (list 'hello 'my 'name 'is)) (hello my name is))))

(test
  (play-computer-check-variadic 'sentence* 'l 'sentence ex-io-sentence)
  '(begin
    (define sentence*
      (lambda (l) (cond [(null? l) '()] [else l])))
    (define sentence (lambda l (sentence* l)))))

(test
  (play-computer-check recursion-pattern-list '(l s) 'append ex-io-append)
  '(define append
     (lambda (l s)
       (cond
         ((null? l) s)
         (else (cons (car l) (append (cdr l) s)))))))

(test
  ;; TODO: the system should know to reuse append instead of re-defining a new recursive function.
  (play-computer-check recursion-pattern-list '(l) 'append123 ex-io-append123)
  '(define append123
     (lambda (l)
       (cond
         ((null? l) '(1 2 3))
         (else (cons (car l) (append123 (cdr l))))))))

(test
  (play-computer-check recursion-pattern-list '(xs) 'stutter ex-io-stutter)
  '(define stutter
     (lambda (xs)
       (cond
         ((null? xs) '())
         (else (cons (car xs) (cons (car xs) (stutter (cdr xs)))))))))

(test
  (play-computer-check recursion-pattern-list-index1 '(f xs) 'map ex-io-map)
  '(define map
     (lambda (f xs)
       (cond
         [(null? xs) '()]
         [else (cons (f (car xs)) (map f (cdr xs)))]))))

(test
  (play-computer-check recursion-pattern-list '(fs x) 'compose-list ex-io-compose-list)
  '(define compose-list
    (lambda (fs x)
      (cond
        [(null? fs) x]
        [else ((car fs) (compose-list (cdr fs) x))]))))

(test
  (play-computer-check recursion-pattern-list '(xs) 'even-length? ex-io-even-length?)
  '(define even-length?
    (lambda (xs)
      (cond
        ((null? xs) #t)
        (else (not (even-length? (cdr xs))))))))

(test
  (play-computer-check recursion-pattern-list '(xs) 'length ex-io-length)
  '(define length
    (lambda (xs)
      (cond
        ((null? xs) 0)
        (else (add1 (length (cdr xs))))))))

(test
  (play-computer-check recursion-pattern-list '(xs) 'length ex-io-length-bis)
  '(define length
    (lambda (xs)
      (cond
        ((null? xs) 0)
        (else (add1 (length (cdr xs))))))))

(test
  (play-computer-check recursion-pattern-nat '(n) 'factorial ex-io-factorial)
  '(define factorial
     (lambda (n)
       (cond
         ((= n 0) 1)
         (else (* n (factorial (- n 1))))))))

(test
  (play-computer-deep-check recursion-pattern-list 0 '(ll) 'flatten ex-io-flatten)
  '(define flatten
     (lambda (ll)
       (cond
         ((null? ll) '())
         ((pair? (car ll)) (append (flatten (car ll)) (flatten (cdr ll))))
         (else (cons (car ll) (flatten (cdr ll))))))))

(test
 (play-computer-check recursion-pattern-nat '(n) 'sum ex-io-sum)
 '(define sum
    (lambda (n)
      (cond
        ((= n 0) 0)
        (else (+ n (sum (- n 1))))))))

(test
  (play-computer-check recursion-pattern-list2 '(l s) 'same-length? ex-io-same-length?)
  '(define same-length?
     (lambda (l s)
       (cond
         [(and (null? l) (null? s)) #t]
         [(null? l) #f]
         [(null? s) #f]
         [else (same-length? (cdr l) (cdr s))]))))

(test
  (play-computer-check recursion-pattern-nat2 '(a b) 'nat> ex-io-nat>)
  '(define nat>
     (lambda (a b)
       (cond
         ((= a 0) #f)
         ((= b 0) #t)
         (else (nat> (- a 1) (- b 1)))))))

(test
  (play-computer-check recursion-pattern-nat2 '(a b) 'nat>= ex-io-nat>=)
  '(define nat>=
     (lambda (a b)
       (cond
         ((= b 0) #t)
         ((= a 0) #f)
         (else (nat>= (- a 1) (- b 1)))))))

(test
  (play-computer-check recursion-pattern-list2 '(l s) 'addlists ex-io-addlists)
  '(define addlists
    (lambda (l s)
      (cond
        [(null? l) '()]
        [(null? s) '()]
        [else
          (cons (+ (car s) (car l)) (addlists (cdr l) (cdr s)))]))))

(test
(play-computer-check recursion-pattern-list2 '(l s) 'longer? ex-io-longer?)
'(define longer?
  (lambda (l s)
    (cond
      [(null? l) #f]
      [(null? s) #t]
      [else (longer? (cdr l) (cdr s))]))))

(test 
  (play-computer-check recursion-pattern-nat '(n) 'countdown ex-io-countdown)
  '(define countdown
  (lambda (n)
    (cond [(= n 0) '(0)] [else (cons n (countdown (- n 1)))]))))

(test
  (play-computer-deep-check recursion-pattern-list 0 '(l) 'deep-size ex-io-deep-size)
  '(define deep-size
     (lambda (l)
       (cond
         [(null? l) 0]
         [(pair? (car l))
          (+ (deep-size (cdr l)) (deep-size (car l)))]
         [else (add1 (deep-size (cdr l)))]))))

(test
  (check-prog
   (play-computer-split
    recursion-pattern-list '(l) 'even-only ex-io-even-only
    (vanilla-case-split
     recursion-pattern-list
     (lambda (formals) `(even? (car ,(car formals))))
     (lambda (arg-exprs) (even? (car (my-eval (car arg-exprs)))))))
   ex-io-even-only)
  '(define even-only
     (lambda (l)
       (cond
         ((null? l)
          '())
         ((even? (car l))
          (cons (car l) (even-only (cdr l))))
         (else
          (even-only (cdr l)))))))

(test
  (check-prog
   (play-computer-split
    recursion-pattern-list '(l) 'deep-even-only ex-io-deep-even-only
    (deep-recursion-case-split recursion-pattern-list 0)
    (vanilla-case-split
     recursion-pattern-list
     (lambda (formals) `(even? (car ,(car formals))))
     (lambda (arg-exprs) (even? (car (my-eval (car arg-exprs)))))))
   ex-io-deep-even-only)
  '(define deep-even-only
     (lambda (l)
       (cond
         ((null? l)
          '())
         ((pair? (car l))
          (cons (deep-even-only (car l)) (deep-even-only (cdr l))))
         ((even? (car l))
          (cons (car l) (deep-even-only (cdr l))))
         (else
          (deep-even-only (cdr l)))))))

(test
  (check-prog
   (play-computer-split
    recursion-pattern-list-index1 '(p xs) 'filter ex-io-filter
    (vanilla-case-split
     recursion-pattern-list-index1
     (lambda (formals) `(,(car formals) (car ,(cadr formals))))
     (lambda (arg-exprs) (my-eval `(,(car arg-exprs) (car ,(cadr arg-exprs)))))))
   ex-io-filter)
  '(define filter
     (lambda (p xs)
       (cond
         [(null? xs) '()]
         [(p (car xs)) (cons (car xs) (filter p (cdr xs)))]
         [else (filter p (cdr xs))]))))

(define (fib-split rec-pattern)
  (make-case-split
   (lambda (arg-exprs) #t)
   (lambda (formals) 'else)
   (lambda (fun-name formals all-io* split-io*)
     (fill-recursive-case-from-io*
      (list
       (make-subcall
        '?n-2
        (lambda (arg-exprs)
          (list (- (my-eval (car arg-exprs)) 2)))
        (lambda (fun-name formals)
          `(,fun-name (- ,(car formals) 2))))
       (subcall-rec rec-pattern))
      all-io* split-io* rec-pattern fun-name formals))))

(test
  (check-prog
   (play-computer-split
    recursion-pattern-nat '(n) 'fib ex-io-fib
    (vanilla-case-split
     #f
     (lambda (formals) `(= ,(car formals) 1))
     (lambda (arg-exprs) (= (my-eval (car arg-exprs)) 1)))
    (fib-split recursion-pattern-nat))
   ex-io-fib)
  '(define fib
     (lambda (n)
       (cond
         ((= n 0) 0)
         ((= n 1) n)
         (else (+ (fib (- n 1)) (fib (- n 2))))))))

(test
  (check-prog
   (play-computer-split
    recursion-pattern-nat01 '(n) 'fib ex-io-fib
    (fib-split recursion-pattern-nat01))
   ex-io-fib)
  '(define fib
     (lambda (n)
       (cond
         ((< n 2) n)
         (else (+ (fib (- n 1)) (fib (- n 2))))))))

(test
 (let ((calls             0)
       (underlying-solver fill-bottomup)) 
   (parameterize ((*base-solver*
                   (lambda args
                     (set! calls (add1 calls))
                     (apply underlying-solver args))))
     (play-computer-check recursion-pattern-list '(xs) 'length ex-io-length)
     calls))
 2)

(test
  (parameterize ((*base-solver*
                  (lambda (fun-name formals io* . options)
                    (apply fill-bottomup fun-name formals io*
                           (cons '(conditional? . #t) options)))))
    (play-computer-check recursion-pattern-list-index1 '(p xs) 'filter ex-io-filter))
  '(define filter
     (lambda (p xs)
       (cond
         [(null? xs) '()]
         [else
          (if (p (car xs))
              (cons (car xs) (filter p (cdr xs)))
              (filter p (cdr xs)))]))))

(test-error
 (play-computer-check recursion-pattern-nat '(n) 'fib ex-io-fib))
