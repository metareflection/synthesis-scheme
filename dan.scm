(define-record-type
    recursion-scheme
  (fields
   type ;;predicate
   base-case?-expr ;;function
   base-case? ;;predicate
   base-value
   sub-fun-expr ;;function
   sub-fun ;;function
   ))

(define-record-type
    recursion-pattern
  (fields
   test-base-case-expr ;;function
   find-base-case ;;function
   base-case-keys ;;list
   key-subsumed-by? ;;predicate
   key-singleton? ;;predicate
   base-case? ;;predicate
   sub-fun ;;function
   sub-expr ;;function
    ))

(define-record-type
    case-split
  (fields
   in-case? ;;predicate
   in-case?-expr ;;function
   fill-case-expr ;;function
   ))

(define-record-type
    subcall
  (fields
   formal ;;symbol
   fun ;;function
   expr ;;function
   ))

(define (subcall-rec rec-pattern)
  (make-subcall
   '?rec
   (recursion-pattern-sub-fun rec-pattern)
   (recursion-pattern-sub-expr rec-pattern)))

(define (subcall-deep-car index)
  (make-subcall
   '?rec-car
   (lambda (arg-exprs)
     (let* ((arg-vals (map my-eval arg-exprs))
            (rec-arg (list-ref arg-vals index))
            (sub-arg-vals (list-replace arg-vals index (car rec-arg))))
       sub-arg-vals))
   (lambda (fun-name formals)
     `(,fun-name (car ,(list-ref formals index))))))

(define (find-io-recursive-cohort subcalls rec-pattern fun-name all-io* io*)
  (filter
   (lambda (x) x)
   (map
    (lambda (io)
      (let* ((arg-exprs (io-args io))
             (subvals*
              (map (lambda (subcall) ((subcall-fun subcall) arg-exprs))
                   subcalls))
             (sub-io*s
              (map (lambda (subvals) (filter (find-io fun-name subvals) all-io*))
                   subvals*)))
        (if (all (lambda (x) (not (null? x))) sub-io*s)
            (cons io (map car sub-io*s))
            #f)))
    (filter                              ;
     (lambda (io) (not ((recursion-pattern-base-case? rec-pattern) (io-args io))))
     io*))))

(define (deep-recursion-case-split rec-pattern index)
  (make-case-split
   (lambda (arg-exprs)
     (let* ((arg-vals (map my-eval arg-exprs))
            (rec-arg (list-ref arg-vals index)))
       (pair? (car rec-arg))))
   (lambda (formals)
     (let ((rec-param (list-ref formals index)))
       `(pair? (car ,rec-param))))
   (lambda (fun-name formals all-io* split-io*)
     (fill-recursive-case-from-io*
      (list (subcall-deep-car index) (subcall-rec rec-pattern))
      all-io* split-io* rec-pattern fun-name formals))))

(define (vanilla-case-split rec-pattern-or-none predicate-expr predicate)
  (make-case-split
   (lambda (arg-exprs)
     (predicate arg-exprs))
   (lambda (formals)
     (predicate-expr formals))
   (lambda (fun-name formals all-io* split-io*)
     (if rec-pattern-or-none
         (fill-recursive-case-from-io*
          (list (subcall-rec rec-pattern-or-none))
          all-io* split-io*
          rec-pattern-or-none fun-name formals)
         (caar (fill-with-some-solver (lambda (filler) (filler fun-name formals split-io*))))))))


(define (else-case-split rec-pattern)
  (vanilla-case-split rec-pattern
                      (lambda (formals) 'else)
                      (lambda (arg-exprs) #t)))
  
(define (recursion-pattern-one-index rec-scheme rec-index)
  (recursion-pattern-index* (list (cons rec-index rec-scheme))))

(define (all-subsets xs)
  (if (null? xs)
      '(())
      (let ((ss (all-subsets (cdr xs))))
        (append
         (map (lambda (s) (cons (car xs) s)) ss)
         ss))))

(define (maybe-and-expr es)
  (if (null? (cdr es))
      (car es)
      (cons 'and es)))

(define (recursion-pattern-index* rec-scheme-map)
  (let ((index* (map car rec-scheme-map))
        (rec-scheme-of (lambda (i) (cdr (assoc i rec-scheme-map)))))
    (make-recursion-pattern
     ;;test-base-case-expr
     (lambda (key formals)
       (maybe-and-expr
        (filter
         (lambda (x) x)
         (map
          (lambda (i formal)
            (if (member i key)
                ((recursion-scheme-base-case?-expr (rec-scheme-of i)) formal)
                #f))
          (iota (length formals)) formals))))
     ;;find-base-case
     (lambda (key fun-name formals)
       (search-io
        fun-name
        (lambda (input)
          (all
           (lambda (i)
             (if (member i index*)
                 (let ((r
                        ((recursion-scheme-base-case? (rec-scheme-of i))
                         (my-eval (list-ref (cdr input) i)))))
                   (if (member i key)
                       r
                       (not r)))
                 #t))
           (iota (length formals))))))
     ;;base-case-keys
     (filter (lambda (x) (not (null? x))) (all-subsets index*))
     ;;key-subsumed-by?
     (lambda (key1 key2)
       (and (not (equal? key1 key2))
	    (all (lambda (i2) (member i2 key1)) key2)))
     ;;key-singleton?
     (lambda (key)
       (null? (cdr key)))
     ;;base-case?
     (lambda (arg-exprs)
       (some
        (lambda (i e)
          (if (member i index*)
              ((recursion-scheme-base-case? (rec-scheme-of i))
               (my-eval e))
              #f))
        (iota (length arg-exprs)) arg-exprs))
     ;;sub-fun
     (lambda (arg-exprs)
       (map
        (lambda (i v)
          (if (member i index*)
              ((recursion-scheme-sub-fun (rec-scheme-of i)) v)
              v))
        (iota (length arg-exprs)) (map my-eval arg-exprs)))
     ;;sub-expr
     (lambda (fun-name formals)
       (let* ((sub-arg-exprs
               (map
                (lambda (i formal)
                  (if (member i index*)
                      ((recursion-scheme-sub-fun-expr (rec-scheme-of i)) formal)
                      formal))
                (iota (length formals)) formals))
              (sub-expr
               `(,fun-name . ,sub-arg-exprs)))
         sub-expr)))))

(define recursion-scheme-list
  (make-recursion-scheme
   list?
   (lambda (x) `(null? ,x))
   null?
   '()
   (lambda (x) `(cdr ,x))
   cdr))

(define (nat? x)
  (and (integer? x) (<= 0 x)))

(define recursion-scheme-nat
  (make-recursion-scheme
   nat?
   (lambda (x) `(= ,x 0))
   (lambda (x) (= x 0))
   0
   (lambda (x) `(- ,x 1))
   (lambda (x) (- x 1))))

(define (arg-equal? arg v)
  (value-equal? (my-eval arg) v))

(define (search-io name predicate)
  (lambda (io)
    (let ((input (car io)))
      (and (eq? (car input) name)
           (predicate input)))))

(define (find-io name vs)
  (search-io
   name
   (lambda (input)
     (all arg-equal? (cdr input) vs))))

(define (find-io-index name v i)
  (search-io
   name
   (lambda (input)
     (arg-equal? (list-ref (cdr input) i) v))))

(define (find-io-recursive-pairs rec-pattern fun-name all-io* io*)
  (find-io-recursive-cohort
   (list (subcall-rec rec-pattern))
   rec-pattern fun-name
   all-io*
   io*))

(define (find-io-recursive-triples rec-pattern index fun-name all-io* io*)
  (find-io-recursive-cohort
   (list (subcall-deep-car index) (subcall-rec rec-pattern))
   rec-pattern fun-name
   all-io*
   io*))

(define recursion-pattern-list (recursion-pattern-one-index recursion-scheme-list 0))

(define recursion-pattern-list-index1 (recursion-pattern-one-index recursion-scheme-list 1))

(define recursion-pattern-nat (recursion-pattern-one-index recursion-scheme-nat 0))

(define (io*-from-recursive-cohort dummy-name io-rec*)
  (map
   (lambda (io-rec)
     (let* ((io (car io-rec))
            (io-sub* (cdr io-rec))
            (sub-result* (map io-output io-sub*))
            (result (io-output io)))
       `((,dummy-name ,@(map maybe-quote sub-result*) . ,(io-args io))
         ,result)))
   io-rec*))

(define (fill-recursive-case-delegate dummy-name filler subcalls rec-pattern fun-name formals io*)
  (pretty-print io*)
  (let* ((subcall-formals (map (lambda (subcall) (subcall-formal subcall)) subcalls))
         (b (filler dummy-name (append subcall-formals formals) io*
                    `(fun-name . ,fun-name))))
    (if b
        (let* ((subcall-exprs
                (map (lambda (subcall) ((subcall-expr subcall) fun-name formals))
                     subcalls))
               (m (map cons subcall-formals subcall-exprs)))
          (import template)
          (let ((r (walk* (caar b) m)))
            (pretty-print r)
            r))
        #f)))

(define (fill-base-case-from-io* io* rec-pattern key fun-name formals)
  (let* ((io-base-case?
          ((recursion-pattern-find-base-case rec-pattern) key fun-name formals))
         (io-base* (filter io-base-case? io*)))
    (caar (fill-with-some-solver (lambda (filler) (filler fun-name formals io-base* '(base-case? . #t)))))))

(define (try-in-series . things-to-try)
  (lambda args
    (define loop
      (case-lambda
        [(ticks proc name . more-things)
         (let ((r ;;guard (x (else '()))
                (monitor ticks
                         (lambda (monitored-solver)
                           (apply monitored-solver args))
                         proc name)))
           (if (not (null? r))
               r
               (apply loop more-things)))]
        [() '()]))
    (apply loop things-to-try)))

(define *base-solver* (make-parameter
                       (try-in-series
                        2000000000 fill-bottomup   'bottomup
                        100000000  barliman-filler 'barliman)))

(define (fill-with-some-solver f)
  (f (*base-solver*)))

(define (fill-recursive-case-from-io* subcalls all-io* io* rec-pattern fun-name formals)
  (let* ((dummy-name 'f)
         (io-rec* (find-io-recursive-cohort subcalls rec-pattern fun-name all-io* io*))
         (io* (io*-from-recursive-cohort dummy-name io-rec*)))
    (fill-with-some-solver (lambda (filler) (fill-recursive-case-delegate dummy-name filler subcalls rec-pattern fun-name formals io*)))))

(define (fill-base-case-clauses io* rec-pattern fun-name formals)
  (let* ((base-case-keys (recursion-pattern-base-case-keys rec-pattern))
         (base-case-clauses-with-keys
          (map
           (lambda (key)
             (list
	      key
              ((recursion-pattern-test-base-case-expr rec-pattern) key formals)
              (fill-base-case-from-io* io* rec-pattern key fun-name formals)))
           base-case-keys))
         (base-case-keys-top
	  (filter
	   (lambda (c1)
	     (let ((key1 (car c1)))
	       (if ((recursion-pattern-key-singleton? rec-pattern) key1)
                   (let ((rhs1 (caddr c1))
			 (subsumed-clauses
			  (filter
			   (lambda (c2)
		             (let ((key2 (car c2)))
		               ((recursion-pattern-key-subsumed-by? rec-pattern) key2 key1)))
			   base-case-clauses-with-keys)))
		     (all (lambda (c2)
			    (let ((rhs2 (caddr c2)))
			      (equal? rhs1 rhs2)))
			  subsumed-clauses))
		   #f)))
	   base-case-clauses-with-keys))
	 (base-case-clauses-with-keys-consolidated
	  (append base-case-keys-top
		  (filter (lambda (c2)
			    (let ((key2 (car c2)))
			      (not (some (lambda (c1)
				      (let ((key1 (car c1)))
					(or (equal? key1 key2)
					    ((recursion-pattern-key-subsumed-by? rec-pattern) key2 key1))))
					 base-case-keys-top))))
			  base-case-clauses-with-keys)))
	 (base-case-clauses
	  (map cdr base-case-clauses-with-keys-consolidated)))
    base-case-clauses))

(define (fill-split-case-clauses splits all-io* io* rec-pattern fun-name formals)
  (if (null? splits)
      (begin
        (assert (null? io*))
        '())
      (let ((split (car splits)))
        (let-values
            (((in-io* out-io*)
              (partition
               (lambda (io) ((case-split-in-case? split) (io-args io)))
               io*)))
          (cons
           (list
            ((case-split-in-case?-expr split) formals)
            ((case-split-fill-case-expr split) fun-name formals all-io* in-io*))
           (fill-split-case-clauses (cdr splits) all-io* out-io* rec-pattern fun-name formals))))))

(define (play-computer rec-pattern formals fun-name io*)
  (let* ((base-case-clauses (fill-base-case-clauses
                             io* rec-pattern fun-name formals))
         (rec-case (fill-recursive-case-from-io*
                    (list (subcall-rec rec-pattern))
                    io* io* rec-pattern fun-name formals)))
    `(define ,fun-name
       (lambda ,formals
         (cond
           ,@base-case-clauses
           (else ,rec-case))))))

(define (maybe-add-else-split splits rec-pattern formals)
  (if (or (null? splits)
          (not (eq? ((case-split-in-case?-expr (last splits)) formals)
                    'else)))
      (append splits (list (else-case-split rec-pattern)))
      splits))

(define (play-computer-split rec-pattern formals fun-name io* . splits)
  (let* ((base-case-clauses (fill-base-case-clauses
                             io* rec-pattern fun-name formals))
         (split-case-clauses
          (fill-split-case-clauses
           (maybe-add-else-split splits rec-pattern formals)
           io*
           (filter
            (lambda (io)
              (not ((recursion-pattern-base-case? rec-pattern) (io-args io))))
            io*)
           rec-pattern fun-name formals)))
    `(define ,fun-name
       (lambda ,formals
         (cond
           ,@base-case-clauses
           ,@split-case-clauses)))))

(define (play-computer-deep rec-pattern index formals fun-name io*)
  (play-computer-split
   rec-pattern formals fun-name io*
   (deep-recursion-case-split rec-pattern index)))

(define (play-computer-deep-check rec-pattern index params fun-name io*)
  (check-prog (play-computer-deep rec-pattern index params fun-name io*)
              io*))

(define (check-io-examples io*)
  (cons
   'list
   (map
    (lambda (io)
      `(assert
        (equal? ,(io-input io)
                ,(maybe-quote (io-output io)))))
    io*)))

(define (check-io prog io*)
  (let ((expr
         (define-to-letrec
           prog
           (check-io-examples io*))))
    (pretty-print expr)
    (my-eval expr))
  #t)


(define (check-prog prog io)
  (pretty-print prog)
  (check-io prog io)
  prog)

(define (play-computer-check rec-pattern params fun-name io*)
  (check-prog (play-computer rec-pattern params fun-name io*)
              io*))

(define (variadic-to-list fun-name io*)
  (map
   (lambda (io)
     (list (list fun-name (cons 'list (io-args io)))
           (io-output io)))
   io*))

(define (play-computer-check-variadic list-fun-name param fun-name io*)
  (let* ((list-io* (variadic-to-list list-fun-name io*))
         (list-define (play-computer-check recursion-pattern-list (list param) list-fun-name list-io*))
         (variadic-define `(define ,fun-name (lambda ,param (,list-fun-name ,param))))
         (expr
          (define-to-letrec
            list-define
            (define-to-letrec
              variadic-define
              (check-io-examples io*)))))
    (pretty-print expr)
    (my-eval expr)
  `(begin ,list-define ,variadic-define)))

(define ex-io-append
  '(((append '() '(x)) (x))
    ((append '(1 2 3) '(x y z)) (1 2 3 x y z))
    ((append '(2 3) '(x y z)) (2 3 x y z))
    ((append '(3) '(x y z)) (3 x y z))
    ((append '() '(x y z)) (x y z))))

(define ex-io-even-length?
  '(((even-length? '()) #t)
    ((even-length? '(1)) #f)
    ((even-length? '(2 1)) #t)
    ((even-length? '(3 2 1)) #f)))

(define ex-io-butlast
  '(((butlast '())      ())
    ((butlast '(1))     ())
    ((butlast '(2 1))   (2))
    ((butlast '(3 2 1)) (3 2))))

(define ex-io-bin->nat
  '(((bin->nat '()) 0)
    ((bin->nat '(1)) 1)
    ((bin->nat '(0 1)) 2)
    ((bin->nat '(1 1)) 3)
    ((bin->nat '(0 0 1)) 4)
    ((bin->nat '(1 0 1)) 5)
    ((bin->nat '(0 1 1)) 6)
    ((bin->nat '(1 1 1)) 7)))

(define ex-io-bin->nat-be
  '(((bin->nat-be '()) 0)
    ((bin->nat-be '(1)) 1)
    ((bin->nat-be '(0 1)) 2)
    ((bin->nat-be '(1 1)) 3)
    ((bin->nat-be '(0 0 1)) 4)
    ((bin->nat-be '(1 0 1)) 5)
    ((bin->nat-be '(0 1 1)) 6)
    ((bin->nat-be '(1 1 1)) 7)))

(define ex-io-sum-list
  '(((sum-list '())        0)
    ((sum-list '(9))       9)
    ((sum-list '(1 9))     10)))

(define ex-io-sum-snoc
  '(((sum-snoc '())             0)
    ((sum-snoc '(() . 9))       9)
    ((sum-snoc '((() . 9) . 1)) 10)))

(define ex-io-stutter
  '(((stutter '(1 2 3)) (1 1 2 2 3 3))
    ((stutter '(2 3)) (2 2 3 3))
    ((stutter '()) ())))

(define ex-io-length
  '(((length '()) 0)
    ((length '(c)) 1)
    ((length '(b c)) 2)
    ((length '(a b c)) 3)))

(define ex-io-length-bis
  '(((length '(2 3 4 5 6 7)) 6)
    ((length '(1 2 3 4 5 6 7)) 7)
    ((length '()) 0)))

(define ex-io-factorial
  '(((factorial 0) 1)
    ((factorial 5) 120)
    ((factorial 6) 720)))

(define ex-io-append123
  '(((append123 '(4 5 6)) (4 5 6 1 2 3))
    ((append123 '(5 6)) (5 6 1 2 3))
    ((append123 '()) (1 2 3))))

(define ex-io-flatten
  '(((flatten '()) ())
    ((flatten '((1 2) (3 4) (5 6))) (1 2 3 4 5 6))
    ((flatten '(1 2)) (1 2))
    ((flatten '(2)) (2))
    ((flatten '(1 (2 3))) (1 2 3))
    ((flatten '((2 3))) (2 3))
    ((flatten '((3 4) (5 6))) (3 4 5 6))
    ((flatten '(((a b)) c d e)) (a b c d e))
    ((flatten '((a b))) (a b))
    ((flatten '(c d e)) (c d e))))
  
(define ex-io-product
  '(((product) 1)
    ((product 3) 3)
    ((product 3 5) 15)
    ((product 2 4 5) 40)
    ((product 4 5) 20)))

(define ex-io-sentence
  '(((sentence) ())
    ((sentence 'hello) (hello))
    ((sentence 'hello 'my) (hello my))
    ((sentence 'hello 'my 'name) (hello my name))
    ((sentence 'hello 'my 'name 'is) (hello my name is))))

(define ex-io-sum
  '(((sum 0) 0)
    ((sum 5) 15)
    ((sum 6) 21)))

(define recursion-pattern-list2
  (recursion-pattern-index*
   (list
    (cons 0 recursion-scheme-list)
    (cons 1 recursion-scheme-list))))

(define ex-io-same-length?
  '(((same-length? '(1 2) '(3 4)) #t)
    ((same-length? '(2) '(4)) #t)
    ((same-length? '() '()) #t)
    ((same-length? '(1 2 3) '(4 5)) #f)
    ((same-length? '(2 3) '(5)) #f)
    ((same-length? '(3) '()) #f)
    ((same-length? '(1 2) '(3 4 5)) #f)
    ((same-length? '(2) '(4 5)) #f)
    ((same-length? '() '(5)) #f)))

(define recursion-pattern-nat2
  (recursion-pattern-index*
   (list
    (cons 0 recursion-scheme-nat)
    (cons 1 recursion-scheme-nat))))

(define ex-io-nat>
  '(((nat> 0 0) #f)
    ((nat> 1 0) #t)
    ((nat> 0 1) #f)
    ((nat> 2 1) #t)
    ((nat> 2 0) #t)
    ((nat> 3 1) #t)
    ((nat> 0 2) #f)
    ((nat> 1 3) #f)))

(define ex-io-nat>=
  '(((nat>= 0 0) #t)
    ((nat>= 1 0) #t)
    ((nat>= 0 1) #f)
    ((nat>= 2 1) #t)
    ((nat>= 2 0) #t)
    ((nat>= 3 1) #t)
    ((nat>= 0 2) #f)
    ((nat>= 1 3) #f)
    ((nat>= 1 1) #t)))

(define ex-io-addlists
  '(((addlists '(1 2 3) '(4 5 6)) (5 7 9))
    ((addlists '(2 3) '(5 6)) (7 9))
    ((addlists '(3) '(6)) (9))
    ((addlists '() '()) ())
    ((addlists '(3) '()) ())
    ((addlists '() '(3)) ())))

(define ex-io-longer?
  '(((longer? '(a b c) '(a b)) #t)
    ((longer? '(b c) '(b)) #t)
    ((longer? '(c) '()) #t)
    ((longer? '() '()) #f)
    ((longer? '(a b) '(a b c)) #f)
    ((longer? '(b) '(b c)) #f)
    ((longer? '() '(c)) #f)
    ((longer? '(a b c) '(a b c)) #f)
    ((longer? '(1 2) '(5 6 7)) #f)
    ((longer? '(2) '(6 7)) #f)
    ((longer? '() '(7)) #f)))

(define ex-io-countdown
'(((countdown 4) (4 3 2 1 0))
  ((countdown 3) (3 2 1 0))
  ((countdown 2) (2 1 0))
  ((countdown 1) (1 0))
  ((countdown 0) (0))))

(define ex-io-deep-size
  '(((deep-size '()) 0)
    ((deep-size '(a b c)) 3)
    ((deep-size '(b c)) 2)
    ((deep-size '((a (b)) c d e)) 5)
    ((deep-size '(a (b))) 2)
    ((deep-size '(c d e)) 3)))

(define ex-io-even-only
  '(((even-only '(1 2 3 4)) (2 4))
    ((even-only '(2 3 4)) (2 4))
    ((even-only '(3 4)) (4))
    ((even-only '(4)) (4))
    ((even-only '(2 4 6 7)) (2 4 6))
    ((even-only '()) ())))

(define ex-io-deep-even-only
  '(((deep-even-only '(1 2 3 4)) (2 4))
    ((deep-even-only '(2 3 4)) (2 4))
    ((deep-even-only '(3 4)) (4))
    ((deep-even-only '(4)) (4))
    ((deep-even-only '(2 4 6 7)) (2 4 6))
    ((deep-even-only '()) ())
    ((deep-even-only '((3 4) 2 4 6 7)) ((4) 2 4 6))))

(define ex-io-fib
  '(((fib 0) 0)
    ((fib 1) 1)
    ((fib 2) 1)
    ((fib 3) 2)
    ((fib 4) 3)
    ((fib 5) 5)
    ((fib 6) 8)
    ((fib 7) 13)
    ))

(define recursion-scheme-nat01
  (let* ((rs recursion-scheme-nat)
         (rs (make-recursion-scheme
              (recursion-scheme-type rs)
              (lambda (x) `(< ,x 2)) ;;base-case?-expr
              (lambda (x) (< x 2)) ;;base-case?
              (recursion-scheme-base-value rs)
              (recursion-scheme-sub-fun-expr rs)
              (recursion-scheme-sub-fun rs))))
    rs))

(define recursion-pattern-nat01 (recursion-pattern-one-index recursion-scheme-nat01 0))

;; Can't solve this one.
(define ex-io-remove
  '(((remove '() 'a) ())
    ((remove '(a) 'a) ())
    ((remove '(b) 'a) (b))
    ((remove '(b a) 'a) (b))
    ((remove '(c b a) 'a) (c b))
    ((remove '(a b) 'a) (b))
    ((remove '(c a b) 'a) (c b))
    ((remove '(a c a b) 'a) (c b))
    ((remove '(a a c a b) 'a) (c b))
    ((remove '(d a a c a b) 'a) (d c b))))

(define ex-io-map
  '(((map (lambda (x) (+ x 1)) '(3 2 1)) (4 3 2))
    ((map (lambda (x) (+ x 1)) '(2 1)) (3 2))
    ((map (lambda (x) (+ x 1)) '(1)) (2))
    ((map (lambda (x) (+ x 1)) '()) ())
    ((map (lambda (x) (* x 3)) '(3 2 1)) (9 6 3))
    ((map (lambda (x) (* x 3)) '(2 1)) (6 3))
    ((map (lambda (x) (* x 3)) '(1)) (3))
    ((map (lambda (x) (* x 3)) '()) ())))

(define ex-io-filter
  '(((filter even? '(3 4 2 1)) (4 2))
    ((filter even? '(4 2 1)) (4 2))
    ((filter even? '(2 1)) (2))
    ((filter even? '(1)) ())
    ((filter even? '()) ())
    ((filter (lambda (x) (not (= x 0))) '(3 0 2 1 0)) (3 2 1))
    ((filter (lambda (x) (not (= x 0))) '(0 2 1 0)) (2 1))
    ((filter (lambda (x) (not (= x 0))) '(2 1 0)) (2 1))
    ((filter (lambda (x) (not (= x 0))) '(1 0)) (1))
    ((filter (lambda (x) (not (= x 0))) '(0)) ())
    ((filter (lambda (x) (not (= x 0))) '()) ())))

(define ex-io-compose-list
  '(((compose-list (list (lambda (x) (+ x 1)) (lambda (x) (* x 3)) (lambda (x) (+ x 2))) 10)
     37)
    ((compose-list (list (lambda (x) (* x 3)) (lambda (x) (+ x 2))) 10)
     36)
    ((compose-list (list (lambda (x) (+ x 2))) 10)
     12)
    ((compose-list (list) 10)
     10)
    ((compose-list (list) 1)
     1)))

(define all-synthesis-problems
  (list
   (list 'append ex-io-append)
   (list 'append123 ex-io-append123)
   (list 'stutter ex-io-stutter)
   (list 'even-length? ex-io-even-length?)
   (list 'length ex-io-length)
   (list 'length-bis ex-io-length-bis)
   (list 'factorial ex-io-factorial)
   (list 'flatten ex-io-flatten)
   (list 'sum ex-io-sum)
   (list 'same-length? ex-io-same-length?)
   (list 'nat> ex-io-nat>)
   (list 'nat>= ex-io-nat>=)
   (list ex-io-addlists'addlists )
   (list 'longer? ex-io-longer?)
   (list 'countdown ex-io-countdown)
   (list 'deep-size ex-io-deep-size)
   (list 'even-only ex-io-even-only)
   (list 'deep-even-only ex-io-deep-even-only)
   (list 'product ex-io-product)
   (list 'sentence ex-io-sentence)))
