(define (make-piece expr size deno)
  (cons 'piece (list (cons 'expr expr) (cons 'size size) (cons 'deno deno))))
(define (piece-expr x) (cdr (car (cdr x))))
(define (piece-size x) (cdr (cadr (cdr x))))
(define (piece-deno x) (cdr (caddr (cdr x))))

(define-record-type op (fields name applicable? compute-expr compute-val arity))

(define ops
  (list
   (make-op
    'cons
    (lambda (v1 v2) #t)
    (lambda (e1 e2) `(cons ,e1 ,e2))
    (lambda (v1 v2) (cons v1 v2))
    2)
   (make-op
    'car
    (lambda (v) (pair? v))
    (lambda (e) `(car ,e))
    car
    1)
   (make-op
    'cdr
    (lambda (v) (pair? v))
    (lambda (e) `(cdr ,e))
    cdr
    1)
   (make-op
    'not
    boolean?
    (lambda (e) `(not ,e))
    not
    1)
   (make-op
    'add1
    number?
    (lambda (e) `(add1 ,e))
    add1
    1)
   (make-op
    '+
    (lambda (v1 v2) (and (number? v1) (number? v2)))
    (lambda (e1 e2) `(+ ,e1 ,e2))
    (lambda (v1 v2) (+ v1 v2))
    2)
   (make-op
    '*
    (lambda (v1 v2) (and (number? v1) (number? v2)))
    (lambda (e1 e2) `(* ,e1 ,e2))
    (lambda (v1 v2) (* v1 v2))
    2)
   (make-op
    'append
    (lambda (v1 v2) (and (list? v1) (list? v2)))
    (lambda (e1 e2) `(append ,e1 ,e2))
    append
    2)
   (make-op
    'apply1
    (lambda (v1 v2) (can-apply? v1 (list v2)))
    list
    (lambda (v1 v2) (my-apply DEFAULT_FUEL (lambda (x) x) v1 (list v2)))
    2)
   (make-op
    'apply
    can-apply?
    (lambda (e1 e2) `(apply ,e1 ,e2))
    (lambda (v1 v2) (my-apply DEFAULT_FUEL (lambda (x) x) v1 v2))
    2)))

(define (fill-bottomup fun-name formals io* . options)
  (let* ((base-case? (assoc-get #f 'base-case? options))
         (syn-name (assoc-get #f 'fun-name options))
         (ops (if syn-name
                  (filter (lambda (op) (not (eq? (op-name op) syn-name)))
                          ops)
                  ops)))
    (let ((outputs (map io-output io*)))
      (list ;; to match Barliman
       (list
        (if (and (not (null? outputs))
                 (or base-case? (not (null? (cdr outputs))))
                 (all (lambda (v) (equal? v (car outputs))) outputs))
            (maybe-quote (car outputs))
            (let-values (((p piecess) (apply bottomup ops formals io* options)))
              (piece-expr p))))))))

(define (bottomup ops formals io* . options)
  (let* ((inputs (map (lambda (io) (map my-eval (io-args io))) io*))
         (outputs (map io-output io*))
         (input-pieces
          (map
           (lambda (i formal)
             (make-piece
              formal
              1
              (map (lambda (input) (list-ref input i)) inputs)))
           (iota (length formals)) formals))
         (constant-pieces
          (map
           (lambda (e)
             (let ((v (my-eval e)))
               (make-piece
                e
                1
                (map (lambda (_) v) outputs))))
           (unique
            (cons ''()
                  (if (null? (cdr outputs))
                      '()
                      (map maybe-quote (filter number? (unique outputs)))))))))
    (let ((piecess (apply bottomup-iter ops outputs 1 (list '() (append input-pieces constant-pieces)) options)))
      (values (get-deno outputs (last piecess)) piecess))))

(define (has-deno deno pieces)
  (not (null? (filter
               (lambda (piece)
                 (equal? deno (piece-deno piece)))
               pieces))))

(define (get-deno deno pieces)
  (car (filter
        (lambda (piece)
          (equal? deno (piece-deno piece)))
        pieces)))

(define (applicable-op? op . ps)
  (apply
   all
   (lambda args (apply (op-applicable? op) args))
   (map piece-deno ps)))

(define (apply-op op . ps)
  (let ((vs
         (guard (x (else #f))
                (list (apply map (op-compute-val op)
                             (map piece-deno ps))))))
    (and vs
         (make-piece
          (apply (op-compute-expr op)
                 (map piece-expr ps))
          (apply + 1
                 (map piece-size ps))
          (car vs)))))

(define (first-n n xs)
  (if (= n 0)
      '()
      (begin
        (assert (not (null? xs)))
        (cons (car xs) (first-n (- n 1) (cdr xs))))))

(define (all-piece-args arity size piecess)
  (cond
    ((= 0 arity) '(()))
    ((<= size 0) '())
    ((= 1 arity)
     (map list (list-ref piecess (- size 1))))
    (else
     (apply
      append
      (map
       (lambda (i pieces)
         (let ((rec (all-piece-args (- arity 1) (- size i) piecess)))
           (apply
            append
            (map
             (lambda (piece)
               (map
                (lambda (rest) (cons piece rest))
                rec))
             pieces))))
       (cdr (iota size)) (cdr (first-n size piecess)))))))

(define (compute-new-pieces ops new-size piecess)
  (filter
   (lambda (x) x)
   (apply
    append
    (map
     (lambda (op)
       (map
        (lambda (piece-args)
          (and (apply applicable-op? op piece-args)
               (apply apply-op op piece-args)))
        (all-piece-args (op-arity op) new-size piecess)))
     ops))))

(define (last xs)
  (if (null? (cdr xs))
      (car xs)
      (last (cdr xs))))

(define (equal-denos? p1 p2)
  (equal? (piece-deno p1) (piece-deno p2)))

(define (unique-denos pieces)
  (cond
    ((null? pieces)
     '())
    ((some (lambda (p) (equal-denos? p (car pieces))) (cdr pieces))
     (unique-denos (cdr pieces)))
    (else
     (cons (car pieces) (unique-denos (cdr pieces))))))

(define (without-denos piecess new-pieces)
  (filter
   (lambda (p)
     (not (some
           (lambda (pieces)
             (some (lambda (old-p) (equal-denos? old-p p)) pieces))
           piecess)))
   new-pieces))

(define (bottomup-iter ops outputs size piecess . options)
  (if (has-deno outputs (last piecess))
      piecess
      (let* ((conditional? (assoc-get #f 'conditional? options))
             (r (if conditional?
                    (find-conditional outputs (apply append piecess))
                    #f)))
          (if r
              (list (list r))
              (let* ((new-size (+ 1 size))
                     (new-pieces (compute-new-pieces ops new-size piecess))
                     (unique-new-pieces
                      (without-denos piecess (unique-denos new-pieces)))
                     (new-piecess (append piecess (list unique-new-pieces))))
                (apply bottomup-iter ops outputs new-size
                       new-piecess options))))))

(define (find-conditional outputs pieces)
  (let ((rs
         (filter
          (lambda (x)
            (and (not (null? (cadr x)))
                 (not (null? (caddr x)))))
          (map
           (lambda (cond-piece)
             (let* ((cond-denot (piece-deno cond-piece))
                    (find-pieces (lambda (p)
                                   (filter
                                    (lambda (piece)
                                      (all (lambda (c x o) (if (p c) (equal? x o) #t))
                                           cond-denot
                                           (piece-deno piece)
                                           outputs))
                                    pieces))))
               (list cond-piece (find-pieces (lambda (x) x)) (find-pieces not))))
           (filter
            (lambda (piece) (member #f (piece-deno piece)))
            pieces)))))
    (if (null? rs)
        #f
        (let* ((r (car rs))
               (cond-piece (car r))
               (then-piece (car (cadr r)))
               (else-piece (car (caddr r))))
          (make-piece
           `(if ,(piece-expr cond-piece)
                ,(piece-expr then-piece)
                ,(piece-expr else-piece))
           (+ 1
              (piece-size cond-piece)
              (piece-size then-piece)
              (piece-size else-piece))
           outputs)))))

(define (compute-io* f fun-name args*)
  (map
   (lambda (args)
     `((,fun-name . ,args) ,(apply f args)))
   args*))
