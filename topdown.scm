(define (make-hole)
  '_.0)

(define (make-holes arity)
  (map (lambda (_) (make-hole)) (iota arity)))

(define arities
  (list
   (list 'null? 1)
   (list 'cons 2)
   (list 'car 1)
   (list 'cdr 1)
   (list 'if 3)))

(define (fill-hole fun-name arity formals)
  (append
   formals
   (map (lambda (fa) (cons (car fa) (make-holes (cadr fa)))) arities)
   (list (cons fun-name (make-holes arity)))))

(define DONE '(DONE))

(define (next-steps fun-name arity formals e)
  (cond
    ((hole? e)
     (fill-hole fun-name arity formals))
    ((pair? e)
     (let ((sa (next-steps fun-name arity formals (car e))))
       (if (eq? DONE sa)
           (let ((sd (next-steps fun-name arity formals (cdr e))))
             (if (eq? DONE sd)
                 DONE
                 (map (lambda (d) (cons (car e) d)) sd)))
           (map (lambda (a) (cons a (cdr e))) sa))))
    (else DONE)))

(define (pick-candidate candidates)
  (values (car candidates) (cdr candidates)))

(define (candidate-expression c)
  (car c))
(define (candidate-score c)
  (cadr c))

(define run-until-ticks-values
  (lambda (max-ticks thunk)
    ((make-engine thunk)
     max-ticks
     (lambda (ticks value)
       (values value (- max-ticks ticks) #t))
     (lambda (new-eng)
       (values #f max-ticks #f)))))

(define (evaluate-score fun-name arity formals io* e)
  (call/cc
   (lambda (k)
     (length
      (filter
       (lambda (r) (eq? #t r))
       (map (lambda (io)
              (let ((input (car io))
                    (expected (cadr io)))
                (let ((thunk (lambda ()
                               (my-eval `(letrec ((,fun-name (lambda ,formals ,e)))
                                           ,input)))))
                  (guard
                   (x (else (k -2)))
                   (let-values (((result ticks completed?)
                                 (run-until-ticks-values 100000000 thunk)))
                     (if completed?
                         (if (eq? result HOLE)
                             HOLE
                             (equal? result expected))
                         (k -1)))))))
            io*))))))

(define (merge-candidates fun-name arity formals io* next-expressions other-candidates)
  (let ((next-candidates (map (lambda (e) (list e (evaluate-score fun-name arity formals io* e))) next-expressions)))
    (list-sort (lambda (es1 es2) (> (cadr es1) (cadr es2)))
               (append other-candidates next-candidates))))

(define (synthesize-iter fun-name arity formals io* candidates)
  ;;(printf "Candidates ~a.\n" candidates)
  (if (null? candidates)
      #f
      (let-values (((best-candidate other-candidates)
                    (pick-candidate candidates)))
        ;;(printf "Considering ~a.\n" best-candidate)
        (let ((next-expressions (next-steps fun-name arity formals (candidate-expression best-candidate))))
          (if (eq? DONE next-expressions)
              (if (= (length io*) (candidate-score best-candidate))
                  (list (list (candidate-expression best-candidate)))
                  (synthesize-iter fun-name arity formals io* other-candidates))
              (synthesize-iter fun-name arity formals io* (merge-candidates fun-name arity formals io* next-expressions other-candidates)))))))

(define (synthesize-sketch fun-name arity formals io* sketch)
  (synthesize-iter
   fun-name arity formals io*
   (list (list sketch (evaluate-score fun-name arity formals io* sketch)))))

(define (synthesize fun-name arity formals io*)
  (synthesize-iter fun-name arity formals io* (list (list (make-hole) 0))))
