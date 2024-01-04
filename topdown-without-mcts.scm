(define dan? #t)
(define rollout? #f)

(define (inc-random-choice xs)
  (if (null? (cdr xs))
      (car xs)
      (if (= (random 1) 0)
          (car xs)
          (inc-random-choice (cdr xs)))))

(define (random-expressions hole parent fun-name arity formals)
  (let ((thunk
         (lambda ()
           (inc-random-choice
            (append
             formals
             (map (lambda (fa) (cons (car fa)
                                (map (lambda (_) (inc-random-choice formals))
                                     (iota (cadr fa)))))
                  (grams-arities #f parent hole)))))))
    (list
     (thunk)
     ;;(thunk)
     )))

(define (fill-all-holes fun-name arity formals e)
  ;;(printf "Considering ~a.\n" e)
  (let ((r (next-steps-f
            #f
            (lambda (hole parent) (random-expressions hole parent fun-name arity formals))
            fun-name arity formals e)))
    (if (eq? DONE r)
        (list e)
        (apply append
               (map (lambda (e) (fill-all-holes fun-name arity formals e)) r)))))

(define (compare-candidates es1 es2)
  (let ((s1 (cadr es1))
        (s2 (cadr es2)))
    (or
     (> s1 s2)
     (and
      dan?
      (>= s1 0)
      (>= s2 0)
      (= s1 s2)
      (let ((t1 (force (caddr es1)))
            (t2 (force (caddr es2))))
        (or
         (> t1 t2)
         (and
          rollout?
          (= t1 t2)
          (> (force (cadddr es1)) (force (cadddr es2))))))))))

(define (find-best candidates so-far)
  (if (null? candidates)
      so-far
      (find-best
       (cdr candidates)
       (if (compare-candidates (car candidates) so-far)
           (car candidates)
           so-far))))

(define (pick-candidate candidates)
  (let ((x (find-best (cdr candidates) (car candidates))))
    (values x (remq x candidates))))

(define (candidate-expression c)
  (car c))
(define (candidate-score c)
  (cadr c))

(define (rollout fun-name arity formals io* e)
  (let ((r (fill-all-holes fun-name arity formals e)))
    (/
     (apply
      +
      (filter
       (lambda (x) (> x 0))
       (map (lambda (e) (evaluate-score #f fun-name arity formals io* e)) r)))
     (length r))))

(define (merge-candidates fun-name arity formals io* next-expressions other-candidates)
  (let* ((next-candidates
          (map (lambda (e)
                 (list e
                       (evaluate-score #t fun-name arity formals io* e)
                       (delay (compute-dan-score fun-name arity formals e))
                       (delay (rollout fun-name arity formals io* e))))
               next-expressions))
         (next-candidates
          (filter (lambda (es) (>= (cadr es) 0)) next-candidates)))
    (append other-candidates next-candidates)))

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
   (list (list sketch (evaluate-score #t fun-name arity formals io* sketch)))))

(define (synthesize fun-name arity formals io*)
  (synthesize-iter fun-name arity formals io* (list (list (make-hole 0) 0))))
