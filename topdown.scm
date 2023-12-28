(define rollout? #f)

(define (make-hole i)
  (string->symbol (string-append "_." (number->string i))))

(define (make-holes arity)
  (map (lambda (i) (make-hole (+ 1 i))) (iota arity)))

(define arities
  (list
   (list 'null? 1)
   (list 'car 1)
   (list 'cdr 1)
   (list 'cons 2)
   (list 'if 3)))

(define (grams parent hole)
  (cond
    ((eq? parent 'null?)
     (list 'cdr))
    ((eq? parent 'car)
     (list 'car 'cdr))
    ((eq? parent 'cdr)
     (list 'car 'cdr))
    ((and (eq? parent 'if) (eq? hole '_.1))
     (list 'null?))
    ((and (eq? parent 'if) (eq? hole '_.2))
     (list 'car 'cdr 'cons))
    ((and (eq? parent 'if) (eq? hole '_.3))
     (list 'cons 'cdr 'car))
    ((and (eq? parent 'cons) (eq? hole '_.1))
     (list 'car 'cons))
    ((and (eq? parent 'cons) (eq? hole '_.2))
     (list 'car 'cons))
    (else #f)))

(define (grams-arities all? parent hole)
  (let ((ps (grams parent hole)))
    (if ps
        (map (lambda (p) (assq p arities)) ps)
        (if all? arities '()))))

(define (fill-hole hole parent fun-name arity formals)
  (append
   formals
   (map (lambda (fa) (cons (car fa) (make-holes (cadr fa))))
        (grams-arities #t parent hole))
   (list (cons fun-name (make-holes arity)))))

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

(define DONE '(DONE))

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

(define (next-steps fun-name arity formals e)
  (next-steps-f
   #f
   (lambda (hole parent) (fill-hole hole parent fun-name arity formals))
   fun-name arity formals e))

(define (next-steps-f parent thunk fun-name arity formals e)
  (cond
    ((hole? e)
     (thunk e parent))
    ((pair? e)
     (list-next-steps-f (car e) thunk fun-name arity formals e))
    (else DONE)))

(define (list-next-steps-f parent thunk fun-name arity formals e)
  (if (null? e)
      DONE
      (let ((sa (next-steps-f parent thunk fun-name arity formals (car e))))
        (if (eq? DONE sa)
            (let ((sd (list-next-steps-f parent thunk fun-name arity formals (cdr e))))
              (if (eq? DONE sd)
                  DONE
                  (map (lambda (d) (cons (car e) d)) sd)))
           (map (lambda (a) (cons a (cdr e))) sa)))))

(define (compare-candidates es1 es2)
  (let ((s1 (cadr es1))
        (s2 (cadr es2)))
    (or
     (> s1 s2)
     (and
      rollout?
      (>= s1 0)
      (>= s2 0)
      (= s1 s2)
      (> (force (caddr es1)) (force (caddr es2)))))))

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

(define run-until-ticks-values
  (lambda (max-ticks thunk)
    ((make-engine thunk)
     max-ticks
     (lambda (ticks value)
       (values value (- max-ticks ticks) #t))
     (lambda (new-eng)
       (values #f max-ticks #f)))))

(define (evaluate-score fail? fun-name arity formals io* e)
  (call/cc
   (lambda (k)
     (apply +
      (map
       (lambda (r) (if (eq? #t r) 1 (if (and (not fail?) (eq? #f r)) -1 0)))
       (map (lambda (io)
              (let ((input (car io))
                    (expected (cadr io)))
                (let ((thunk (lambda ()
                               (my-eval `(letrec ((,fun-name (lambda ,formals ,e)))
                                           ,input)))))
                  (guard
                   (x (else (k -3)))
                   (let-values (((result ticks completed?)
                                 (run-until-ticks-values 100000 thunk)))
                     (if completed?
                         (if (eq? result HOLE)
                             HOLE
                             (if (equal? result expected)
                                 #t
                                 (if fail? (k -2) #f)))
                         (k -1)))))))
            io*))))))

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
