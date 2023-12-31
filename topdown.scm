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

(define DONE '(DONE))

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
(define EXCEPTION '(EXCEPTION))

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
                               (guard
                                (x (else EXCEPTION))
                                (my-eval `(letrec ((,fun-name (lambda ,formals ,e)))
                                            ,input))))))
                  (let ((result (thunk))) ;; TODO: deal with timeout
                    (if (eq? result EXCEPTION)
                        (k -3)
                        (if (eq? result HOLE)
                            HOLE
                            (if (equal? result expected)
                                #t
                                (if fail? (k -2) #f))))))))
            io*))))))

(define (mcts-synthesize fun-name arity formals io* sketch . options)
  (let ((expansion-count (assoc-get #f 'expansion-count options))
        (n (length io*)))
    (define (child-finder node montecarlo)
      (let ((es (next-steps fun-name arity formals (node-state node))))
        (if (eq? DONE es)
            (if (= n (evaluate-score #t fun-name arity formals io* (node-state node)))
                (begin
                  (node-update-win-value node 1.0)
                  (montecarlo-solution-set! montecarlo (node-state node)))
                (node-update-win-value node -1.0))
            (let* ((nonnegative-cvs
                    (filter
                     (lambda (cv) (or (not (cdr cv)) (>= (cdr cv) 0.0)))
                     (map
                      (lambda (e)
                        (let ((child (node-new e)))
                          (cons child (node-evaluator child montecarlo))))
                      es)))
                   ;;(min-score (apply min 1.0 (filter (lambda (x) x) (map cdr nonnegative-cvs))))
                   (children
                    (map
                     (lambda (cv)
                       (if (and (cdr cv) (> (cdr cv) 0.0))
                           (node-policy-value-set! (car cv) (cdr cv)))
                       (car cv))
                     nonnegative-cvs)))
              (if (null? children)
                  (node-update-win-value node -1.0)
                  (node-add-children node children))))))
    (define (node-evaluator node montecarlo)
      (let ((v (evaluate-score #t fun-name arity formals io* (node-state node))))
        (if (= v 0)
            0.0;; -- don't do rollouts: #f
            (if (< v 0)
                -1.0
                (exact->inexact (/ v n))))))
    (let ((montecarlo (montecarlo-new (node-new sketch))))
      (montecarlo-child-finder-set! montecarlo child-finder)
      (montecarlo-node-evaluator-set! montecarlo node-evaluator)
      (montecarlo-simulate montecarlo expansion-count)
      (let ((solution (montecarlo-solution montecarlo)))
        (if solution
            (list (list solution))
            #f)))))

(define (synthesize-sketch fun-name arity formals io* sketch . options)
  (apply mcts-synthesize fun-name arity formals io* sketch options))

(define (synthesize fun-name arity formals io* . options)
  (apply mcts-synthesize fun-name arity formals io* (make-hole 0) options))
