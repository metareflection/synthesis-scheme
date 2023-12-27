(define lambda-params cadr)
(define lambda-body caddr)

;; (letrec ((name (lambda (x) e))) body)
(define letrec-body caddr)
(define (letrec-name e)
  (let ((bs (cadr e)))
    ;; TODO: support multiple bindings like here:
    ;;  https://github.com/webyrd/Barliman/blob/master/cocoa/Barliman/mk-and-rel-interp/interp-simple.scm
    (assert (null? (cdr bs)))
    (caar bs)))
(define (letrec-lambda e) (cadr (car (cadr e))))

(define (tagged-expr? sym expr)
  (and (pair? expr) (eq? (car expr) sym)))

(define (add-binding x v)
  (cons x (cons 'val v)))

(define-record-type closure (fields formals body env))

(define-record-type primitive (fields name proc))

(define (add-binding-prim x v)
  (add-binding x (make-primitive x v)))

(define (apply-struct? x)
  (or (primitive? x)
      (closure? x)))

(define (can-apply? f args)
  (and (list? args)
       (or (primitive? f)
           (and (closure? f)
                (let ((params (closure-formals f)))
                  (or (symbol? params)
                      (= (length params) (length args))))))))

(define (my-apply f args)
  (cond
    ((primitive? f)
     (apply (primitive-proc f) args))
    ((closure? f)
     (eval-expr
      (closure-body f)
      (let ((params (closure-formals f)))
        (append
         (if (symbol? params)
             (list (add-binding params args))
             (map add-binding params args))
         (closure-env f)))))
    (error 'my-apply "expected procedure" f)))

(define global-env
  (list
   (add-binding-prim '+ +)
   (add-binding-prim '* *)
   (add-binding-prim '- -)
   (add-binding-prim '= =)
   (add-binding-prim 'zero? zero?)
   (add-binding-prim 'add1 add1)
   (add-binding-prim 'sub1 sub1)
   (add-binding-prim 'equal? equal?)
   (add-binding-prim 'eq? eq?)
   (add-binding-prim 'cons cons)
   (add-binding-prim 'car car)
   (add-binding-prim 'cdr cdr)
   (add-binding-prim 'list list)
   (add-binding-prim 'append append)
   (add-binding-prim 'null? null?)
   (add-binding-prim 'pair? pair?)
   (add-binding-prim 'not not)
   (add-binding-prim '< <)
   (add-binding-prim '> >)
   (add-binding-prim '<= <=)
   (add-binding-prim '>= >=)
   (add-binding-prim 'length length)
   (add-binding-prim 'list? list?)
   (add-binding-prim 'cddr cddr)
   (add-binding-prim 'positive? positive?)
   (add-binding-prim 'negative? negative?)
   (add-binding-prim 'even? even?)
   (add-binding-prim 'odd? odd?)
   (add-binding-prim 'apply my-apply)
   ))

(define (eval-expr expr env)
  (cond
    ((boolean? expr)
     expr)
    ((number? expr)
     expr)
    ((symbol? expr)
     (lookup expr env))
    ((tagged-expr? 'quote expr)
     (cadr expr))
    ((tagged-expr? 'if expr)
     (if (eval-expr (cadr expr) env)
         (eval-expr (caddr expr) env)
         (eval-expr (cadddr expr) env)))
    ((tagged-expr? 'cond expr)
     (cond
       ((null? (cdr expr))
        (error 'eval-expr "cond expression with no cases"))
       ((eq? (caadr expr) 'else)
        (eval-expr (cadadr expr) env))
       (else
        (eval-expr
         `(if ,(caadr expr)
              ,(cadadr expr)
              (cond ,@(cddr expr)))
         env))))
    ((tagged-expr? 'and expr)
     (if (null? (cdr expr))
         #t
         (and (eval-expr (cadr expr) env)
              (eval-expr `(and ,@(cddr expr)) env))))
    ((tagged-expr? 'or expr)
     (if (null? (cdr expr))
         #f
         (or (eval-expr (cadr expr) env)
             (eval-expr `(or ,@(cddr expr)) env))))
    ((tagged-expr? 'assert expr)
     (let ((r (eval-expr (cadr expr) env)))
       (if r
           r
           (error 'eval-expr "assertion failed" expr))))
    ((tagged-expr? 'lambda expr)
     (make-my-closure expr env))
    ((tagged-expr? 'let expr)
     (let ((bindings (cadr expr))
           (body (caddr expr)))
       (eval-expr
        body
        (append
         (map (lambda (b) (add-binding (car b) (eval-expr (cadr b) env))) bindings)
         env))))
    ((tagged-expr? 'letrec expr)
     (eval-expr
      (letrec-body expr)
      (cons (cons (letrec-name expr) (cons 'rec (letrec-lambda expr))) env)))
    ((pair? expr)
     (let ((f (eval-expr (car expr) env))
           (args (map (lambda (e) (eval-expr e env)) (cdr expr))))
       (my-apply f args)))
    (else (error 'eval-expr "unexpected expression" expr))))

(define (lookup x env)
  (let ((r (assq x env)))
    (if r
        (let ((b (cdr r)))
          (cond
            ((tagged-expr? 'val b)
             (cdr b))
            ((tagged-expr? 'rec b)
             (make-my-closure (cdr b) env))
            (else (error 'lookup "unknown binding" b))))
        (error 'lookup "unbound variable" x))))

(define (my-eval expr)
  (eval-expr expr global-env))

(define (make-my-closure expr env)
  (make-closure
   (lambda-params expr)
   (lambda-body expr)
   env))

;; (define f (lambda (x) e))
;; (define (f x) e)
(define (define-name expr)
  (let ((name-part (cadr expr)))
    (if (symbol? name-part)
        name-part
        (car name-part))))

(define (define-lambda expr)
  (let ((name-part (cadr expr)))
    (if (symbol? name-part)
        (caddr expr)
        `(lambda ,(cdr name-part) ,(caddr expr)))))

(define (define-to-letrec define-expr body-expr)
  `(letrec ((,(define-name define-expr)
             ,(define-lambda define-expr)))
     ,body-expr))
