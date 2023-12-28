(module
 template
 (var? unify walk* walk ext-s-check subst-add subst-lookup unbound? occurs-check)

 (define var?
   (lambda (x)
     (and
      (symbol? x)
      (let ((s (symbol->string x)))
        (and (> (string-length s) 0)
             (char=? (string-ref s 0) #\?))))))

 (define var-eq? eq?)

 (define (unify u v s)
   (let ((u (walk u s))
         (v (walk v s)))
     (cond
       ((eq? u v) (values s '()))
       ((and (var? u) (var? v))
        (ext-s-check u v s))
       ((var? u) (ext-s-check u v s))
       ((var? v) (ext-s-check v u s))
       ((and (pair? u) (pair? v))
        (let-values (((s added-car) (unify (car u) (car v) s)))
          (if s
              (let-values (((s added-cdr) (unify (cdr u) (cdr v) s)))
                (values s (append added-car added-cdr)))
              (values #f #f))))
       ((equal? u v) (values s '()))
       (else (values #f #f)))))

 (define (walk u S)
   (let rec ((u u))
     (if (var? u)
         (let ((val (subst-lookup u S)))
           (if (unbound? val)
               u
               (rec val)))
         u)))

 (define (occurs-check x v S)
   (let ((v (walk v S)))
     (cond
       ((var? v) (var-eq? v x))
       ((pair? v)
        (or (occurs-check x (car v) S)
            (occurs-check x (cdr v) S)))
       (else #f))))

 (define (ext-s-check x v S)
   (if (occurs-check x v S)
       (values #f #f)
       (values (subst-add S x v) (list (cons x v)))))

 (define (subst-add S x v)
   (cons (cons x v) S))

 (define (subst-lookup u S)
   (let ((res (assq u S)))
     (if res
         (cdr res)
         unbound)))

 (define unbound (list 'unbound))
 (define (unbound? v) (eq? v unbound))

 (define (walk* v S)
   (let ((v (walk v S)))
     (cond
       ((var? v) v)
       ((pair? v)
        (cons (walk* (car v) S) (walk* (cdr v) S)))
       (else v))))
 )
