(define (io-input io) (car io))
(define (io-args io) (cdar io))
(define (io-output io) (cadr io))

(define (choose lst)
  (list-ref lst (random (length lst))))

(define (read-from-string s)
  (read (open-string-input-port s)))

(define (capture-output thunk)
  (parameterize ([current-output-port (open-output-string)])
    (let* ((thunk-result (thunk))
           (output-string (get-output-string (current-output-port))))
      (values thunk-result output-string))))

;; short-circuiting
(define (all predicate . args)
  (assert (not (null? args)))
  (if (null? (filter null? args))
      (and (apply predicate (map car args))
           (apply all predicate (map cdr args)))
      (begin
        (map (lambda (x) (assert (null? x))) args)
        #t)))

(define (negate predicate)
  (lambda args (not (apply predicate args))))

(define (some predicate . args)
  (not (apply all (negate predicate) args)))

(define (list-replace xs i v)
  (if (null? xs)
      '()
      (if (= i 0)
          (cons v (cdr xs))
          (cons (car xs) (list-replace (cdr xs) (- i 1) v)))))

;; Quote values, except for self-evaluating literals.
(define (maybe-quote x)
  (cond
    ((number? x) x)
    ((boolean? x) x)
    (else `(quote ,x))))

(define (assoc-get default key map)
  (let ((r (assoc key map)))
    (if r
        (cdr r)
        default)))

(define (unique l)
  (if (null? l)
      '()
      (let ((a (car l)))
        (cons a (unique (filter (lambda (b) (not (equal? a b))) (cdr l)))))))

(define (all-same xs)
  (cond ((null? xs)
         #f)
        ((null? (cdr xs))
         (car xs))
        ((equal? (car xs) (car (cdr xs)))
         (all-same (cdr xs)))
        (else #f)))
