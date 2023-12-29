;; Monte Carlo Markov Chain Synthesis
;; (a stand-alone experiment)
;; based on the Chapter Stochastic Search
;; of the draft book
;; Program Synthesis: The Book
;; by Loris d'Antoni and Nadia Polikarpova 

;; n := x | 0 | 1 | n + n | n * n | ite(b, n, n)
;; b := #f | #t | n = n | n < n | b or b | not b
(define ex-types
  '((n x y z 0 1 + * ite)
    (b #f #t = < or not)))

(define ex-ranks
  '((x)
    (y)
    (z)
    (0)
    (1)
    (+ n n)
    (* n n)
    (ite b n n)
    (#f)
    (#t)
    (= n n)
    (< n n)
    (or b b)
    (not b)))

(define (generate-tree-depth d ts rs t)
  (let ((os (cdr (assoc t ts))))
    (if (<= d 0)
        (choose (filter (lambda (o) (null? (cdr (assoc o rs)))) os))
        (let* ((o (choose os))
               (cs (cdr (assoc o rs)))
               (recs (map (lambda (t) (generate-tree-depth (- d 1) ts rs t)) cs))
               (res (if (null? recs) o (cons o recs))))
          res))))

(generate-tree-depth 5 ex-types ex-ranks 'n)

(define (random-partition n-partitions size)
  (if (= n-partitions 0)
      '()
      (let loop ((remaining-size (- size n-partitions))
                 (res (map (lambda (x) 1) (iota n-partitions))))
        (if (> remaining-size 0)
            (let ((i (random n-partitions)))
              (loop (- remaining-size 1)
                    (list-replace res i (add1 (list-ref res i)))))
            res))))

;;(random-partition 3 5)

(define (generate-tree size ts rs t)
  (let* ((os (cdr (assoc t ts)))
         (fos (filter (lambda (o) (<= (length (assoc o rs)) size)) os))
         (fos (if (> size 1) (filter (lambda (o) (< 1 (length (assoc o rs)))) fos) fos))
         (no (length fos))
         (o (if (= no 0) (car os) (list-ref fos (random no))))
         (cs (cdr (assoc o rs)))
         (nc (length cs))
         (ncs (random-partition nc (- size 1)))
         (recs (map (lambda (t n) (generate-tree n ts rs t)) cs ncs))
         (res (if (null? recs) o (cons o recs))))
    res))

(generate-tree 5 ex-types ex-ranks 'n)
(generate-tree 10 ex-types ex-ranks 'n)
(generate-tree 100 ex-types ex-ranks 'n)

(define (len x)
  (if (atom? x)
      1
      (length x)))

(define ex-t '(ite (= x 0) 0 (+ x 1)))

(define (node-indices-iter t l)
  (cons (reverse l)
        (if (atom? t)
            '()
            (apply
             append
             (map (lambda (x i) (node-indices-iter x (cons i l)))
                  (cdr t) (iota (length (cdr t))))))))

(define (node-indices t) (node-indices-iter t '()))
(node-indices ex-t)

(define (depth t)
  (+ 1 (if (atom? t)
           0
           (apply max (map depth (cdr t))))))
(depth ex-t)

(define (replace-node tree indices subtree)
  (if (null? indices)
      subtree
      (let ((i (add1 (car indices))))
        (list-replace
         tree i
         (replace-node (list-ref tree i) (cdr indices) subtree)))))

(replace-node ex-t (choose (node-indices ex-t)) '(* 1 0))

(define (tree-ref t indices)
    (if (null? indices)
        t
        (let ((i (add1 (car indices))))
          (tree-ref (list-ref t i) (cdr indices)))))

(define (type-of ts node)
  (let ((s (if (atom? node) node (car node))))
    (caar (filter (lambda (kv) (member s (cdr kv))) ts))))

(type-of ex-types ex-t)

(define (random-neighbour ts rs k e)
  (let* ((indices (choose (node-indices e)))
         (node (tree-ref e indices))
         (t (type-of ts node))
         (k2 (- k (depth node)))
         ;;(_ (assert (>= k2 0)))
         (node2 (generate-tree-depth k2 ts rs t)))
    (replace-node e indices node2)))

(random-neighbour ex-types ex-ranks 5 ex-t)

(define (ex-ev e args)
  (cond
    ((eq? 'x e) (list-ref args 0))
    ((eq? 'y e) (list-ref args 1))
    ((eq? 'z e) (list-ref args 2))
    ((atom? e) e)
    ((tagged-expr? '+ e)
     (+ (ex-ev (cadr e) args)
        (ex-ev (caddr e) args)))
    ((tagged-expr? '* e)
     (* (ex-ev (cadr e) args)
        (ex-ev (caddr e) args)))
    ((tagged-expr? 'ite e)
     (if (ex-ev (cadr e) args)
         (ex-ev (caddr e) args)
         (ex-ev (cadddr e) args)))
    ((tagged-expr? '= e)
     (= (ex-ev (cadr e) args)
        (ex-ev (caddr e) args)))
    ((tagged-expr? '< e)
     (< (ex-ev (cadr e) args)
        (ex-ev (caddr e) args)))
    ((tagged-expr? 'or e)
     (or (ex-ev (cadr e) args)
         (ex-ev (caddr e) args)))
    ((tagged-expr? 'not e)
     (not (ex-ev (cadr e) args)))
    (else (error 'ex-ev "unknown expression" e))))

(define (evs ev io* e)
  (let ((diff (filter
               (lambda (io)
                 (let ((args (io-args io)))
                   (not (equal? (ev e args) (io-output io)))))
               io*)))
    (if (null? diff)
        #f
        (/ 1.0 (length diff)))))

(define ex-io* '(((f 0) 1) ((f 1) 2) ((f 2) 3)))

(define (remq-case x xss)
  (map (lambda (xs) (remq x xs)) xss))

(define (stochastic-search k ts rs ev io* initial-program)
  (let* ((argc (length (io-args (car io*))))
         (ts (if (< argc 2) (remq-case 'y ts) ts))
         (ts (if (< argc 3) (remq-case 'z ts) ts))

         (e initial-program)
         (q (evs ev io* e)))
    (let loop ((e e) (q q))
      (if q
          (let* ((e2 (random-neighbour ts rs k e))
                 (q2 (evs ev io* e2)))
            (if (or (not q2) (> q2 q))
                (loop e2 q2)
                (let ((p (/ q2 q)))
                  (if (< (random 1.0) p)
                      (loop e2 q2)
                      (loop e q)))))
          e))))

(stochastic-search 4 ex-types ex-ranks ex-ev ex-io* 0)

(define ex-io2* '(((f 0) 2) ((f 1) 3) ((f 2) 4)))

(stochastic-search 4 ex-types ex-ranks ex-ev ex-io2* 0)

(define ex-io3* '(((f 1 2) 2) ((f 2 1) 2)
                  ((f 2 3) 3) ((f 3 2) 3)))

(stochastic-search 4 ex-types ex-ranks ex-ev ex-io3* 0)
