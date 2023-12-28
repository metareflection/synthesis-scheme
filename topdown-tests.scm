(test
  (synthesize 'id 1 '(x) '(((id 1) 1) ((id 2) 2)))
  '((x)))

(test
  (synthesize 'foo 3 '(a b c) '(((foo #t 1 2) 1) ((foo #f 1 2) 2)))
  '(((if a b c))))

(test
  (synthesize-sketch 'last 1 '(xs) '(((last '(x)) x) ((last '(x y)) y)) '(if (null? (cdr xs)) (car _.0) (last (cdr xs))))
  '(((if (null? (cdr xs)) (car xs) (last (cdr xs))))))

(test
  (synthesize-sketch 'last 1 '(xs) '(((last '(x)) x) ((last '(x y)) y)) '(if (null? _.0) (car xs) (last (cdr xs))))
  '(((if (null? (cdr xs)) (car xs) (last (cdr xs))))))

(test
  (synthesize-sketch 'last 1 '(xs) '(((last '(x)) x) ((last '(x y)) y)) '(if (null? (cdr xs)) (car xs) (last _.0)))
  '(((if (null? (cdr xs)) (car xs) (last (cdr xs))))))

(test
  (synthesize-sketch 'last 1 '(xs) '(((last '(x)) x) ((last '(x y)) y) ((last '(x y z)) z)) '(if (null? (cdr xs)) (car xs) _.3))
  '(((if (null? (cdr xs)) (car xs) (last (cdr xs))))))

(test
 (synthesize-sketch 'last 1 '(xs) '(((last '(x)) x) ((last '(x y)) y) ((last '(x y z)) z)) '(if (null? _.1) _.2 (last (cdr xs))))
 '(((if (null? (cdr xs)) (car xs) (last (cdr xs))))))

(test
 (synthesize 'last 1 '(xs) '(((last '(x)) x) ((last '(x y)) y) ((last '(x y z)) z) ((last '(x y z a)) a)))
 '(((if (null? (cdr xs)) (car xs) (last (cdr xs))))))

(test
 (synthesize 'repeat 2 '(c xs) '(((repeat 'a '()) ()) ((repeat 'a '(1 2 3)) (a a a))))
 '(((if (null? xs) xs (cons c (repeat c (cdr xs))))))
 )

(test
  (synthesize-sketch
   'append 2 '(xs ys)
   '(((append '() '()) ())
     ((append '(a) '(b)) (a b))
     ((append '(g) '(h)) (g h))
     ((append '(c d) '(e f)) (c d e f))
     ((append '(w x y z) '(1 2 3 4)) (w x y z 1 2 3 4))
     )
   '(if (null? xs) _.2 (cons (car xs) (append (cdr xs) ys))))
  '(((if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))

(todo
 "append"
  (synthesize
   'append 2 '(xs ys)
   '(((append '() '()) ())
     ((append '(a) '(b)) (a b))
     ((append '(g) '(h)) (g h))
     ((append '(c d) '(e f)) (c d e f))
     ((append '(w x y z) '(1 2 3 4)) (w x y z 1 2 3 4))
     ))
  'TODO)
