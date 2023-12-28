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
  (synthesize-sketch 'last 1 '(xs) '(((last '(x)) x) ((last '(x y)) y) ((last '(x y z)) z)) '(if (null? (cdr xs)) (car xs) _.0))
  '(((if (null? (cdr xs)) (car xs) (last (cdr xs))))))

(todo
 "last"
 (synthesize 'last 1 '(xs) '(((last '(x)) x) ((last '(x y)) y)))
 'TODO)

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
