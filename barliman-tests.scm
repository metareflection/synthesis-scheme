(test
  (barliman-filler 'foo '(x) '(((foo 1) (1))))
  '(((cons x '()))))

(test
  (length
   (barliman-partial-programs
    3
    'append '(xs ys)
    '(((append '() '()) ())
      ((append '(a) '(b)) (a b))
      ((append '(g) '(h)) (g h))
      ((append '(c d) '(e f)) (c d e f))
      ((append '(w x y z) '(1 2 3 4)) (w x y z 1 2 3 4)))))
  3)


