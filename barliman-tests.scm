(test
  (barliman-filler 'foo '(x) '(((foo 1) (1))))
  '(((cons x '()))))

(test
  (barliman-filler
   'foo '(x f) '(((foo 1 list) ((1))) ((foo 2 (lambda (x) x)) 2)))
  '(((f (f x)))))

(test
  (length
   (barliman-partial-programs
    3
    'append '(xs ys)
    '(;;((append '() '()) ())
      ((append '(a) '(b)) (a b))
      ;;((append '(g) '(h)) (g h))
      ((append '(c d) '(e f)) (c d e f))
      ;;((append '(w x y z) '(1 2 3 4)) (w x y z 1 2 3 4))
      ))
   )
  3)


