(let ()
  (import template)
  (test #t (var? '?x))
  (test '((?x . hello)) (let-values (((s a) (unify '(hello world) '(?x world) '()))) s)))
