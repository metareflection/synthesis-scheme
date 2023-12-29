(let ()
  (define (child-finder node montecarlo)
    (if (= (node-state node) 0)
        (node-add-children node (list (node-new 1) (node-new -1)))
        (map
         (lambda (i)
           (let ((modifier
                  (* (if (= i 1) 100 200)
                     (if (< (node-state node) 0) -1 1))))
             (node-add-child node (node-new (+ (node-state node) modifier)))))
         (iota 2))))
  
  (define (node-evaluator node montecarlo)
    (if (> (node-state node) 1000)
        1
        (if (< (node-state node) -1000)
            -1
            0 ;; TODO: why can't this be #f?
            )))

  (define montecarlo (montecarlo-new (node-new 0)))
  (montecarlo-child-finder-set! montecarlo child-finder)
  (montecarlo-node-evaluator-set! montecarlo node-evaluator)

  (montecarlo-simulate montecarlo 50)

  (let ()
    (define chosen-node (montecarlo-make-choice montecarlo))
    (define exploratory-node (montecarlo-make-exploratory-choice montecarlo))
    (test
      (node-state chosen-node)
      1)
    (test
      (not (not exploratory-node))
      #t)))
