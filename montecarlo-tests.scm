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
            #f)))

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

(let ()
  (define (child-finder node montecarlo)
    (node-add-children node (build-children node))
    (node-update-win-value node (node-state node)))

  (define (build-children node)
    (let ((children
           (map
            (lambda (i)
              (let ((child (node-new (or (if (= (node-state node) 0) (if (= i 1) 1 -1) (node-state node))))))
                (node-policy-value-set! child (if (= i 1) 0.90 0.10))
                child))
            (iota 2))))
      (node-update-win-value node 0)
      children))
  
  (define montecarlo (montecarlo-new (node-new 0)))
  (montecarlo-child-finder-set! montecarlo child-finder)

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
