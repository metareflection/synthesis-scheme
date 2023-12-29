;; based on
;; https://github.com/namin/llm-verified-with-monte-carlo-tree-search/tree/main/montecarlo
;; itself adapted from
;; https://github.com/ImparaAI/monte-carlo-tree-search

(define-record-type
    montecarlo
  (fields
   root-node
   (mutable solution)
   (mutable child-finder)
   (mutable node-evaluator)))

(define (montecarlo-new root-node)
  (make-montecarlo root-node #f #f (lambda (child montecarlo) #f)))

(define (node-random-best-child node root-node f)
  (let ((children (node-children node)))
    (let loop ((children (cdr children))
               (best-children (list (car children)))
               (most-so-far (f (car children))))
      (if (null? children)
          (choose best-children)
          (let* ((child (car children))
                 (f-child (f child)))
            (if (> f-child most-so-far)
                (loop (cdr children) (list child) f-child)
                (if (= f-child most-so-far)
                    (loop (cdr children) (cons child best-children) most-so-far)
                    (loop (cdr children) best-children most-so-far))))))))

(define (montecarlo-make-choice montecarlo)
  (let ((root-node (montecarlo-root-node montecarlo)))
    (node-random-best-child root-node root-node node-visits)))

(define (montecarlo-make-exploratory-choice montecarlo)
  (let* ((root-node (montecarlo-root-node montecarlo))
         (children-visits (map node-visits (node-children root-node)))
         (children-visit-probabilities
          (map (lambda (visit) (exact->inexact (/ visit (node-visits root-node))))
               children-visits))
         ;; TODO: in the original, total is 1.0
         ;;  but it seems wrong, because parent node might be visited more than all children
         (total (apply + children-visit-probabilities))
         (random-probability (random total)))
    (let loop ((visit-probabilities children-visit-probabilities)
               (children (node-children root-node))
               (probabilities-already-counted 0.0))
      (let* ((probability (car visit-probabilities))
             (child (car children))
             (p (+ probabilities-already-counted probability)))
        (if (>= p random-probability)
            child
            (loop (cdr visit-probabilities) (cdr children) p))))))

(define (node-preferred-child-until-not-expanded current-node root-node)
  (if (node-expanded current-node)
      (node-preferred-child-until-not-expanded
       (node-get-preferred-child current-node root-node)
       root-node)
      current-node))

(define (montecarlo-simulate montecarlo expansion-count)
  (let loop ((i 0))
    (if (or (not expansion-count) (< i expansion-count))
        (if (not (montecarlo-solution montecarlo))
            (let ((current-node
                   (node-preferred-child-until-not-expanded
                    (montecarlo-root-node montecarlo)
                    (montecarlo-root-node montecarlo))))
              (montecarlo-expand montecarlo current-node)
              (loop (+ i 1)))))))

(define (montecarlo-expand montecarlo node)
  ((montecarlo-child-finder montecarlo) node montecarlo)
  (for-each
    (lambda (child)
      (let ((child-win-value ((montecarlo-node-evaluator montecarlo) child montecarlo)))
        (if child-win-value
            (node-update-win-value child child-win-value))
        (if (not (node-is-scorable child))
            (begin
              (montecarlo-random-rollout montecarlo child)
              (node-children-set! child '())))))
    (node-children node))
  (if (not (null? (node-children node)))
      (node-expanded-set! node #t)))

(define (montecarlo-random-rollout montecarlo node)
  ((montecarlo-child-finder montecarlo) node montecarlo)
  (let ((child (choose (node-children node))))
    (node-children-set! child '())
    (node-add-child node child)
    (let ((child-win-value ((montecarlo-node-evaluator montecarlo) child montecarlo)))
      (if child-win-value
          (node-update-win-value child child-win-value)
          (montecarlo-random-rollout montecarlo child)))))

(define-record-type
    node
  (fields
   state
   (mutable win-value)
   (mutable policy-value)
   (mutable visits)
   (mutable parent)
   (mutable children)
   (mutable expanded)
   player-number
   discovery-factor))

(define (node-new state)
  (make-node state 0 #f 0 #f '() #f #f 0.35))

(define (node-update-win-value node value)
  (node-win-value-set! node (+ (node-win-value node) value))
  (node-visits-set! node (+ (node-visits node) 1))
  (if (node-parent node)
      (node-update-win-value (node-parent node) value)))

(define (node-update-policy-value node value)
  (node-policy-value-set! node value))

(define (node-add-child node child)
  (node-children-set! node (append (node-children node) (list child)))
  (node-parent-set! child node))

(define (node-add-children node children)
  (for-each
    (lambda (child) (node-add-child node child))
    children))

(define (node-get-preferred-child node root-node)
  (node-random-best-child node root-node (lambda (node) (node-get-score node root-node))))

(define (node-get-score node root-node)
  (let* ((visits (if (> (node-visits node) 0) (node-visits node) 1))
         (discovery-operand
         (* (node-discovery-factor node)
            (or (node-policy-value node) 1)
            (sqrt (/ (log (node-visits (node-parent node)))
                     visits))))
        (win-multipler
         (if (equal? (node-player-number (node-parent node))
                     (node-player-number root-node))
             1.0
             -1.0))
        (win-operand
         (/ (* win-multipler (node-win-value node))
            visits)))
    (+ win-operand discovery-operand)))

(define (node-is-scorable node)
  (or (> (node-visits node) 0) (node-policy-value node)))
