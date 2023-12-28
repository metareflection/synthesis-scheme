(define-record-type
    solver-stat
  (fields
   call ;;list
   result ;;any
   ticks ;;nat
   max-ticks ;;nat
   completed? ;;boolean
))

(define monitor-log '())
(define (add-monitor-stat! x)
  (set! monitor-log (cons x monitor-log)))

(define (monitor max-ticks f solver solver-name)
  (let ((call-log '()))
    (let ((solver-monitored
           (lambda args
             (set! call-log (cons (cons solver-name args) call-log))
             (apply solver args))))
      (let-values
          (((r ticks completed?)
            (run-until-ticks-values max-ticks (lambda () (f solver-monitored)))))
        (add-monitor-stat!
         (make-solver-stat
          call-log
          r
          ticks
          max-ticks
          completed?))
        (if completed?
            r
            (error 'monitor "expired"))))))
