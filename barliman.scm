(load "Barliman/cocoa/Barliman/mk-and-rel-interp/mk/mk-vicare.scm")
(load "Barliman/cocoa/Barliman/mk-and-rel-interp/mk/mk.scm")
(load "Barliman/cocoa/Barliman/mk-and-rel-interp/interp.scm")

;; this is for formatting in Barliman call
(define spell-the-conses
  (lambda (xs)
    (if (null? xs)
        ''()
        `(cons ,(car xs) ,(spell-the-conses (cdr xs))))))

(define (barliman-filler fun-name formals io* . options)
  (let* ((inputs (map car io*))
         (outputs (map cadr io*)))
    (define (ans)
      (define (results)
        (define (absento-all v q)
          (cond
            ((null? v) succeed)
            ((pair? v)
             (fresh ()
               (absento-all (car v) q)
               (absento-all (cdr v) q)))
            ((number? v)
             (absento v q))
            (else
             ;; makes the search incomplete to restrict on whole io*
             ;; trade-offs are worth thinking about
             succeed)))
        (run 1 (q)
          (absento 'if q)
          (absento 'cond q)
          (absento 'letrec q)
          (absento-all io* q)
          (evalo `(begin
                    (define ,fun-name (lambda ,formals ,q))
                    ,(spell-the-conses inputs))
                 outputs)))

      (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
        (if (null? results-fast)
            (begin (set! allow-incomplete-search? #f) (results))
            results-fast)))

    (ans)))

