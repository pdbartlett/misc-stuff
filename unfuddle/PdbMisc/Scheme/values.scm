(define testPair (cons 1 2))

(define splitPair
  (lambda (x)
    (values (car x) (cdr x))
    )
  )

(let-values (( (fst snd) (splitPair testPair) ))
  (display (+ fst snd))
  )