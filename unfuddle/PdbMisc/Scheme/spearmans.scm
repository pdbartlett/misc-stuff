(load "preludish.scm")
(load "sort.scm")

(define makeAcsendingSortFunc
  (lambda (f)
    (lambda (x y)
      (< (f x) (f y))
      )
    )
  )

(define rank
  (lambda (ls)
    (if (not (list? ls)) (error "Expected a list"))
    (let*
        (
         (indexed  (begin (reset) (map number ls)))
         (sorted   (sort (makeAcsendingSortFunc car) indexed))
         (ranked   (begin (reset) (map number sorted)))
         (resorted (sort (makeAcsendingSortFunc cadar) ranked))
         )
      (map (lambda (x) (cons (caar x) (cadr x))) resorted)
      )
    )
  )

(define spearmans
  (lambda (lsData1 lsData2)
    (let*
        (
         (ranked1 (rank lsData1))
         (ranked2 (rank lsData2))
         (diffs   (map (lambda (p1 p2) (- (cdr p1) (cdr p2))) ranked1 ranked2))
         (ssd     (foldLeft 0 (lambda (last new) (+ last (* new new))) diffs))
         (n       (length diffs))
         )
      (- 1 (* 6 (/ ssd (* n (- (* n n) 1)))))
      )
    )
  )

;=======
; Tests
;=======

(define testPositive
  (lambda ()
    (= 1 (spearmans (list 2 1 3) (list 20 10 30)))
    )
  )

(define testNegative
  (lambda ()
    (= -1 (spearmans (list 3 1 2) (list 10 30 20)))
    )
  )

(runTests (list testPositive testNegative))

