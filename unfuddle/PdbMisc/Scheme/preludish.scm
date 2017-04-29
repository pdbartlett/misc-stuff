(load "test.scm")

(define counter 1)

(define next
  (lambda ()
    (let ((ret counter))
      (set! counter (+ counter 1))
      ret
      )
    )
  )

(define reset
  (lambda ()
    (set! counter 1)
    )
  )

(define number
  (lambda (x)
    (list x (next))
    )
  )

(define foldLeft
  (lambda (init fn list)
    (if (null? list) init (foldLeft (fn init (car list)) fn (cdr list)))
    )
  )

(define groupBy
  (lambda (fn list)
    (foldLeft () (groupByHelper fn) list)
    )
  )

; Not yet finished
(define groupByHelper
  (lambda (fn)
    (lambda (prevList nextItem)
      (if (null? prevList)
          (list (list nextItem))
          (append prevList (list nextItem))
          )
      )
    )
  )

(define applyPartial
  (lambda (fn . boundArgs)
    (lambda freeArgs
      (apply fn (append boundArgs freeArgs))
      )
    )
  )

(define listEqual
  (lambda (x y)
    (if (list? x)
        (if (= (length x) (length y))
            (listEqualHelper x y)
            #f
            )
        (= x y)
        )
    )
  )

(define listEqualHelper
  (lambda (x y)
    (if (null? x)
        #t
        (if (listEqual (car x) (car y))
            (listEqualHelper (cdr x) (cdr y))
            #f
            )
        )
    )
  )

;=======
; Tests
;=======

(define testCounter1
  (lambda ()
    (= 1 (next))
    )
  )

(define testCounter2AndReset
  (lambda ()
    (let ((value (next)))
      (reset)
      value
      )
    )
  )

(define testFoldLeft
  (lambda ()
    (= 15 (foldLeft 0 + (list 1 2 3 4 5)))
    )
  )

(define testListEqualTrue
  (lambda ()
    (listEqual (list 1 2 3) (list 1 2 3))
    )
  )

(define testListEqualFalse1
  (lambda ()
    (not (listEqual (list 1 2 3) (list 3 2 1)))
    )
  )

(define testListEqualFalse2
  (lambda ()
    (not (listEqual (list 1 2 3) (list 1 2)))
    )
  )

(define testListEqualNested
  (lambda ()
    (listEqual (list (list 1 2 3) (list 4 5 6))
               (list (list 1 2 3) (list 4 5 6))
               )
    )
  )

(define testGroupBy
  (lambda ()
    (listEqual (list (list 1) (list 2)) (groupBy = (list 1 2)))
    )
  )

(define testApplyPartial
  (lambda ()
    (let ((add2 (applyPartial + 2)))
      (= 3 (add2 1))
      )
    )
  )

(runTests (list testCounter1 testCounter2AndReset testFoldLeft
                testListEqualTrue testListEqualFalse1 testListEqualFalse2 testListEqualNested
                testGroupBy testApplyPartial))
