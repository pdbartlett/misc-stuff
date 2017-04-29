(define runTest
  (lambda (test)
    (let ((result (apply test ())))
      (display test)
      (display " ")
      (display (if result "OK" "Failed"))
      (newline)
      result
      )
    )
  )

(define runTests
  (lambda (tests)
    (display (if (and (map runTest tests)) "OK" "Failed"))
    (newline)
    )
  )
