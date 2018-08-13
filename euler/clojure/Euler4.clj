(defn palindrome? [n] (= (seq (str n)) (reverse (str n))))

(defn solve-it []
  (reduce max
    (filter palindrome? (for [i (range 1000) :when (> i 99) j (range 1000) :when (>= j i)] (* i j)))
  )
)

(println (palindrome? 123))
(println (palindrome? 121))

(println (solve-it))
  