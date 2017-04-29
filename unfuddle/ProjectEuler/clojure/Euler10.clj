(defn primes-to [n]
  (let [
      all-true (zipmap (drop 2 (range (inc n))) (cycle (list true)))
      max-fact (Math/floor (Math/sqrt n))
    ]
    (loop [i 2, sieve all-true]
      (cond
        (> i max-fact) (map #(key %) (filter #(val %) sieve))
        (get sieve i) (recur (inc i)
                        (merge sieve
                          (zipmap
                            (map (partial * i)
                              (drop 2 (range (inc (Math/floor (/ n i)))))
                            )
                            (cycle (list false))
                          )
                        )
                      )
        true (recur (inc i) sieve)
      )
    )
  )
)

(println (reduce + (primes-to 10)))
(println (reduce + (primes-to 2000000)))