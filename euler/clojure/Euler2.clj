(defn solve-it [max]
  (loop [ths 1, nxt 1, total 0]
    (if (>= ths max)
      (println total)
      (if (zero? (rem ths 2))
        (recur nxt (+ nxt ths) (+ total ths))
        (recur nxt (+ nxt ths) total)
      )
    )
  )
)

(solve-it 4000000)