(defn py-triplet? [a b c] (= (* c c) (+ (* a a) (* b b))))

(defn solve-it [n]
  (loop [i 1, j 2]
    (let [k (- n (+ i j))]
      (cond
        (< k 0) (list) 
        (zero? k) (recur (inc i) (+ i 2))
        (py-triplet? i j k) (list i j k)
        true (recur i (inc j))
      )
    )
  )
)

(def answer (solve-it 1000))

(doseq n answer (println n))
(println (reduce * answer))
