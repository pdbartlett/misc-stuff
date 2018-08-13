(defn sum-squares [n] (reduce + (map #(* % %) (range (inc n)))))

(defn abs [n] (if (< n 0) (- n) n))

(defn solve-it [n]
  (let [s (* (/ n 2) (+ n 1))] 
    (abs (- (sum-squares n) (* s s)))
  )
)

(println (solve-it 100))