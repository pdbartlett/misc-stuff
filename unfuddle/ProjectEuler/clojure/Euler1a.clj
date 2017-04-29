(defn sum-multiples [n d]
  (let [c (int (/ n d))] (* d (* (/ c 2) (+ c 1))))
)

(defn solve-it [n a b]
  (+ (sum-multiples n a) (sum-multiples n b) (- (sum-multiples n (* a b))))
)

(println (solve-it 999 3 5))