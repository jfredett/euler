(ns problem-1.core)

(defn multiple-of? [n m] 
  (== (mod m n) 0))

(defn solution-for [numbers]
  (reduce 
    (fn [a e] (+ a e))
    (filter (fn [x] 
      (or (multiple-of? 3 x)
          (multiple-of? 5 x)))
    numbers)))

(solution-for (range 1 1000))

