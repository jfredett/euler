(ns problem-2.core)

(def fibs (range 0 10))

(defn fib [x]
  (defn fib-help [x acc]
    (cond (= x 0)
          (first acc)
          :else
          (recur (dec x) (rest acc))))
  (fib-help x fibs))


