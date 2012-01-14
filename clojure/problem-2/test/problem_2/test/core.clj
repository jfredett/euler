(ns problem-2.test.core
  (:use [problem-2.core])
  (:use [clojure.test]))

(deftest helpers
  (testing "fibonacci function"
           (is (= 0 (fib 0)))
           (is (= 1 (fib 1)))
           (is (= (+ (fib 5) (fib 6)) (fib 7)))))
              

