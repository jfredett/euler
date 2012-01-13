(ns problem-1.test.core
  (:use [problem-1.core])
  (:use [clojure.test]))


(deftest helper-functions
  (testing "multiple-of?"
    (is (= true (multiple-of? 3 9)))
    (is (= false (multiple-of? 4 9)))))

(deftest solution
  (testing "example from the problem statment"
    (is (= 23 (solution-for (range 1 10))))))
