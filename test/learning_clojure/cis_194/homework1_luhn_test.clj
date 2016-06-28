(ns learning-clojure.cis-194.homework1-luhn-test
  (:require
    [clojure.test :refer :all]
    [learning-clojure.cis-194.homework1-luhn :refer :all]))

(deftest luhn-test
  (testing "Luhn"
    (is (luhn 5594589764218858))
    (is (not (luhn 1234567898765432)))))

(deftest to-rev-digits-test
  (testing "reversing digits"
    (is (= (to-rev-digits 123) '(3 2 1)))
    (is (= (to-rev-digits 1234)
           (to-rev-digits' 1234)))))

(deftest last-digit-test
  (testing "returning the last digit"
    (is (= (last-digit 123) 3))))

(deftest drop-last-digit-test
  (testing "removing the last digit"
    (is (= (drop-last-digit 123) 12))))

(luhn-test)
(to-rev-digits-test)
(last-digit-test)
(drop-last-digit-test)