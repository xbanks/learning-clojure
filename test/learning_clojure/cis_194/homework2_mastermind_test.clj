(ns learning-clojure.cis-194.homework2-mastermind-test
  (:require
   [clojure.test :refer :all]
   [learning-clojure.cis-194.homework2-mastermind :refer :all]))

;; Exercise 1 test
(deftest exact-matches-test
  "Exact matches between two lists"
  (def matches1 (exact-matches [1 2 3]
                               [1 2 4]))
  (def matches2 (exact-matches [:green :blue :green :red]
                               [:green :blue :teal  :red]))
  (is (= matches1 2))
  (is (= matches2 3)))


;; Exercise 2 test
(deftest count-colors-test
  "Count the amount of occurances of certain colors in a list"
  (def given-colors {:green 0 :blue 0 :red 0 })
  (def color-list [:green :green :blue :red])
  (def expected-count {:green 2 :blue 1 :red 1})
  (is (= (count-colors color-list given-colors) expected-count)))


(deftest matches-test
  "Matches between lists"
  (def list-a [] ))

;; Exercise 3 test
(deftest get-move-test
  ""
  (def test-secret [:green :blue :orange])
  (def test-code-1 [:green :green :blue])
  (def test-move {:guess test-code-1 :exact 1 :non-exact 1})
  (is (= test-move (get-move test-secret test-code-1)))
  (is (= test-move (get-move' test-secret test-code-1))))


;; Exercise 4 test
(deftest consistent?-test
  "Checking consistency"
  (let [test-move {:guess [:red :red :blue :green] :exact 1 :non-exact 1}
        test-code-1 [:red :blue :yellow :purple]
        test-code-2 [:red :blue :red :purple]]
    (is (consistent? test-move test-code-1))
    (is (not (consistent? test-move test-code-2)))))

;; Exercise 5 test
(deftest filter-codes-test
  ""
  (def test-code-list [[:red :blue :yellow :purple]
                  [:red :blue :red :purple]])
  (def filtered [[:red :blue :yellow :purple]])
  (def test-move {:guess [:red :red :blue :green] :exact 1 :non-exact 1})
  (is (= filtered (filter-codes test-move test-code-list))))


;; Exercise 6 test
;; I'm not sure what a good test for this would be yet, as i'm not sure if the solve function will always give the same output.
;; It probably will, since I *think* all the functions i've used are referentially transparent... 
;; TODO: I'll write this test later
