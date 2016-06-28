(ns learning-clojure.cis-194.homework1-luhn)

; Exercise 1: Get the last digit of a number
(defn last-digit [num] (mod num 10))

; Exercise 2: Dropping the last digit from a number
; Note: quot actually does integer division while / produces a ratio. So (/ 10 3) => 10/3 while (quot 10 3) => 3
(defn drop-last-digit [num] (quot num 10))

; Exercise 3: Split a number into a list of digits, and then reverse it
; Kind of an all in one approach...
(defn to-rev-digits [num]
  (if (zero? num)
    '()
    (conj (to-rev-digits (drop-last-digit num))
          (last-digit num))))

; A more modular way of doing it?
; Note: conj on a vector adds cheaply to the end, while conj on a list adds cheaply to the beginning
; http://stackoverflow.com/questions/5734435/put-an-element-to-the-tail-of-a-collection
(defn to-digits [num]
  (if (zero? num) [] (conj (to-digits (drop-last-digit num)) (last-digit num))))

; Note ->> and -> are a type of piping mechanisim, -> pipes the output of one function to the FIRST argument, while ->> pipes it to the LAST one
(defn to-rev-digits' [num] (->> (to-digits num) reverse))

(defn double-every-other [lst]
  (map-indexed #(if (odd? %1) (* %2 2) %2) lst))

(defn sum-digits [lst]
  (->> (map to-rev-digits lst) (reduce concat) (reduce +))) ; Piping to the last argument

(defn luhn
  ([num]
   (-> (to-rev-digits num) ; Piping to the first argument
       (double-every-other)
       (sum-digits)
       (mod 10)
       (zero?)))
  ([num & args]
   (->> (conj args num) (map luhn))))