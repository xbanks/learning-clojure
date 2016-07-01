(ns learning-clojure.cis-194.homework2_mastermind)

; helper functions
; I liked haskell zip functions, so i made some
(defn zip [lst1 lst2] (map vector lst1 lst2))

(defn zip-with [fn lst1 lst2]
  (->> (zip lst1 lst2) (map fn)))

; determines whether a vector has 2 equal elements. I should probably use apply in the zip-with
(defn vec-eq [[a b]] (= a b))

; increments a number, if the 2nd argument is true, helps with my reduction in Exercise 1
(defn inc-if-true [num bool] (+ num (if (true? bool) 1 0)))

(def colors [:green :blue :red :orange])
(def initial-color-count (reduce #(assoc %1 %2 0 ) {} colors))

; Exercise 1: Create a function that takes two lists, and outputs the number of exact matches between them. So: exact-matches [1 2 3] [1 3 3] => 2
(defn exact-matches [left right]
   (reduce inc-if-true 0 (zip-with vec-eq left right)))

; Exercise 2: Create a function that takes a list of colors, and outputs a list (i used a map) containing the number of occurances of each color
; count-colors [:green :blue :blue] => {:green 1 :blue 2 :yellow 0 etc...}
; I think I might be able to abstract this out
(defn count-colors
  ([lst current-count]
    (let [head (first lst)
          tail (rest lst)]
      (if (nil? head)
        current-count
        (count-colors tail (if (nil? (current-count head))
                             current-count
                             (update current-count head inc))))))
  ([colors] 
   (let [color-count initial-color-count]
     (count-colors colors color-count))))

; get the total amount of matches between the lists
(defn matches [list1 list2]
  (->>
    [list1 list2]
    (map count-colors)
    (apply zip)
    (map #(map second %))
    (map #(apply min %))
    (reduce +)))

;; Exercise 3: Create a function that takes the secret code, and the guess code, and output a "move", which is basically the guess, exact matches, and non-exact matches
;; I've decided to return them as a map, to aid with exercise 4
(defn get-move [secret guess]
  (let [exact (exact-matches secret guess)
        all   (matches secret guess)
        non-exact (- all exact)] (->
           {} ; Can shorten this to one line
           (assoc :guess guess)
           (assoc :exact exact)
           (assoc :non-exact non-exact))))

(get-move [:red :blue :yellow :orange] [:red :orange :orange :blue])

;; Exercise 4: 
(defn consistent? [move code]
  (let [move2 (get-move (:guess move) code)] 
    (and
      (apply = (map :exact [move move2]))
      (apply = (map :non-exact [move move2])))))

;; move these to the tests
(def testmove {:guess [:red :red :blue :green] :exact 1 :non-exact 1})
(def testcode [:red :blue :yellow :purple])
(def testcode2 [:red :blue :red :purple])
(consistent? testmove testcode)
(consistent? testmove testcode2)

;; Exercise 5
(defn filter-codes [move code-list]
  (filter #(consistent? move %) code-list))

(defn all-codes 
  ([num color-list]
   (if (zero? num)
     []
     (if (= 1 num)
       (map #(conj [] %) color-list)
       (as->
         (all-codes (dec num) color-list) v
         (map (fn [a] (map #(conj % a) v)) color-list )
         (reduce concat v)))))
  ([num] (all-codes num colors)))

(defn solve
  ([secret guess code-list]
   (if (not (= (count secret) (count guess)))
     ;; should probably raise an error here?
     ((println "The length of the secret and the guess must be the same!") [])
     (if (= secret guess)
            [(get-move secret guess)]
            (let [current-move (get-move secret guess)
                  filtered (filter-codes current-move code-list)
                  next-guess (first filtered)]
              (cons current-move (solve secret next-guess filtered))))))
  ([secret guess]
   (solve secret guess (all-codes (count secret))))
  ([secret]
   (solve secret (repeat (count secret) :red))))

(defn pretty-print-solve [secret] (->> secret solve (map println)))
