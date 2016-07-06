(ns learning-clojure.cis-194.homework2-mastermind)

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
  ([[head & tail] current-count]
   (if (nil? head)
     current-count
     (count-colors tail (if (current-count head) ;; If the current color exists in the map, then increment it's count
                          (update current-count head inc)                          
                          current-count))))
  ([colors]
   (let [color-count initial-color-count]
     (count-colors colors color-count))))

;; get the total amount of matches between the lists
(defn matches [list1 list2]
  (->>
   [list1 list2]
   (map count-colors) ; => [{:green # :blue # etc...} {:green # :blue # etc..}]
   (map vals)
   (apply zip)
   (map #(apply min %))
   (reduce +)))

;; Exercise 3: Create a function that takes the secret code, and the guess code, and output a "move", which is basically the guess, exact matches, and non-exact matches
;; I've decided to return them as a map, to aid with exercise 4
(defn get-move [secret guess]
  (let [exact (exact-matches secret guess)
        all   (matches secret guess)
        non-exact (- all exact)] 
    (->                             
     {} ; Can shorten this to one line
     (assoc :guess guess)
     (assoc :exact exact)
     (assoc :non-exact non-exact))))

;; made it a little shorter..
(defn get-move' [secret guess]
  (let [exact (exact-matches secret guess)
        all   (matches secret guess)]
    {:guess guess :exact exact :non-exact (- all exact)}))

(get-move [:red :blue :yellow :orange] [:red :orange :orange :blue])

;; Exercise 4: Check to see if a move is consistent.
;; To be consistent in this context means that the number of exact and non-exact matches are the same between
;; the current move and the secret, and the given code and the current move
(defn consistent? [move code]
  (let [move-to-check (get-move (:guess move) code)] 
    (and
     (apply = (map :exact [move move-to-check]))
     (apply = (map :non-exact [move move-to-check])))))


;; Exercise 5: Filter out all non-consistent moves from a code list
(defn filter-codes [move code-list]
  (filter #(consistent? move %) code-list))

;; Exercise 6: Generate all possible sequences of codes
;; Maybe I can make this more readable/idiomatic?
(defn all-codes 
  ([num color-list]
   (if (zero? num)
     []
     (if (= 1 num)
       (map #(conj [] %) color-list)
       (as-> ;; another threading macro, using as-> you can pass the results as a named variable, in my case, 'v'
         (all-codes (dec num) color-list) v
         (map (fn add-permutation [a] (map #(conj % a) v)) color-list)
         (reduce concat v)))))
  ([num] (all-codes num colors)))


;; Exercise 7: Write the actual solver for mastermind.
;; This algorithm is the basic version, each time you make a guess, you filter the list of consistent moves,
;; and choose the first one as the guess for the next iteration.
(defn solve
  ([secret guess code-list]
   (if (not (= (count secret) (count guess)))
     ((println "The length of the secret and the guess must be the same!") '())
     (let [current-move (get-move secret guess)]
       (if (= guess secret)
         (list current-move)
         (conj (solve secret
                      (first (filter-codes current-move code-list))
                      (filter-codes current-move code-list))
               current-move)))))
  ([secret guess]
   (solve secret guess (all-codes (count secret))))
  ([secret]
   (def secret-length (count secret))
   (def default-code (repeat secret-length :red))
   (solve secret default-code)))

(defn pretty-print-solve [secret] (->> secret solve (zip (map #(str "Guess " % ": ") (iterate inc 1))) (map println)))
