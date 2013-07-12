(ns genetic.core)

;; We decided to deal with lower-case only plus space. Since strings
;; in Clojure can be treated as lists, we could have achieved the same
;; thing with:
;; (def letters
;;   " abcdefghijklmnopqrstuvwxyz")
(def letters
  (conj (map (comp char
                   (partial + 97))
             (range 26))
        \space))

;; Create a random string of n letters from a given alphabet.
(defn random-string [n source]
  (apply str
         (repeatedly n
                     (fn []
                       (rand-nth source)))))

;; Assign numeric values in a map to each item in 'letters. We first
;; create a sequence of [key value] pairs, and then put them into an
;; empty map.
(def letter-val-map
  (into {}
        (map vector
             letters
             (range))))

;; How far away do two characters differ?
(defn char-diff [x y]
  (Math/abs (- (letter-val-map x)
               (letter-val-map y))))

;; For two strings of equal length, calculate the sum of the pairwise
;; character distances.
(defn string-distance [s1 s2]
  (reduce + (map char-diff s1 s2)))

;; Given a string, choose a random letter and mutate it.
(defn mutate-string [s]
  (let [n (rand-int (count s))
        [l [_ & r]] (split-at n s)]
    (apply str (concat l [(rand-nth letters)] r))))

;; Given a target and a seed string, mutate the seed 100 times, and
;; choose the result whose distance from the target string is minimal.
(defn genetic-iteration [target seed]
  (println seed)
  (apply min-key
         (partial string-distance target)
         (conj (map mutate-string (repeat 100 seed))
               seed)))

;; Create a random seed, and keep iterating the genetic-iteration
;; function until we reach the target string.
(defn guess-target [target]
  (first (drop-while (partial not= target)
                     (iterate (partial genetic-iteration target)
                              (random-string (count target) letters)))))
