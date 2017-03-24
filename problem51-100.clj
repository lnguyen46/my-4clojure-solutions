;; #51 - Advanced Destructuring
[1 2 3 4 5]

;; #52 - Intro to Destructuring
[c e]

;; #53 - Longest Increasing Sub-Seq
(fn linc-subseq [coll]
  (loop [acc []
         res []
         [head & more :as all] coll]
    (cond
      (nil? head) (->> (conj res acc)
                       (filter #(>= (count %) 2))
                       (sort-by count)
                       (last)
                       (vec))
      (or (empty? acc)
          (= head (inc (peek acc)))) (recur (conj acc head) res more)
      :else (recur [] (conj res acc) all))))

;; #54 - Partition a Sequence
(fn [n coll]
  (->> (partition-by #(quot (.indexOf coll %)
                            n)
                     coll)
       (filter #(= n (count %)))))

;; #55 - Count Occurrences
#(reduce (fn [acc item]
           (if (some #{item} acc)
             acc
             (conj acc item)))
         [] %)

;; #56 - Find Distinct Items
#(reduce (fn [acc item]
           (if (some #{item} acc)
             acc
             (conj acc item)))
         [] %)

;; #57 - Simple Recursions
'(5 4 3 2 1)

;; #58 - Function Composition
(fn [& fns]
  (let [[f & fs] (reverse fns)]
    (fn [& args]
      (reduce #(%2 %1)
              (apply f args)
              fs))))

;; #59 - Juxtaposition
(fn [& fs]
  (fn [& args]
    (map #(apply % args) fs)))

;; #60 - Sequence Reductions
(fn red
  ([f coll] (red f (first coll) (rest coll)))
  ([f init coll]
     (let [[x & xs] coll]
       (cons init (lazy-seq
                   (when x (red f (f init x) xs)))))))

;; #61 - Map Construction
#(into {} (map vector %1 %2))

;; #62 - Re-implement Iterate
(fn ite [f val]
  (cons val (lazy-seq (ite f (f val)))))

;; #63 - Group a Sequence
(fn gby [f coll]
  (apply (partial merge-with into)
         (map #(assoc {} (f %) [%]) coll)))

;; #64 - Intro to Reduce
+

;; #65 - Black Box Testing
(fn bb [coll]
  (cond
    (= coll (conj coll {})) :map
    (get (conj coll :t) :t) :set
    (let [g1 (gensym)
          g2 (gensym)]
      (= g2 (last (conj coll g1 g2)))) :vector
    :else :list))

;; #66 - Greatest Common Divisor
(fn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

;; #67 - Prime Numbers
(fn boring [n]
  (take n
        (filter (fn is-prime [n]
                  (nil? (some #(zero? (mod n %))
                              (range 2 n))))
                (cons 2 (range 3 1000 2)))))

;; #68 - Recurring Theme
'(7 6 5 4 3)

;; #69 - Merge with a Function
(fn merw [f & maps]
  (let [[m & more] maps]
    (reduce (fn [acc hmap]
              (reduce-kv (fn [acc k v]
                           (if (contains? acc k)
                             (assoc acc k (f (get acc k) v))
                             (assoc acc k v)))
                         acc
                         hmap))
            m
            more)))

;; #70 - Word Sorting
(fn [s]
  (sort-by #(clojure.string/lower-case %)
           (clojure.string/split s #"\W")))

;; #71 - Rearranging Code: ->
last

;; #72 - Rearranging Code: ->>
apply +

;; #73 - Analyze a Tic-Tac-Toe Board
(fn [board]
  (let [[row1 row2 row3 :as rows] board
        columns (apply map vector board)
        diags [(vector (first row1)
                       (second row2)
                       (last row3))
               (vector (last row1)
                       (second row2)
                       (first row3))]]
    (some #{:x :o}
          (flatten (filter #(= 1 (count (distinct %)))
                           (reduce into (list rows columns diags)))))))

;; #74 - Filter Perfect Squares
(fn [s]
  (->> s
       (#(clojure.string/split % #","))
       (map read-string)
       (filter #(== (Math/sqrt %) (int (Math/sqrt %))))
       (clojure.string/join ",")))

;; #75 - Euler's Totient Function
(fn [x]
  (if (= x 1)
    1
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))]
      (->> (range 1 x)
           (filter #(and (< % x) (= 1 (gcd x %))))
           (count)))))

;; #76 - Intro to Trampoline
[1 3 5 7 9 11]

;; #77 - Anagram Finder
(fn [words]
  (->> words
       (group-by set)
       (vals)
       (filter next)
       (map set)
       (set)))

;; #78 - Reimplement Trampoline
(fn tramp
  ([f]
     (let [res (f)]
       (if (fn? res)
         (recur res)
         res)))
  ([f & args]
     (tramp #(apply f args))))

;; #79 - Triangle Minimal Path
(fn [tree]
  (first 
   (reduce #(map +
                (map min (butlast %1)
                     (rest %1))
                %2)
           (reverse tree))))

;; #80 - Perfect Numbers
(fn [n]
  (= n
     (apply + (filter #(zero? (rem n %)) (range 1 n)))))

;; #81 - Set Intersection
(comp set filter)

;; #82 - Word Chains
 (fn [s]
  "Return true if a set of words has at maximum 2 links that have only one connection."
  (letfn [(sub? [w1 w2]
            (= 1 (get (frequencies (map #(= %1 %2) w1 w2))
                      false)))
          (insert-or-del? [a b]
            (let [min (min-key count a b)]
              (= (count min)
                 (count (clojure.set/intersection (set a) (set b))))))
          (connect? [word another]
            (let [lw (count word)
                  la (count another)]
              (cond
                (= lw la) (sub? word another)
                (= 1 (Math/abs (- lw la))) (insert-or-del? word another)
                :else nil)))
          (number-of-connections [word]
            (count (filter #(connect? word %) (disj s word))))]
    (-> (group-by #(number-of-connections %) s)
        (get 1)
        (count)
        (<= 2))))

;; #83 - A Half-Truth
not=

;; #84 - Transitive Closure
(fn [s]
  (let [m (into {} s)]
    (reduce-kv (fn [acc k v]
                 (if-let [val (get m v)]
                   (recur (conj acc [k v] [k val])
                          k val)
                   (conj acc [k v])))
               #{} m)))

;; #85 - Power Set
(fn powerset [s]
  (if (empty? s)
    #{#{}}
    (let [rest-set (powerset (next s))]
      (clojure.set/union rest-set
                         (map #(conj % (first s)) rest-set)))))

;; #86 - Happy numbers
(fn [n]
  (loop [n n, seen #{}]
    (cond
      (= n 1) true
      (seen n) false
      :else
      (recur (->> (str n)
                  (map #(Character/digit % 10))
                  (map #(* % %))
                  (reduce +))
             (conj seen n)))))

;; #88 - Symmetric Difference
#(->> (into (vec %1) (vec %2))
      (remove (clojure.set/intersection %1 %2))
      (set))

;; #89 - Graph Tour


;; #90 - Cartesian Product
(fn [xs ys]
  (set
   (for [x xs y ys]
     [x y])))

;; #91 - Graph Connectivity


;; #92 - Read Roman numerals
#(let [roman-num (zipmap ["I" "V" "X" "L" "C" "D" "M" "IV" "IX" "XL" "XC" "CD" "CM"]
                         [1 5 10 50 100 500 1000 4 9 40 90 400 900])]
   (apply +
          (map roman-num
               (re-seq #"CM|CD|XC|XL|IX|IV|[MDCLXVI]" %))))

;; #93 - Partially Flatten a Sequence
(fn [coll]
  (filter #(and (sequential? %)
                (every? (complement sequential?) %))
          (rest (tree-seq sequential? seq coll))))

;; #94 - Game of Life
(fn [board]
  "[x, y] - [x element of row y - row y of this board]"
  (let [dx (count (first board))
        dy (count board)
        neighbours (fn [pos]
                     (let [ps [[-1 -1] [0 -1] [1 -1]
                               [-1  0]        [1  0]
                               [-1  1] [0  1] [1  1]]
                           neighbour-positions (map #(map + pos %)
                                                    ps)]
                       (map (fn [[x y]]
                              (get-in board [y x]))
                            neighbour-positions)))]
    (map
       #(apply str %)
       (partition
          dx
          (for [row (range dy) elt (range dx)]
            (let [cell [elt row]
                  live-neighbours (count (filter #{\#} (neighbours cell)))
                  cell-live? (= \# (get-in board [row elt]))]
              (cond
                (and cell-live? (< live-neighbours 2)) \space
                (and cell-live? (or (= live-neighbours 2) (= live-neighbours 3))) \#
                (and cell-live? (> live-neighbours 3)) \space
                (and (not cell-live?) (= live-neighbours 3)) \#
                :else \space)))))))

;; #95 - To Tree, or not to Tree
(fn bin-tree? [tree]
  (or (nil? tree)
      (and (sequential? tree)
           (= 3 (count tree))
           (every? bin-tree? (rest tree)))))

;; #96 - Beauty is Symmetry
(letfn [(mirror? [t1 t2]
          (or (= nil t1 t2)
              (let [[x l1 r1] t1
                    [y l2 r2] t2]
                (and (= x y)
                     (mirror? l1 r2)
                     (mirror? l2 r1)))))]
  #(mirror? (nth % 1)
            (nth % 2)))

;; #97 - Pascal's Triangle
 (fn [n]
  "Using `n choose k` method to implement this function."
  (let [fac (fn fac [x]
              (if (zero? x)
                1
                (* x (fac (- x 1)))))
        row (- n 1)]
    (for [term (range n)]
      (/ (fac row)
         (* (fac term) (fac (- row term)))))))

;; #98 - Equivalence Classes
(fn [f d]
  (->> d
       (group-by f)
       (vals)
       (map set)
       (set)))

;; #99 - Product Digits
(fn [n1 n2]
  (->> (* n1 n2)
       (str)
       (map #(Character/digit % 10))))

;; #100 - Least Common Multiple
(fn [& ns]
  (letfn [(gcd [a b]
            (if (zero? b)
              a
              (recur b (mod a b))))
          (lcm [a b]
            (/ (* a b) (gcd a b)))]
    (reduce lcm ns)))


