;; #1 - Nothing but the Truth
true

;; #2 - Simple Math
4

;; #3 - Intro to Strings
"HELLO WORLD"

;; #4 - Intro to Lists
:a :b :c

;; #5 - Lists: conj
'(1 2 3 4)

;; #6 - Intro to Vectors
:a :b :c

;; #7 - Vectors: conj
[1 2 3 4]

;; #8 - Intro to Sets
#{:c :b :d :a}

;; #9 - Sets: conj
2

;; #10 - Intro to Maps
20

;; #11 - Maps: conj
{:b 2}

;; #12 - Intro to Sequences
3

;; #13 - Sequences: rest
[20 30 40]

;; #14 - Intro to Functions
8

;; #15 - Double down
(partial * 2)

;; #16 - Hello World
#(format "Hello, %s!" %)

;; #17 - Sequences: map
'(6 7 8)

;; #18 - Sequences: filter
'(6 7)

;; #19 - Last element
#(first (reverse %))

;; #20 - Penultimate Element
(comp second reverse)

;; #21 - Nth Element
.get

;; #22 - Count a Sequence
#(.size (vec %))

;; #23 - Reverse a Sequence
#(reduce conj () %)

;; #24 - Sum It All Up
reduce +

;; #25 - Find the odd numbers
filter odd?

;; #26 - Fibonacci Sequence
#(take % (map first (iterate (fn [[a b]]
                               [b (+ a b)])
                             [1 1])))

;; #27 - Palindrome Detector
#(= (vec %) (reverse %))

;; #28 - Flatten a Sequence
(fn fl [coll]
  (mapcat #(if (sequential? %)
             (fl %)
             [%])
          coll))

;; #29 - Get the Caps
(fn [s]
  (apply str (filter #(Character/isUpperCase %) s)))

;; #30 - Compress a Sequence
#(map first (partition-by identity %))

;; #31 - Pack a Sequence
partition-by identity

;; #32 - Duplicate a Sequence
mapcat #(repeat 2 %)

;; #33 - Replicate a Sequence
(fn [coll times]
  (mapcat #(repeat times %) coll))

;; #34 - Implement range
(fn [start end]
  (take (- end start) (iterate inc start)))

;; #35 - Local bindings
7

;; #36 - Let it be
[x 7
 y 3
 z 1]

;; #37 - Regular Expressions
"ABC"

;; #38 - Maximum value
(comp last sort list)

;; #39 - Interleave Two Seqs
#(flatten (map (fn [item1 item2]
                 (conj '() item2 item1))
               %1 %2))

;; #40 - Interpose a Seq
#(butlast (interleave %2 (repeat %1)))

;; #41 - Drop Every Nth Item
(fn drop-every-n [coll n]
   (keep-indexed
    (fn [index value]
      (if (not= 0 (rem (inc index) n))
        value))
    coll))

;; #42 - Factorial Fun
#(reduce * (take % (iterate inc 1)))

;; #43 - Reverse Interleave
(fn re-interleave [coll n]
  (for [i (range n)]
    (take-nth n (drop i coll))))

;; #44 - Rotate sequence
(fn [n coll]
  (let [c (count coll)]
    (take c (drop (mod n c) (cycle coll)))))

;; #45 - Intro to iterate
'(1 4 7 10 13)

;; #46 - Flipping out
(fn [f]
  (fn [arg1 arg2]
    (f arg2 arg1)))

;; #47 - Contain yourself
4

;; #48 - Intro to some
6

;; #49 - Split a sequence
(juxt take drop)

;; #50 - Split by type
#(vals (into [] (group-by type %)))
