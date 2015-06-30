;;; combinatorics.clj: efficient, functional algorithms for generating lazy
;;; sequences for common combinatorial functions.

;; by Mark Engelberg (mark.engelberg@gmail.com)
;; Last updated - March 20, 2015

(ns
  #^{:author "Mark Engelberg",
     :doc "Efficient, functional algorithms for generating lazy
sequences for common combinatorial functions. (See the source code 
for a longer description.)"}
  clojure.core.typed.deps.clojure.math.combinatorics
  (:refer-clojure :exclude [update]))

(comment
"  
(combinations items t) - A lazy sequence of all the unique
ways of taking t different elements from items.
Example: (combinations [1 2 3] 2) -> ((1 2) (1 3) (2 3))
Example: (combinations [1 1 2 2] 2) -> ((1 1) (1 2) (2 2))

(count-combinations items t) - (count (combinations items t)), but computed more directly
(nth-combination items t n) - (nth (combinations items t) n), but computed more directly

(subsets items) - A lazy sequence of all the subsets of
items (but generalized to all sequences, not just sets).
Example: (subsets [1 2 3]) -> (() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))
Example: (subsets [1 1 2 2]) -> (() (1) (2) (1 1) (1 2) (2 2) (1 1 2) (1 2 2) (1 1 2 2))

(count-subsets items) - (count (subsets items)), but computed more directly
(nth-subset items n) - (nth (subsets items) n), but computed more directly

(cartesian-product & seqs) - Takes any number of sequences
as arguments, and returns a lazy sequence of all the ways
to take one item from each seq.
Example: (cartesian-product [1 2] [3 4]) -> ((1 3) (1 4) (2 3) (2 4))
(cartesian-product seq1 seq2 seq3 ...) behaves like but is
faster than a nested for loop, such as:
(for [i1 seq1 i2 seq2 i3 seq3 ...] (list i1 i2 i3 ...))

(selections items n) - A lazy sequence of all the ways to
take n (possibly the same) items from the sequence of items.
Example: (selections [1 2] 3) -> ((1 1 1) (1 1 2) (1 2 1) (1 2 2) (2 1 1) (2 1 2) (2 2 1) (2 2 2))

(permutations items) - A lazy sequence of all the permutations
of items.
Example: (permutations [1 2 3]) -> ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
Example: (permutations [1 1 2]) -> ((1 1 2) (1 2 1) (2 1 1))

(count-permutations items) - (count (permutations items)), but computed more directly
(nth-permutation items n) - (nth (permutations items)), but computed more directly
(drop-permutations items n) - (drop n (permutations items)), but computed more directly
(permutation-index items) - Returns the number n where (nth-permutation (sort items) n) is items

(partitions items) - A lazy sequence of all the partitions
of items.
Example: (partitions [1 2 3]) -> (([1 2 3])
                                  ([1 2] [3])
                                  ([1 3] [2])
                                  ([1] [2 3])
                                  ([1] [2] [3]))
Example: (partitions [1 1 2]) -> (([1 1 2])
                                  ([1 1] [2])
                                  ([1 2] [1])
                                  ([1] [1] [2]))
Example: (partitions [1 1 2 2] :min 2 :max 3) -> (([1 1 2] [2])
                                                  ([1 1] [2 2])
                                                  ([1 1] [2] [2])
                                                  ([1 2 2] [1])
                                                  ([1 2] [1 2])
                                                  ([1 2] [1] [2])
                                                  ([1] [1] [2 2]))

About this code:
These combinatorial functions can be written in an elegant way using recursion.  However, when dealing with combinations and permutations, you're usually generating large numbers of things, and speed counts.  My objective was to write the fastest possible code I could, restricting myself to Clojure's functional, persistent data structures (rather than using Java's arrays) so that this code could be safely leveraged within Clojure's transactional concurrency system.

I also restricted myself to algorithms that return results in a standard order.  For example, there are faster ways to generate cartesian-product, but I don't know of a faster way to generate the results in the standard nested-for-loop order.

Most of these algorithms are derived from algorithms found in Knuth's wonderful Art of Computer Programming books (specifically, the volume 4 fascicles), which present fast, iterative solutions to these common combinatorial problems.  Unfortunately, these iterative versions are somewhat inscrutable.  If you want to better understand these algorithms, the Knuth books are the place to start.
"
)

(defn- all-different?
  "Annoyingly, the built-in distinct? doesn't handle 0 args, so we need
to write our own version that considers the empty-list to be distinct"
  [s]
  (if (seq s)
    (apply distinct? s)
    true))

(defmacro assert-with-message
  "Clojure 1.2 didn't allow asserts with a message, so we roll our own here for backwards compatibility"
  [x message]
  (when *assert*
    `(when-not ~x
       (throw (new AssertionError (str "Assert failed: " ~message "\n" (pr-str '~x)))))))

;; so this code works with both 1.2.x and 1.3.0:
(def ^{:private true} plus (first [+' +]))
(def ^{:private true} mult (first [*' *]))

(defn- index-combinations
  [n cnt]
  (lazy-seq
    (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
          iter-comb
          (fn iter-comb [c j]
            (if (> j n) nil
              (let [c (assoc c j (dec (c j)))]
                (if (< (c j) j) [c (inc j)]
                  (loop [c c, j j]
                    (if (= j 1) [c j]
                      (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
          step
          (fn step [c j]
            (cons (rseq (subvec c 1 (inc n)))
                  (lazy-seq (let [next-step (iter-comb c j)]
                              (when next-step (step (next-step 0) (next-step 1)))))))]
      (step c 1))))

;; Helper function for bounded-distributions
(defn- distribute [m index total distribution already-distributed]
  (loop [distribution distribution
         index index
         already-distributed already-distributed]
    (if (>= index (count m)) nil
      (let [quantity-to-distribute (- total already-distributed)
            mi (m index)]
        (if (<= quantity-to-distribute mi)
          (conj distribution [index quantity-to-distribute total])
          (recur (conj distribution [index mi (+ already-distributed mi)])
                 (inc index)
                 (+ already-distributed mi))))))) 

;; Helper function for bounded-distributions
(defn- next-distribution [m total distribution]
  (let [[index this-bucket this-and-to-the-left] (peek distribution)]
    (cond
      (< index (dec (count m)))
      (if (= this-bucket 1)
        (conj (pop distribution) [(inc index) 1 this-and-to-the-left])
        (conj (pop distribution) 
              [index (dec this-bucket) (dec this-and-to-the-left)]
              [(inc index) 1 this-and-to-the-left])),
      ; so we have stuff in the last bucket
      (= this-bucket total) nil
      :else
      (loop [distribution (pop distribution)],
        (let
          [[index this-bucket this-and-to-the-left] (peek distribution),
           distribution (if (= this-bucket 1) 
                          (pop distribution)
                          (conj (pop distribution)
                                [index (dec this-bucket) (dec this-and-to-the-left)]))],
          (cond
            (<= (- total (dec this-and-to-the-left)) (apply + (subvec m (inc index))))       
            (distribute m (inc index) total distribution (dec this-and-to-the-left)),
            
            (seq distribution) (recur distribution)
            :else nil))))))
      
;; Helper function for multi-comb
(defn- bounded-distributions
  [m t]
  (let [step 
        (fn step [distribution]
          (cons distribution
                (lazy-seq (when-let [next-step (next-distribution m t distribution)]
                            (step next-step)))))]
    (step (distribute m 0 t [] 0))))

;; Combinations of multisets
;; The algorithm in Knuth generates in the wrong order, so this is a new algorithm
(defn- multi-comb
  "Handles the case when you want the combinations of a list with duplicate items."
  [l t]
  (let [f (frequencies l),
        v (vec (distinct l)),
        domain (range (count v))
        m (vec (for [i domain] (f (v i))))
        qs (bounded-distributions m t)]
    (for [q qs]
      (apply concat
             (for [[index this-bucket _] q]
               (repeat this-bucket (v index)))))))
                
(defn combinations
  "All the unique ways of taking t different elements from items"
  [items t]      
  (let [v-items (vec (reverse items))]
    (if (zero? t) (list ())
      (let [cnt (count items)]
        (cond (> t cnt) nil
              (= t 1) (for [item (distinct items)] (list item))
              (all-different? items) (if (= t cnt) 
                                        (list (seq items))
                                        (map #(map v-items %) (index-combinations t cnt))),
              :else (multi-comb items t))))))

(defn- unchunk
  "Given a sequence that may have chunks, return a sequence that is 1-at-a-time
lazy with no chunks. Chunks are good for efficiency when the data items are
small, but when being processed via map, for example, a reference is kept to
every function result in the chunk until the entire chunk has been processed,
which increases the amount of memory in use that cannot be garbage
collected."
  [s]
  (lazy-seq
    (when (seq s)
      (cons (first s) (unchunk (rest s))))))

            (defn subsets
  "All the subsets of items"
  [items]
  (mapcat (fn [n] (combinations items n))
          (unchunk (range (inc (count items))))))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                      (if-let [rst (next (v-seqs i))]
                        (assoc v-seqs i rst)
                        (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))


(defn selections
  "All the ways of taking n (possibly the same) elements from the sequence of items"
  [items n]
  (apply cartesian-product (take n (repeat items))))


(defn- iter-perm [v]
  (let [len (count v),
        j (loop [i (- len 2)]
            (cond (= i -1) nil
                  (< (v i) (v (inc i))) i
                  :else (recur (dec i))))]
    (when j
      (let [vj (v j),
            l (loop [i (dec len)]
                (if (< vj (v i)) i (recur (dec i))))]
        (loop [v (assoc v j (v l) l vj), k (inc j), l (dec len)]
          (if (< k l)
            (recur (assoc v k (v l) l (v k)) (inc k) (dec l))
            v))))))

(defn- vec-lex-permutations [v]
  (when v (cons v (lazy-seq (vec-lex-permutations (iter-perm v))))))

(defn- lex-permutations
  "DEPRECATED as a public function.

In prior versions of the combinatorics library, there were two similar functions: permutations and lex-permutations.  It was a source of confusion to know which to call.  Now, you can always call permutations.  When appropriate (i.e., when you pass in a sorted sequence of numbers), permutations will automatically call lex-permutations as a speed optimization."
  {:deprecated "1.3"}
  [c]
  (lazy-seq
    (let [vec-sorted (vec (sort c))]
      (if (zero? (count vec-sorted))
        (list [])
        (vec-lex-permutations vec-sorted)))))

(defn- sorted-numbers?
  "Returns true iff s is a sequence of numbers in non-decreasing order"
  [s]
              (and (every? number? s)
       (or (empty? s) (apply <= s))))

(defn- multi-perm
  "Handles the case when you want the permutations of a list with duplicate items."
  [l]
  (let [f (frequencies l),
        v (vec (distinct l)),
        indices (apply concat
                       (for [i (range (count v))]
                         (repeat (f (v i)) i)))]
    (map (partial map v) (lex-permutations indices))))

(defn permutations
  "All the distinct permutations of items, lexicographic by index 
(special handling for duplicate items)."
  [items]
  (cond
    (sorted-numbers? items) (lex-permutations items),
    
    (all-different? items)
    (let [v (vec items)]
      (map #(map v %) (lex-permutations (range (count v)))))
    
    :else
    (multi-perm items)))

;; Jumping directly to a given permutation

;; First, let's deal with the case where all items are distinct
;; This is the easier case.

(defn- factorial [n]
  {:pre [(integer? n) (not (neg? n))]}
  (loop [acc 1, n n]
    (if (zero? n) acc (recur (mult acc n) (dec n)))))

(defn- factorial-numbers
  "Input is a non-negative base 10 integer, output is the number in the
factorial number system (http://en.wikipedia.org/wiki/Factorial_number_system)
expressed as a list of 'digits'" 
  [n]
  {:pre [(integer? n) (not (neg? n))]}
  (loop [n n, digits (), divisor 1]
    (if (zero? n) 
      digits
      (let [q (quot n divisor), r (rem n divisor)]
        (recur q (cons r digits) (inc divisor))))))

(defn- remove-nth [l n]
  (loop [acc [], l l, n n]
    (if (zero? n) (into acc (rest l))
      (recur (conj acc (first l)) (rest l) (dec n)))))

(defn- nth-permutation-distinct
  "Input should be a sorted sequential collection l of distinct items, 
output is nth-permutation (0-based)"
  [l n]
  (assert-with-message (< n (factorial (count l))) 
                       (format "%s is too large. Input has only %s permutations."
                               (str n) (str (factorial (count l)))))
  (let [length (count l)
        fact-nums (factorial-numbers n)]
    (loop [indices (concat (repeat (- length (count fact-nums)) 0)
                           fact-nums),
           l l
           perm []]
      (if (empty? indices) perm
        (let [i (first indices),
              item (nth l i)]
          (recur (rest indices) (remove-nth l i) (conj perm item))))))) 

;; Now we generalize to collections with duplicates

(defn- count-permutations-from-frequencies [freqs]
  (let [counts (vals freqs)]
    (reduce / (factorial (apply + counts))
            (map factorial counts ))))

(defn count-permutations
  "Counts the number of distinct permutations of l"
  [l]
  (if (all-different? l) 
    (factorial (count l))
    (count-permutations-from-frequencies (frequencies l))))    

(defn- initial-perm-numbers
  "Takes a sorted frequency map and returns how far into the sequence of
lexicographic permutations you get by varying the first item"
  [freqs]
  (reductions + 0
              (for [[k v] freqs]
                (count-permutations-from-frequencies (assoc freqs k (dec v))))))

;; Explanation of initial-perm-numbers:
; (initial-perm-numbers (sorted-map 1 2, 2 1)) => (0 2 3) because when
; doing the permutations of [1 1 2], there are 2 permutations starting with 1
; and 1 permutation starting with 2.
; So the permutations starting with 1 begin with the 0th permutation
; and the permutations starting with 2 begin with the 2nd permutation
; (The final 3 denotes the total number of permutations).

(defn- index-remainder
  "Finds the index and remainder from the initial-perm-numbers."
  [perm-numbers n]
  (loop [perm-numbers perm-numbers
         index 0]
    (if (and (<= (first perm-numbers) n)
             (< n (second perm-numbers)))
      [index (- n (first perm-numbers))]
      (recur (rest perm-numbers) (inc index)))))

;; Explanation of index-remainder:
; (index-remainder [0 6 9 11] 8) => [1 2]
; because 8 is (+ (nth [0 6 9 11] 1) 2)
; i.e., 1 gives us the index into the largest number smaller than n
; and 2 is the remaining amount needed to sum up to n.

(defn- dec-key [m k]
  (if (= 1 (m k))
    (dissoc m k)
    (update-in m [k] dec)))

(defn- factorial-numbers-with-duplicates
  "Input is a non-negative base 10 integer n, and a sorted frequency map freqs.
Output is a list of 'digits' in this wacky duplicate factorial number system" 
  [n freqs]
  (loop [n n, digits [], freqs freqs]
    (if (zero? n) (into digits (repeat (apply + (vals freqs)) 0))
      (let [[index remainder] 
            (index-remainder (initial-perm-numbers freqs) n)]
        (recur remainder (conj digits index)
               (let [nth-key (nth (keys freqs) index)]
                 (dec-key freqs nth-key)))))))

(defn- nth-permutation-duplicates
  "Input should be a sorted sequential collection l of distinct items, 
output is nth-permutation (0-based)"
  [l n]
  (assert-with-message (< n (count-permutations l)) 
                       (format "%s is too large. Input has only %s permutations."
                               (str n) (str (count-permutations l))))
  (loop [freqs (into (sorted-map) (frequencies l)),
         indices (factorial-numbers-with-duplicates n freqs)
         perm []]
    (if (empty? indices) perm
      (let [i (first indices),
            item (nth (keys freqs) i)]
        (recur (dec-key freqs item)
               (rest indices) 
               (conj perm item))))))

;; Now we create the public version, which detects which underlying algorithm to call

(defn nth-permutation
  "(nth (permutations items)) but calculated more directly."
  [items n]
  (if (sorted-numbers? items)
    (if (all-different? items) 
      (nth-permutation-distinct items n)
      (nth-permutation-duplicates items n))
    (if (all-different? items)
      (let [v (vec items),
            perm-indices (nth-permutation-distinct (range (count items)) n)]
        (vec (map v perm-indices)))
      (let [v (vec (distinct items)),
            f (frequencies items),
            indices (apply concat
                           (for [i (range (count v))]
                             (repeat (f (v i)) i)))]
        (vec (map v (nth-permutation-duplicates indices n)))))))

(defn drop-permutations
  "(drop n (permutations items)) but calculated more directly."
  [items n]
  (cond
    (zero? n) (permutations items)
    (= n (count-permutations items)) ()
    :else
    (if (sorted-numbers? items)
      (if (all-different? items) 
        (vec-lex-permutations (nth-permutation-distinct items n))
        (vec-lex-permutations (nth-permutation-duplicates items n)))
      (if (all-different? items)
        (let [v (vec items),
              perm-indices (nth-permutation-distinct (range (count items)) n)]
          (map (partial map v) (vec-lex-permutations perm-indices)))
        (let [v (vec (distinct items)),
              f (frequencies items),
              indices (apply concat
                             (for [i (range (count v))]
                               (repeat (f (v i)) i)))]
          (map (partial map v) 
               (vec-lex-permutations
                 (nth-permutation-duplicates indices n))))))))

;; Let's do the same thing now for combinations

(defn- n-take-k [n k]
  (cond
    (< k 0) 0
    (> k n) 0
    (zero? k) 1
    (= k 1) n
    (> k (quot n 2)) (recur n (- n k))
    :else (/ (apply mult (range (inc (- n k)) (inc n)))
             (apply mult (range 1 (inc k))))))

(defn- ^{:dynamic true} count-combinations-from-frequencies [freqs t]
  (let [counts (vals freqs)
        sum (apply + counts)]
    (cond 
      (zero? t) 1
      (= t 1) (count freqs)
      (every? #{1} counts) (n-take-k (count freqs) t)
      (> t sum) 0
      (= t sum) 1
      (= (count freqs) 1) 1
      :else
      (let [new-freqs (dec-key freqs (first (keys freqs)))]
        (plus (count-combinations-from-frequencies new-freqs (dec t))
              (count-combinations-from-frequencies (dissoc freqs (first (keys freqs))) t))))))

(defn- count-combinations-unmemoized
  "We need an internal version that doesn't memoize each call to count-combinations-from-frequencies
so that we can memoize over a series of calls."
  [items t]
  (if (all-different? items)
    (n-take-k (count items) t)
    (count-combinations-from-frequencies (frequencies items) t)))

(defn count-combinations
  "(count (combinations items t)) but computed more directly"
  [items t]
  (binding [count-combinations-from-frequencies (memoize count-combinations-from-frequencies)]
    (count-combinations-unmemoized items t)))

(defn- expt-int [base pow]
  (loop [n pow, y 1, z base]
    (let [t (even? n), n (quot n 2)]
      (cond
       t (recur n y (mult z z))
       (zero? n) (mult z y)
       :else (recur n (mult z y) (mult z z))))))

(defn- count-subsets-unmemoized
  [items]
  (cond 
    (empty? items) 1
    (all-different? items) (expt-int 2 (count items))
    :else (apply plus (for [i (range 0 (inc (count items)))]
                        (count-combinations-unmemoized items i)))))

(defn count-subsets
  "(count (subsets items)) but computed more directly"  
  [items]
  (binding [count-combinations-from-frequencies
            (memoize count-combinations-from-frequencies)]
    (count-subsets-unmemoized items)))

(defn- nth-combination-distinct
  "The nth element of the sequence of t-combinations of items,
where items is a collection of distinct elements"
  [items t n]
  (loop [comb []
         items items,
         t t,
         n n]
    (if (or (zero? n) (empty? items)) (into comb (take t items))
      (let [dc-dt (n-take-k (dec (count items)) (dec t))]
        (if (< n dc-dt) 
          (recur (conj comb (first items)) (rest items) (dec t) n)
          (recur comb (rest items) t (- n dc-dt)))))))
                         
(defn- nth-combination-freqs
  "The nth element of the sequence of t-combinations of the multiset
represented by freqs"
  [freqs t n]
  (loop [comb [],
         freqs freqs,
         t t, 
         n n]
    (if (or (zero? n) (empty? freqs)) 
      (into comb (take t (apply concat (for [[k v] freqs] (repeat v k)))))
      (let [first-key (first (keys freqs)),
            remove-one-key (dec-key freqs first-key)
            dc-dt (count-combinations-from-frequencies remove-one-key (dec t))]
        (if (< n dc-dt)
          (recur (conj comb first-key) remove-one-key (dec t) n)
          (recur comb (dissoc freqs first-key) t (- n dc-dt)))))))
              
(defn nth-combination
  "The nth element of the sequence of t-combinations of items"
  [items t n]
  (assert-with-message (< n (count-combinations items t))
                       (format "%s is too large. Input has only %s combinations."
                               (str n) (str (count-combinations-unmemoized items t))))
  (if (all-different? items)
    (nth-combination-distinct items t n)
    (binding [count-combinations-from-frequencies (memoize count-combinations-from-frequencies)]
      (let [v (vec (distinct items))
            f (frequencies items),
            indices (apply concat
                           (for [i (range (count v))]
                             (repeat (f (v i)) i)))
            indices-freqs (into (sorted-map) (frequencies indices))]
        (vec (map v (nth-combination-freqs indices-freqs t n)))))))

(defn nth-subset
  [items n]
  (assert-with-message (< n (count-subsets items))
                       (format "%s is too large. Input has only %s subsets."
                               (str n) (str (count-subsets items))))
  (loop [size 0,
         n n]
    (let [num-combinations (count-combinations items size)]
      (if (< n num-combinations)
        (nth-combination items size n)
        (recur (inc size) (- n num-combinations)))))) 

;; Now let's go the other direction, from a sortable collection to the nth
;; position in which we would find the collection in the lexicographic sequence
;; of permutations
    
(defn- list-index
  "The opposite of nth, i.e., from an item in a list, find the n"
  [l item]
  (loop [l l, n 0]
    (assert (seq l))
    (if (= item (first l)) n
      (recur (rest l) (inc n)))))

(defn- permutation-index-distinct
  [l]
  (loop [l l, index 0, n (dec (count l))]
    (if (empty? l) index
      (recur (rest l) 
             (+ index (* (factorial n) (list-index (sort l) (first l))))
             (dec n)))))

(defn- permutation-index-duplicates
  [l]
  (loop [l l, index 0, freqs (into (sorted-map) (frequencies l))]
    (if (empty? l) index
      (recur (rest l)
             (reduce + index 
                     (for [k (take-while #(neg? (compare % (first l))) (keys freqs))]
                       (count-permutations-from-frequencies (dec-key freqs k))))
             (dec-key freqs (first l))))))

(defn permutation-index
  "Input must be a sortable collection of items.  Returns the n such that
    (nth-permutation (sort items) n) is items."
  [items]
  (if (all-different? items)
    (permutation-index-distinct items)
    (permutation-index-duplicates items)))

;;;;; Partitions, written by Alex Engelberg; adapted from Knuth Volume 4A

;;;;; Partitions - Algorithm H

; The idea in Algorithm H is to find the lexicographic "growth string" vectors, mapping each index
; in 0..N-1 to the partition it belongs to, for all indices in 0..N-1.
; Example: for the partition ([0 2] [1] [3]), the corresponding growth string would be [0 1 0 2].

; The rule for each growth string L is that for each i in 0..N-1,
; L[i] <= max(L[0] ... L[i-1]) + 1

; During the course of the algorithm, I keep track of two vectors, a and b.
; For each i in 0..N-1, a[i] = L[i], and b[i] = max(L[0] ... L[i-1]) + 1.

; "r" is the maximum partition count, and "s" is the minimum. You can also think of these as being
; the bounds of the maximum number in each growth string.

(defn- update
  [vec index f]
  (let [item (vec index)]
    (assoc vec index (f item))))

(defmacro ^:private reify-bool
  [statement]
  `(if ~statement 1 0))

(defn- init
  [n s]
  (if s
    (vec (for [i (range 1 (inc n))]
           (max 0 (- i (- n s -1)))))
    (vec (repeat n 0))))

(defn- growth-strings-H
  ([n r s] ; H1
           (growth-strings-H n
                             (init n s)
                             (vec (repeat n 1))
                             r
                             s))
  ([n a b r s]
    (cons a   ; begin H2
          (lazy-seq
            (if (and (> (peek b) (peek a))
                     (if r (< (peek a) (dec r)) true)) ; end H2
              (growth-strings-H n (update a (dec n) inc) b r s)  ; H3
              (let [j (loop [j (- n 2)] ; begin H4
                        (if (and (< (a j) (b j))
                                 (if r
                                   (< (a j) (dec r))
                                   true)
                                 (if s
                                   (<= (- s (b j) (reify-bool (== (inc (a j)) (b j)))) (- n j))
                                   true))
                          j
                          (recur (dec j))))] ; end H4
                (if (zero? j) ;begin H5
                  ()
                  (let [a (update a j inc) ; end H5
                        x (when s
                            (- s
                               (+ (b j)
                                  (reify-bool (= (a j) (b j))))))
                        [a b] (loop [a a
                                     b b
                                     i (inc j)
                                     current-max (+ (b j)
                                                    (reify-bool (== (b j) (a j))))]
                                (cond
                                  (== i n) [a b]
                                  
                                  (and s (> i (- (- n x) 1)))
                                  (let [new-a-i (+ (- i n) s)]
                                    (recur (assoc a i new-a-i)
                                           (assoc b i current-max)
                                           (inc i)
                                           (max current-max (inc new-a-i))))
                                  
                                  :else (recur (assoc a i 0)
                                               (assoc b i current-max)
                                               (inc i)
                                               current-max)))]
                    (growth-strings-H n a b r s))))))))) ;end H6

(defn- lex-partitions-H
  [N & {from :min to :max}]
  (if (= N 0)
    (if (<= (or from 0) 0 (or to 0))
      '(())
      ())
    (let [from (if (and from (<= from 1)) nil from)
          to (if (and to (>= to N)) nil to)]
      (cond
        (not (<= 1 (or from 1) (or to N) N)) ()
        
        (= N 0) '(())
        (= N 1) '(([0]))
        (= to 1) `((~(range N)))
        :else (let [growth-strings (growth-strings-H N to from)]
                (for [growth-string growth-strings
                      :let [groups (group-by growth-string (range N))]]
                  (map groups (range (count groups)))))))))

(defn- partitions-H
  [items & args]
  (let [items (vec items)
        N (count items)
        lex (apply lex-partitions-H N args)]
    (for [parts lex]
      (for [part parts]
        (-> (reduce (fn [v o] (conj! v (items o))) (transient []) part) ; mapv
          persistent!)))))

;;;;;; Partitions - Algorithm M

; In Algorithm M, the idea is to find the partitions of a list of items that may contain duplicates.
; Within the algorithm, the collections are stored as "multisets," which are maps that map items
; to their frequency. (keyval pairs with a value of 0 are not included.) Note that in this algorithm,
; the multisets are not stored as maps, but all multisets are stored together across multiple vectors.

; Here is what the internal vectors/variables will look like when the algorithm is visiting the
; partition ([1 1 2 2 2] [1 2] [1]):

; c[i] =      1 2|1 2|1
; v[i] =      2 3|1 1|1
; u[i] =      4 4|2 1|1
; ---------------------------
;    i =      0 1 2 3 4 5
; f[x]=i:     0   1   2 3
; l = 2
; n = 8
; m = 2

; You can think of (c,v) and (c,u) as the (keys,vals) pairs of two multisets.
; u[i] represents how many c[i]'s were left before choosing the v values for the current partition.
; (Note that v[i] could be 0 if u[i] is not 0.)
; f[x] says where to begin looking in c, u, and v, to find information about the xth partition.
; l is the number of partitions minus one.
; n is the total amount of all items (including duplicates).
; m is the total amount of distinct items.

; During the algorithm, a and b are temporary variables that end up as f(l) and f(l+1).
; In other words, they represent the boundaries of the "workspace" of the most recently written-out partition.

(declare m5 m6)

(defn- multiset-partitions-M
  ([multiset r s] ; M1
                  (let [n (apply + (vals multiset))
                        m (count multiset)
                        f []
                        c []
                        u []
                        v []
                        ; these vectors will grow over time, as new values are assoc'd into the next spots.
                        [c u v] (loop [j 0, c c, u u, v v]
                                  (if (= j m)
                                    [c u v]
                                    (recur (inc j)
                                           (assoc c j (inc j))
                                           (assoc u j (multiset (inc j)))
                                           (assoc v j (multiset (inc j))))))
                        a 0, b m, l 0
                        f (assoc f 0 0, 1 m)
                        stack ()]
                    (multiset-partitions-M n m f c u v a b l r s)))
  ([n m f c u v a b l r s]
    (let [[u v c j k] (loop [j a, k b, x false     ; M2
                             u u, v v, c c]
                        (if (>= j b)
                          [u v c j k]
                          (let [u (assoc u k (- (u j) (v j)))]
                            (if (= (u k) 0)
                              (recur (inc j), k, true
                                     u, v, c)
                              (if-not x
                                (let [c (assoc c k (c j))
                                      v (assoc v k (min (v j) (u k)))
                                      x (< (u k) (v j))
                                      k (inc k)
                                      j (inc j)]
                                  (recur j, k, x
                                         u, v, c))
                                (let [c (assoc c k (c j))
                                      v (assoc v k (u k))
                                      k (inc k)
                                      j (inc j)]
                                  (recur j, k, x
                                         u, v, c)))))))]
      (cond  ; M3
             (and r
                  (> k b)
                  (= l (dec r))) (m5 n m f c u v a b l r s)
             (and s
                  (<= k b)
                  (< (inc l) s)) (m5 n m f c u v a b l r s)
             (> k b) (let [a b, b k, l (inc l)
                           f (assoc f (inc l) b)]
                       (recur n m f c u v a b l r s))
             :else (let [part (for [y (range (inc l))]
                                (let [first-col (f y)
                                      last-col (dec (f (inc y)))]
                                  (into {} (for [z (range first-col (inc last-col))
                                                 :when (not= (v z) 0)]
                                             [(c z) (v z)]))))]
                     (cons part ; M4
                           (lazy-seq (m5 n m f c u v a b l r s))))))))

(defn- m5  ; M5
  [n m f c u v a b l r s]
  (let [j (loop [j (dec b)]
            (if (not= (v j) 0)
              j
              (recur (dec j))))]
    (cond
      (and r
           (= j a)
           (< (* (dec (v j)) (- r l))
              (u j))) (m6 n m f c u v a b l r s)
      (and (= j a)
           (= (v j) 1)) (m6 n m f c u v a b l r s)
      :else (let [v (update v j dec)
                  diff-uv (if s (apply + (for [i (range a (inc j))]
                                           (- (u i) (v i)))) nil)
                  v (loop [ks (range (inc j) b)
                           v v]
                      (if (empty? ks)
                        v
                        (let [k (first ks)]
                          (recur (rest ks)
                                 (assoc v k (u k))))))
                  min-partitions-after-this (if s (- s (inc l)) 0)
                  amount-to-dec (if s (max 0 (- min-partitions-after-this diff-uv)) 0)
                  v (if (= amount-to-dec 0)
                      v
                      (loop [k-1 (dec b), v v
                             amount amount-to-dec]
                        (let [vk (v k-1)]
                          (if (> amount vk)
                            (recur (dec k-1)
                                   (assoc v k-1 0)
                                   (- amount vk))
                            (assoc v k-1 (- vk amount))))))]
              (multiset-partitions-M n m f c u v a b l r s)))))

(defn- m6  ; M6
  [n m f c u v a b l r s]
  (if (= l 0)
    ()
    (let [l (dec l)
          b a
          a (f l)]
      (m5 n m f c u v a b l r s))))

(defn- partitions-M
  [items & {from :min to :max}]
  (if (= (count items) 0)
    (if (<= (or from 0) 0 (or to 0))
      '(())
      ())
    (let [items (vec items)
          ditems (vec (distinct items))
          freqs (frequencies items)
          N (count items)
          M (count ditems)
          from (if (and from (<= from 1)) nil from)
          to (if (and to (>= to N)) nil to)]
      (cond
        (not (<= 1 (or from 1) (or to N) N)) ()
        (= N 1) `(([~(first items)]))
        :else (let [start-multiset (into {} (for [i (range M)
                                                  :let [j (inc i)]]
                                              [j (freqs (ditems i))]))
                    parts (multiset-partitions-M start-multiset to from)]
                (->> multiset
                  (mapcat (fn [[index numtimes]] (repeat numtimes (ditems (dec index)))))
                  vec
                  (for [multiset part])
                  (for [part parts])))))))

(defn partitions
  "All the lexicographic distinct partitions of items.
    Optionally pass in :min and/or :max to specify inclusive bounds on the number of parts the items can be split into."
  [items & args]
  (cond
    (= (count items) 0) (apply partitions-H items args)
    (all-different? items) (apply partitions-H items args)
    :else (apply partitions-M items args)))