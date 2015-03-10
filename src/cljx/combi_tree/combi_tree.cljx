(ns combi-tree.combi-tree
  "Combinatorics functions based on building a combination tree."
  {:author "François Rey"}
  (:require [clojure.set :refer [union]]
            [clojure.zip :as zip]))

; First a few functions for adding meta-data marker to the tree data structures.
; This marker is used to distinguish the combinatoric tree data structures from
; the data structures in the tree.

(defn with-tree-meta
  "Returns obj as a list with ::tree meta-data set to nil.
  An optional meta data map can be passed to be added as well."
  ([obj]
    (when obj
      (let [l #+clj obj #+cljs (if (instance? IndexedSeq obj) ; IndexedSeq has
                                 (apply list obj) ; no meta-data in cljs
                                 obj)]
        (with-meta l {::tree true}))))
  ([obj m]
    (when obj
      (let [l #+clj obj #+cljs (if (instance? IndexedSeq obj) ; IndexedSeq has
                                 (apply list obj) ; no meta-data in cljs
                                 obj)]
        (with-meta l (assoc m ::tree true))))))

(defn tree-seq?
  "Returns true if the given object is a seq with a ::tree meta-data."
  [o]
  (and (seq? o) (contains? (meta o) ::tree)))

(def empty-tree-list (with-tree-meta ()))
(defn tree-list
  "Creates a list with a marker meta-data and containing the given elements.
  Used in combinations trees to distinguishing sequences that are part of the
  tree structure from sequences that are node values."
  [& elements]
  (if elements
    (with-tree-meta elements)
    empty-tree-list))

(defn tree-cons
  "Creates a cons with a marker meta-data and containing the given elements.
  Used in combinations trees to distinguishing sequences that are part of the
  tree structure from sequences that are node values."
  [x seq]
  (with-tree-meta (cons x seq)))

(defn tree-rest
  "Returns the rest of the given coll with a marker meta-data.
  Used in combinations trees to distinguishing sequences that are part of the
  tree structure from sequences that are node values."
  [coll]
  (if-let [n (next coll)]
    (with-tree-meta n)
    empty-tree-list))

(def zero-combinations-tree
  (sorted-map 0 empty-tree-list))

(defn tree-conj
  "Add a marker meta-data to the result of calling conj on the given arguments.
  Used in combinations trees to distinguishing sequences that are part of the
  tree structure from sequences that are node values."
  ([coll x]
    (with-tree-meta (conj coll x)))
  ([coll x & xs]
    (if xs
      (recur (tree-conj coll x) (first xs) (next xs))
      (tree-conj coll x))))

(defn combinations-tree
  "Returns nested and mostly lazy sequences representing a tree of all the
  unique ways of taking n different elements from a sequence (i.e. elements at
  n distinct positions in the sequence) while preserving their original order.
  A branch in the tree is a sequence of at least 2 items, the first being
  the node and the rest being the children of that node. Nodes and leafs are
  items of the original collection so that each path in the tree provides a
  possible combination. The result of this function is a sequence of root
  children, meaning there is no root node in the first position.
  If the original collection contains duplicates, the resulting combinations
  will also contain duplicates. Use distinct-combinations-tree to get a tree
  that yields no duplicates.
  Examples:
    (combinations-tree [:k :s :k :s] 2)
    => ((:k :s :k :s) (:s :k :s) (:k :s))
    (combinations-tree [:k :s :k :s] 3)
    => ((:k (:s :k :s) (:k :s)) (:s (:k :s)))
  When only coll is provided as argument, this function returns a map of
  all combinations trees indexed by their size:
    (combinations-tree [:k :s :k :s])
    => {0 ()
        1 (:k :s :k :s), 
        2 ((:k :s :k :s) (:s :k :s) (:k :s)), 
        3 ((:k (:s :k :s) (:k :s)) (:s (:k :s))), 
        4 ((:k (:s (:k :s))))}
  Returns nil if n is 0 or greater than the size of coll.
  Throws an exception if n is negative or nil."
  ([coll]
    (if (seq coll)
      (into zero-combinations-tree
            (mapv #(vector % (combinations-tree coll %))
                  (range 1 (inc (count coll)))))
      zero-combinations-tree))
  ([coll n]
    (when (or (nil? n) (neg? n))
       (let [m (str "Expecting zero or positive integer, got " n)]
         #+cljs (throw (js/Error. m))
         #+clj  (throw (RuntimeException. m))))
    (letfn [(build-tree
              [coll n]
              (if (zero? n)
                empty-tree-list
                (with-tree-meta
                  (lazy-seq
                    (let [cc (count coll)
                         rc (rest coll)]
                      (if (= n cc)
                        (if (<= n 2)
                          (with-tree-meta coll)
                          (tree-list (first coll) (build-tree rc (dec n))))
                        (when (< n cc)
                          (if (= n 1)
                            (with-tree-meta coll)
                            (tree-cons
                              (tree-cons (first coll) (build-tree rc (dec n)))
                              (if (= 1 (- cc n))
                                (tree-list (build-tree rc n))
                                (build-tree rc n)))))))))))]
      (cond
        (or (zero? n) (nil? coll) (< (count coll) n)) empty-tree-list
        :else (let [t (build-tree coll n)
                    c (count coll)]
                (if (and (> c 1) (= n c))
                  (tree-list t)
                  t))))))

(defn distinct-combinations-tree
  "Same as combinations-tree, except that trees corresponding to duplicate
  combinations are merged into a single path terminating by a ::dups leaf.
  This function can accept a single parameter, which can either be a collection
  for which combinations tree must be computed, or a tree as returned by
  combinations-tree which must satisfy tree-seq?."
  ([coll-or-combinations-tree]
    (cond
      (nil? coll-or-combinations-tree) zero-combinations-tree
      (tree-seq? coll-or-combinations-tree)
        (if (empty? coll-or-combinations-tree)
          empty-tree-list
          (letfn [; returns a map of duplicate nodes and their subtrees
                  (dups [tree]
                    (->> tree 
                      (map #(if (tree-seq? %)
                              [(first %) (tree-rest %)]
                              [% nil]))
                      (reduce (fn [[seen dups] [node subtree]]
                                (if (seen node)
                                  [(assoc seen node (conj (seen node) subtree))
                                   (conj dups node)]
                                  [(assoc seen node [subtree]) dups]))
                              [{} #{}])
                      (apply select-keys)))
                  (nil-or-dup? [x] (or (nil? x) (= ::dups x)))
                  (merge-trees [trees]
                    ;(println "  merge" trees)
                    (if (every? nil-or-dup? trees)
                      (tree-list ::dups)
                      (walk-tree (with-tree-meta (apply concat trees)))))
                  (walk-tree [tree]
                    (when (tree-seq? tree)
                      (let [dups (dups tree)]
                        ;(println "(walk-tree " tree "), dups=" dups)
                        (->> tree 
                          (reduce (fn [[walked-tree dups-seen :as r] branch]
                                    ;(println "  reduce" r branch)
                                    (let [node (if (tree-seq? branch)
                                                 (first branch)
                                                 branch)
                                          subtree (if (tree-seq? branch)
                                                    (tree-rest branch)
                                                    nil)]
                                      (if-let [dup-subtrees (dups node)]
                                       (if (dups-seen node)
                                         [walked-tree dups-seen]
                                         [(conj walked-tree
                                                (tree-cons node
                                                    (merge-trees dup-subtrees)))
                                          (conj dups-seen node)])
                                       (if (seq subtree)
                                         [(conj walked-tree
                                                (tree-cons node
                                                           (walk-tree subtree)))
                                          dups-seen]
                                         [(conj walked-tree node) dups-seen]))))
                                  [[] #{}])
                          (first)
                          (seq)
                          (with-tree-meta)))))]
            (walk-tree coll-or-combinations-tree)))
      (seq coll-or-combinations-tree)
        (into zero-combinations-tree
              (mapv #(vector % (distinct-combinations-tree
                                 coll-or-combinations-tree %))
                    (range 1 (inc (count coll-or-combinations-tree)))))
      :else zero-combinations-tree))
  ([coll n]
    (distinct-combinations-tree (combinations-tree coll n))))

(defn combinations-zip
  "Returns a clojure.zip zipper on a given combinations tree. Operates on the
  result of combinations-tree or distinct-combinations-tree. However these trees
  have no real root node, so for zipper to work a dummy nil root node is added
  to the tree passed as argument."
  [combinations-tree]
  (zip/zipper tree-seq?
              tree-rest
              (fn [node children]
                (with-tree-meta (tree-cons node children) (meta node)))
              (tree-cons nil combinations-tree)))

(defn tree->combinations
  "Returns a lazy sequence of the combinations derived from a tree as returned
  by combinations-tree or distinct-combinations-tree. An optional boolean
  indicates if the result should include combinations derived from paths 
  terminating by ::dups."
  ([tree] (tree->combinations tree false))
  ([tree remove-dups?]
    (let [filter-fn (if remove-dups?
                      #(not (or (zip/branch? %) (= (zip/node %) ::dups)))
                      (complement zip/branch?))
          z (when (seq tree) (combinations-zip tree))
          leaves (when z (filter filter-fn (take-while (complement zip/end?)
                                                      (iterate zip/next z))))]
      ;(println "(tree->combinations" tree remove-dups? "), z=" z ", leaves=" leaves)
      (when z
        (seq (map #(->> %
                   (zip/path)
                   ((fn [p]
                     (let [node (zip/node %)]
                       (if (= node ::dups)
                         p ; don't add ::dups to path!
                         (conj p (list node)))))) ; add leaf node to path
                   (map first) ; first is node, rest are children
                   (rest)) ; remove nil root node
                leaves))))))

(defn combinations
  "Returns a lazy sequence of all the unique ways of taking n different elements
  from a sequence (i.e. elements at n distinct positions in the sequence) while
  preserving their original order. The same combination may be returned multiple
  times if coll contains duplicates.
  Returns '(()) if n is 0.
  This function also takes a single combinations tree, thus deriving all
  combinations the tree can yield. The given tree can be the the result of
  combinations-tree or distinct-combinations-tree. In the first case duplicates
  may appear if the original collection contains duplicates. In the latter case
  only distinct combinations will be returned (no duplicates).
  Returns nil if no combinations exists, if tree or coll is nil or empty,
  or if n is 0.
  Throws the same exceptions as combinations-tree.
  Throws an exception when n is negative or greater than the size of coll.
  With 2 args this returns what clojure.math.combinatorics/combinations returns
  except when n=0 where it returns nil instead of '(())."
  ([tree]
    (tree->combinations tree false))
  ([coll n]
    (combinations (combinations-tree coll n))))

(defn distinct-combinations
  "Returns a lazy sequence of all the unique ways of taking n different elements
  from a sequence (i.e. elements at n distinct positions in the sequence) while
  preserving their original order. A combination is returned only once even if
  coll contains duplicates.
  Returns nil if no combinations exists, if tree or coll is nil or empty,
  or if n is 0.
  Throws the same exceptions as combinations-tree.
  Throws an exception when n is negative or greater than the size of coll."
  [coll n]
  (combinations (distinct-combinations-tree coll n)))

(defn unique-combinations
  "Computes all unique combinations of n different elements taken from a
  sequence (i.e. elements at n distinct positions in the sequence) while
  preserving their original order.
  When a coll and a single integer is provided, this function returns a sequence
  of all combinations of n elements from coll:
    (unique-combinations [:k :s :k :s] 2)
    => ((:s :k) (:s :s) (:k :k))
  When coll contains duplicates, the set of all possible combinations may also
  contain duplicates which are eliminated from the results of this function as
  they are not unique. In the above example the (:k :s) combination has been
  removed for that reason because it matches positions 1+2 and 3+4.
  When duplicates are not be be removed, use the combinations function instead.
  This function also takes a single tree as returned by distinct-combinations-tree
  thus deriving all unique combinations the tree can yield.
  Returns nil if no combinations exists, if tree or coll is nil or empty,
  or if n is 0.
  Throws the same exceptions as distinct-combinations-tree."
  ([distinct-combinations-tree]
    (tree->combinations distinct-combinations-tree true))
  ([coll n]
    (unique-combinations (distinct-combinations-tree coll n))))

(defn prefix-tree
  "Adds a sequence of items to the head of a given tree, returning a new tree
  that yields sequences starting with the sequence of items. For example:
  (combinations (prefix-tree [1 2] (combinations-tree [3 4 5 6] 2)))
  => ((1 2 3 4) (1 2 3 5) (1 2 3 6) (1 2 4 5) (1 2 4 6) (1 2 5 6))"
  [heads tree]
  (if (seq heads)
      (let [[h & t] heads]
         (if (seq t)
           (tree-list
             (tree-cons h
               (prefix-tree t tree)))
           (if (seq tree)
             (tree-list (tree-cons h tree))
             (tree-list h))))
      tree))