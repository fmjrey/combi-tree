(ns combi-tree.combi-tree-test
  (:require [combi-tree.combi-tree :refer [combinations
                                           combinations-tree
                                           distinct-combinations
                                           unique-combinations]]
            #+clj [clojure.math.combinatorics :as combi]
            [clojure.set :as sets]
            #+clj [clojure.test :refer :all]
            #+cljs cemerick.cljs.test)
  #+cljs (:use-macros
            [cemerick.cljs.test :only [is are deftest testing]])
)

#+cljs
(def Exception js/Error)

(deftest test-combinations
  (let [cfns [combinations distinct-combinations unique-combinations]]
    (are [c n] (nil? (doseq [cfn cfns]
                       (is (thrown? Exception (cfn c n)))))
      nil     nil
      ()      nil
      [1 2 3] nil
      nil     -1
      ()      -1
      [1 2 3] -1
      ))
  (are [c n ecs edcs eucs] (let [cs  (combinations c n) scs (set cs)
                                 dcs (distinct-combinations c n)
                                 ucs (unique-combinations c n) sucs (set ucs)]
                             (and #+clj (= cs ecs
                                           (let [cc (combi/combinations c n)]
                                             (if (seq (flatten cc)) cc nil)))
                                  (= dcs edcs)
                                  (= ucs eucs)
                                  (= dcs (when cs (distinct cs)))
                                  (let [dups (sets/difference scs sucs)]
                                    (= ucs (when ucs (remove dups dcs))))))
       ;coll   n combinations distinct-combinations unique-combinations
       nil     0 nil          nil                   nil
       ()      0 nil          nil                   nil
       [1]     0 nil          nil                   nil
       nil     1 nil          nil                   nil
       ()      1 nil          nil                   nil
       [1]     2 nil          nil                   nil
       [1 2]   3 nil          nil                   nil
       [1 2 3] 4 nil          nil                   nil
       ;coll                  n combinations 
       ;                        distinct-combinations
       ;                        unique-combinations
       [1 2 3]                1 '((1) (2) (3))
                                '((1) (2) (3))
                                '((1) (2) (3))
       [1 2 3]                2 '((1 2) (1 3) (2 3))
                                '((1 2) (1 3) (2 3))
                                '((1 2) (1 3) (2 3))
       [1 2 3]                3 '((1 2 3))
                                '((1 2 3))
                                '((1 2 3))
       [1 1 1]                1 '((1) (1) (1))
                                '((1))
                                nil
       [:k :s :k :s]          1 '((:k) (:s) (:k) (:s))
                                '((:k) (:s))
                                nil
       [:k :s :k :s]          2 '((:k :s) (:k :k) (:k :s) (:s :k) (:s :s) (:k :s))
                                '((:k :s) (:k :k) (:s :k) (:s :s))
                                '((:k :k) (:s :k) (:s :s))
       [:k :s :k :s]          3 '((:k :s :k) (:k :s :s) (:k :k :s) (:s :k :s))
                                '((:k :s :k) (:k :s :s) (:k :k :s) (:s :k :s))
                                '((:k :s :k) (:k :s :s) (:k :k :s) (:s :k :s))
       [:k :s :k :s]          4 '((:k :s :k :s))
                                '((:k :s :k :s))
                                '((:k :s :k :s))
       [:k :s :k :s :k :s]    0 nil
                                nil
                                nil
       [:k :s :k :s :k :s]    1 '((:k) (:s) (:k) (:s) (:k) (:s))
                                '((:k) (:s))
                                nil
       [:k :s :k :s :k :s]    2 '((:k :s) (:k :k) (:k :s) (:k :k) (:k :s) (:s :k) (:s :s) (:s :k) (:s :s) (:k :s) (:k :k) (:k :s) (:s :k) (:s :s) (:k :s))
                                '((:k :s) (:k :k) (:s :k) (:s :s))
                                nil
       [:k :s :k :s :k :s]    3 '((:k :s :k) (:k :s :s) (:k :s :k) (:k :s :s) (:k :k :s) (:k :k :k) (:k :k :s) (:k :s :k) (:k :s :s) (:k :k :s) (:s :k :s) (:s :k :k) (:s :k :s) (:s :s :k) (:s :s :s) (:s :k :s) (:k :s :k) (:k :s :s) (:k :k :s) (:s :k :s))
                                '((:k :s :k) (:k :s :s) (:k :k :s) (:k :k :k) (:s :k :s) (:s :k :k) (:s :s :k) (:s :s :s))
                                '((:k :k :k) (:s :k :k) (:s :s :k) (:s :s :s))
       [:k :s :k :s :k :s]    4 '((:k :s :k :s) (:k :s :k :k) (:k :s :k :s) (:k :s :s :k) (:k :s :s :s) (:k :s :k :s) (:k :k :s :k) (:k :k :s :s) (:k :k :k :s) (:k :s :k :s) (:s :k :s :k) (:s :k :s :s) (:s :k :k :s) (:s :s :k :s) (:k :s :k :s))
                                '((:k :s :k :s) (:k :s :k :k) (:k :s :s :k) (:k :s :s :s) (:k :k :s :k) (:k :k :s :s) (:k :k :k :s) (:s :k :s :k) (:s :k :s :s) (:s :k :k :s) (:s :s :k :s))
                                '((:k :s :k :k) (:k :s :s :k) (:k :s :s :s) (:k :k :s :k) (:k :k :s :s) (:k :k :k :s) (:s :k :s :k) (:s :k :s :s) (:s :k :k :s) (:s :s :k :s))
       [:k :s :k :s :k :s]    5 '((:k :s :k :s :k) (:k :s :k :s :s) (:k :s :k :k :s) (:k :s :s :k :s) (:k :k :s :k :s) (:s :k :s :k :s))
                                '((:k :s :k :s :k) (:k :s :k :s :s) (:k :s :k :k :s) (:k :s :s :k :s) (:k :k :s :k :s) (:s :k :s :k :s))
                                '((:k :s :k :s :k) (:k :s :k :s :s) (:k :s :k :k :s) (:k :s :s :k :s) (:k :k :s :k :s) (:s :k :s :k :s))
       [:k :s :k :s :k :s]    6 '((:k :s :k :s :k :s))
                                '((:k :s :k :s :k :s))
                                '((:k :s :k :s :k :s))
       '[(:k) (:s) (:k) (:s)] 2 '(((:k) (:s)) ((:k) (:k)) ((:k) (:s)) ((:s) (:k)) ((:s) (:s)) ((:k) (:s)))
                                '(((:k) (:s)) ((:k) (:k)) ((:s) (:k)) ((:s) (:s)))
                                '(((:k) (:k)) ((:s) (:k)) ((:s) (:s)))
  ))

(defn lazy? [coll]
  (or (instance? clojure.lang.LazySeq coll)
      (and (instance? clojure.lang.Cons coll)
           (lazy? (rest coll)))))

(deftest test-laziness
  (is (lazy? (combinations (range 12) 3)) "combinations fn should be lazy")
  (is (lazy? (combinations-tree (range 12) 3)) "combinations-tree fn should be lazy"))
