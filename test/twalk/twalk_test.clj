(ns twalk.twalk-test
  (:require [clojure.test :refer :all]
            [twalk.twalk :refer :all]))

(def ^:const sample-tree
  ;; a -+- b -+- c
  ;;    |     +- d
  ;;    +- e
  ;;
  ;; Each node has two fields:
  ;;  - :name for its name, and
  ;;  - :elements for a list of its children

  (let [c {:name "c"},
        d {:name "d"},
        e {:name "e"},
        b {:name "b", :elements [c d]},
        a {:name "a", :elements [b e]}]
    a))

(def ^:const ctx-base
  {:branch? (fn [node] (contains? node :elements)),
   :children :elements,
   :make-node (fn [old-node new-children]
                (assoc old-node :elements new-children)),
   :pre (fn [ctx node] [ctx node]),     ; no-op
   :post (fn [ctx node] [ctx node])})   ; no-op

(def ^:const nochange-ctx-base
  ;; This context is intended to be used for non-mutation operation.
  ;; It avoids unnecessary tree replication.
  (assoc ctx-base :make-node (fn [old-node new-children] old-node)))

(deftest twalk-preorder-nochange-test
  (testing "Count the number of nodes."
    (let [ctx (assoc nochange-ctx-base
                :count 0,
                :pre (fn [ctx node]
                       [(update-in ctx [:count] inc)
                        node]))
          [ctx' tree'] (twalk ctx sample-tree)]

      (is (= 5 (:count ctx')))
      (is (= tree' sample-tree))
      ;; Make sure that the tree won't be altered
      ;; if :make-node function returns the old node itself.
      (is (identical? tree' sample-tree))))

  (testing "List the names of the nodes in preorder."
    (let [ctx (assoc nochange-ctx-base
                :names [],
                :pre (fn [ctx node]
                       [(update-in ctx [:names] #(conj % (:name node)))
                        node]))
          [ctx' tree'] (twalk ctx sample-tree)]

      (is (= ["a","b","c","d","e"] (:names ctx')))
      (is (= tree' sample-tree))
      (is (identical? tree' sample-tree)))))

(deftest twalk-postorder-nochange-test
  (testing "List the names of the nodes in postorder."
    (let [ctx (assoc nochange-ctx-base
                :names [],
                :post (fn [ctx node]
                        [(update-in ctx [:names] #(conj % (:name node)))
                         node]))
          [ctx' tree'] (twalk ctx sample-tree)]

      (is (= ["c","d","b","e","a"] (:names ctx')))
      (is (= tree' sample-tree))
      (is (identical? tree' sample-tree)))))

(deftest twalk-preorder-mutation-test
  (testing "Number each node in preorder"
    ;; Visit each node in preorder and
    ;; set its visited order in :number field.
    (let [ctx (assoc ctx-base
                :count 0,
                :pre (fn [ctx node]
                       (let [node' (assoc node :number (:count ctx))
                             ctx' (update-in ctx [:count] inc)]
                         [ctx' node'])))
          expected-tree (let [c {:name "c", :number 2},
                              d {:name "d", :number 3},
                              b {:name "b", :number 1, :elements [c d]},
                              e {:name "e", :number 4},
                              a {:name "a", :number 0, :elements [b e]}]
                          a)
          [ctx' tree'] (twalk ctx sample-tree)]

      (is (= 5 (:count ctx')))
      (is (= expected-tree tree')))))

(deftest twalk-postorder-mutation-test
  (testing "Number each node in postorder"
    ;; Visit each node in postorder and
    ;; set its visited order in :number field.
    (let [ctx (assoc ctx-base
                :count 0,
                :post (fn [ctx node]
                        (let [node' (assoc node :number (:count ctx))
                              ctx' (update-in ctx [:count] inc)]
                          [ctx' node'])))
          expected-tree (let [c {:name "c", :number 0},
                              d {:name "d", :number 1},
                              b {:name "b", :number 2, :elements [c d]},
                              e {:name "e", :number 3},
                              a {:name "a", :number 4, :elements [b e]}]
                          a)
          [ctx' tree'] (twalk ctx sample-tree)]

      (is (= 5 (:count ctx')))
      (is (= expected-tree tree')))))

(deftest twalk-no-stack-overflow-test
  (testing "Traversal over a very deep tree can be done without stack overflow"
    (let [ctx {:branch? next,
               :children #(list (rest %)),
               :make-node #(cons (first %1) (first %2)),

               :pre (fn [ctx node]
                      [(update-in ctx [:sum] #(+ % (first node))),
                       node]),
               :post list,
               :sum 0}]

      (is (= 4999950000
             (twalk ctx (take 100000 (iterate inc 0)) #(-> % first :sum)))))))

(defn- upcase [^String s] (.toUpperCase s))
(defn- update-node-name [f node] (update-in node [:name] f))

(deftest push-test
  (testing "Process children and descendants with ctx changed"
    ;; On descendant of node b, replace their names with upcased one.
    ;; An entry :under-b is added on them, too.
    (let [ctx (assoc ctx-base
                :transform-name identity,
                :pre (fn [ctx node]
                       (let [ctx
                             (if (= "b" (:name node))
                               (push ctx {:transform-name upcase,
                                          :under-b true})
                               ctx)]
                         [ctx node])),
                :post (fn [ctx node]
                        [ctx
                         (-> (update-node-name (:transform-name ctx) node)
                             (merge (select-keys ctx [:under-b]))
                             (assoc :processed true))]))

          expected-tree   (let [c {:name "C", :processed true, :under-b true},
                                d {:name "D", :processed true, :under-b true},
                                e {:name "e", :processed true},
                                b {:name "b", :elements [c d], :processed true},
                                a {:name "a", :elements [b e], :processed true}]
                            a)

          [ctx' tree'] (twalk ctx sample-tree)]

      (is (= expected-tree tree')))))

(deftest push*-test
  (testing "Process a subtree with ctx changed"
    (let [ctx (assoc ctx-base
                :transform-name identity,
                :pre (fn [ctx node]
                       (let [ctx
                             (if (= "b" (:name node))
                               (push* ctx {:transform-name upcase})
                               ctx)]
                         [ctx node])),
                :post (fn [ctx node]
                        [ctx (update-node-name (:transform-name ctx) node)]))

          expected-tree   (let [c {:name "C"},
                                d {:name "D"},
                                e {:name "e"},
                                b {:name "B", :elements [c d]},
                                a {:name "a", :elements [b e]}]
                            a)

          [ctx' tree'] (twalk ctx sample-tree)]

      (is (= expected-tree tree')))))

(deftest skip-test
  (testing "Skip nodes under a certain node"
    (let [ctx (assoc ctx-base
                :log []
                :pre (fn [ctx node]
                       (if (= "b" (:name node))
                         [(skip ctx) node]
                         [ctx node]))
                :post (fn [ctx node]
                        [(update-in ctx [:log] #(conj % (:name node)))
                         node]))

          [ctx' tree'] (twalk ctx sample-tree)]

      (is (= ["b", "e", "a"] (:log ctx')))))

  (testing "Skip nodes under a certain node and post function on it"
    (let [ctx (assoc ctx-base
                :log []
                :pre (fn [ctx node]
                       (if (= "b" (:name node))
                         [(skip* ctx) node]
                         [ctx node]))
                :post (fn [ctx node]
                        [(update-in ctx [:log] #(conj % (:name node)))
                         node]))

          [ctx' tree'] (twalk ctx sample-tree)]

      (is (= ["e", "a"] (:log ctx')))))

  (testing "Skip post function on a certain node"
    (let [ctx (assoc ctx-base
                :log []
                :pre (fn [ctx node]
                       (if (get #{"a" "b"} (:name node))
                         [(skip-post ctx) node]
                         [ctx node]))
                :post (fn [ctx node]
                        [(update-in ctx [:log] #(conj % (:name node)))
                         node]))

          [ctx' tree'] (twalk ctx sample-tree)]

      (is (= ["c" "d" "e"] (:log ctx'))))))

(deftest nil-for-pre-or-post-test
  (testing ":pre is nil"
    (let [ctx (assoc ctx-base
                :n 0
                :pre nil
                :post (fn [ctx node]
                        [(update-in ctx [:n] inc) node]))
          [ctx' tree'] (twalk ctx sample-tree)]
      (is (= 5 (:n ctx')))))

  (testing ":post is nil"
    (let [ctx (assoc ctx-base
                :n 0
                :pre (fn [ctx node]
                       [(update-in ctx [:n] inc) node])
                :post nil)
          [ctx' tree'] (twalk ctx sample-tree)]
      (is (= 5 (:n ctx'))))))
