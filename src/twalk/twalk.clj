(ns twalk.twalk
  (:require [clojure.set :as set]))

;; Twalk is a general-purpose tree visitor.
;; Its important propety is constant-stack-space consumption.
;; It can process on trees of any size without stack overflow.

(declare twalk-1)

(defn- has-push? [ctx]
  (contains? ctx ::push))

(defn- keyset [m]
  (set (keys m)))

(defn- keys-intersection "Returns keys which both of m1 and m2 have." [m1 m2]
  (set/intersection (keyset m1) (keyset m2)))

(defn- keys-difference "Returns keys which m1 has but m2 doesn't." [m1 m2]
  (set/difference (keyset m1) (keyset m2)))

(defn skip "Returns ctx with annotation to skip processing children."
  [ctx]
  (assoc-in ctx [::skip :skip-children] true))

(defn skip-post "Returns ctx with annotation to skip post function on the current node."
  [ctx]
  (assoc-in ctx [::skip :skip-post] true))

(defn skip* "Returns ctx with annotation to skip processing children and
applying post function to the current node."
  [ctx]
  (-> ctx skip skip-post))

(defn push "Returns ctx merged with change.
This change is automatically reverted before execution of :post function
on the current node."
 [ctx change]
  {:pre [(not (contains? ctx ::push))]
   :post [(contains? % ::push)]}
  (let [modified-keys (keys-intersection ctx change)
        temp-added-keys (keys-difference change ctx)
        saved (select-keys ctx modified-keys)
        push-data {:saved-entries saved,
                   :temp-added-keys temp-added-keys,
                   :pop-before-post true}]

    (-> ctx
        (merge change)
        (assoc ::push push-data))))

(defn push*  "Returns ctx merged with change.
This change is automatically reverted after execution of :post function
on the current node."
  [ctx change]
  (-> (push ctx change)
      (update-in [::push] #(assoc % :pop-before-post false))))

(defn- pop-ctx [ctx push-data]
  (let [temp-added-keys (:temp-added-keys push-data)
        saved  (:saved-entries push-data)]
    (apply dissoc (merge ctx saved) temp-added-keys)))

(defn- twalk-list [ctx nodes k]
  (if (empty? nodes)
    (k ctx ())

    #(twalk-1 ctx (first nodes)
              (fn [[ctx x]]
                (fn [] (twalk-list ctx (rest nodes)
                                   (fn [ctx xs]
                                     (fn [] (k ctx (cons x xs))))))))))

(defn- twalk-dig [ctx node k]
  (if-let [children (and ((:branch? ctx) node)
                         (seq ((:children ctx) node)))]

    (twalk-list ctx children
                (fn [ctx children]
                  #(k ctx ((:make-node ctx) node children))))

    (k ctx node)))

(defn- twalk-1 [ctx node k]
  (let [visit (fn [sym ctx node]
                ((or (sym ctx) vector) ctx node))

        [ctx node] (visit :pre ctx node)
        push-data (::push ctx)
        skip-data (::skip ctx)

        skip-post? (and skip-data (:skip-post skip-data))
        skip-children? (and skip-data (:skip-children skip-data))
        apply-post (if skip-post?
                     (fn [ctx node] [ctx node])
                     (fn [ctx node] (visit :post ctx node)))

        pop-before-post? (and push-data (:pop-before-post push-data))
        pop (fn [ctx] (pop-ctx ctx push-data))
        apply-post-2 (cond
                      (not push-data)
                      apply-post

                      pop-before-post?
                      (fn [ctx node] (apply-post (pop ctx) node))

                      :else
                      (fn [ctx node]
                        (let [[ctx node] (apply-post ctx node)]
                          [(pop ctx) node])))
        
        ctx (dissoc ctx ::push ::skip)]

    (if skip-children?
      (k (apply-post-2 ctx node))
      (twalk-dig ctx node
                 (fn [ctx node]
                   #(k (apply-post-2 ctx node)))))))

(defn twalk "Iterates over a tree.
You can use this function to manipulate nodes and/or 
to accumulate information.

You must let ctx have several functions as follows:
 * :branch?, a predicate to tell whether a node has children or not.
 * :children, a function which returns a seq of child nodes of a node.
 * :make-node, a constructor which creates new node from old one and
   possibly updated children.
 * :pre, a pre-order function to be called on each node.
   It receives ctx and node and returns [ctx' node'].
   A typical no-op function for it is (fn [ctx node] [ctx node]).
 * :post, a post-order function to be called on each node
   It receives ctx and node and returns [ctx' node'].
   A typical no-op function for it is (fn [ctx node] [ctx node]).

Ctx can carry any arbitrary data along with them.

This function returns a vector [new-ctx new-node].
If k is supplied, evaluates (k [new-ctx new-node])
and returns its result instead."
  ([ctx node]
     (trampoline twalk-1 ctx node identity))
  ([ctx node k]
     (trampoline twalk-1 ctx node k)))

(defn return-ctx "Returns ctx.
This function is intended to be used as the third argument of twalk."
  [[ctx node]] ctx)

(defn return-node "Returns node.
This function is intended to be used as the third argument of twalk."
  [[ctx node]] node)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Naïve implementation of a general purpose tree visitor.
;; This is the concept model of twalk.
;; 
;; We can get twalk by tranforming walk with steps as follows:
;;  1. Transform to continuation-passing style
;;  2. Replace tail calls with thunks
;;  3. Alleviate thunkifications in the previous step
;;     while keeping constant-stack-space characteristics:
;;     * Keep only one thunk in the twalk1→twalk-dig→twalk-list recursion loop
;;     * Keep thunks in continuations
;;     * Unthunkify applications of continuation from twalk* functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declare walk)

;; (defn- walk-list [ctx nodes]
;;   (if (empty? nodes)
;;     [ctx ()]
;;     (let [[ctx x] (walk ctx (first nodes))
;;           [ctx xs] (walk-list ctx (rest nodes))]
;;       [ctx (cons x xs)])))

;; (defn- walk-dig [ctx node]
;;   (if-let [children (and ((:branch? ctx) node)
;;                          (seq ((:children ctx) node)))]

;;     (let [[ctx children] (walk-list ctx children)]
;;       [ctx ((:make-node ctx) node children)])

;;     [ctx node]))

;; (defn walk "a naïve version of twalk."
;;   ([ctx node]
;;      (let [[ctx node] ((:pre ctx) ctx node)
;;            [ctx node] (walk-dig ctx node)
;;            [ctx node] ((:post ctx) ctx node)]
;;        [ctx node]))
;;   ([ctx node k]
;;      (k (walk ctx node))))
