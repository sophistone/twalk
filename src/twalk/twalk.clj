(ns twalk.twalk)

;; Twalk is a general-purpose tree visitor.
;; Its important propety is constant-stack-space consumption.
;; It can process on trees of any size without stack overflow.

(declare twalk-1)

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
  (let [[ctx node] ((:pre ctx) ctx node)]
    (twalk-dig ctx node
               (fn [ctx node]
                 #(k ((:post ctx) ctx node))))))

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

(declare walk)

(defn- walk-list [ctx nodes]
  (if (empty? nodes)
    [ctx ()]
    (let [[ctx x] (walk ctx (first nodes))
          [ctx xs] (walk-list ctx (rest nodes))]
      [ctx (cons x xs)])))

(defn- walk-dig [ctx node]
  (if-let [children (and ((:branch? ctx) node)
                         (seq ((:children ctx) node)))]

    (let [[ctx children] (walk-list ctx children)]
      [ctx ((:make-node ctx) node children)])

    [ctx node]))

(defn walk "a naïve version of twalk."
  ([ctx node]
     (let [[ctx node] ((:pre ctx) ctx node)
           [ctx node] (walk-dig ctx node)
           [ctx node] ((:post ctx) ctx node)]
       [ctx node]))
  ([ctx node k]
     (k (walk ctx node))))
