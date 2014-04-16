# twalk

This module provides a generic depth-first tree visitor function in Clojure.
It utilizes trampoline to handle trees of any size with constant satck space consumption.

## Usage

In project.clj:

    (defproject myproject
     ...
     :dependencies [[twalk "0.2.0-SNAPSHOT"]
                    ...])
					
In your code:

    (ns your.module
     (:require [twalk.twalk :refer [twalk]]))
    
    (let [ctx ...
          tree ...
          [ctx' tree'] (twalk ctx tree)]
      ...)

Twalk performs a depth-first tree taversal.
Given a set of functions, it can treat any data structure as a tree.
Features of twalk are:

 * Twalk can do pre-order and/or post-order processing.  Both of them can be performed in one call.
 * Constant stack space consumption.  It can handle trees of any size without fear of stack overflow.
 * Twalk can transform a tree as well as accumulating data on it.

Twalk receives two arguments: a context and a tree.

A context is a map which contains several functions along with any other data.
It has two roles:
first, it provides functions with which twalk operates on the tree.
Second, it can carry arbitrary data during traversal.

You have to define several functions in a context as follows:

 * `:branch?`, a predicate which, given a node, returns true if it can have children.
 * `:children`, a function which, given a node, returns a seq of its children.
 * `:make-node`, a fuction which, given a node and new children, returns new node.
 * `:pre`, a function which, given a context and a node, returns a vector of new context and new node.
 * `:post`, a function which, given a context and a node, returns a vector of new context and new node.

Twalk applies `:pre` function on each node in pre-order, and `:post` function in post-order.

If the operation will not change a tree at all, `:make-node` can simply return the given node instead of replicating it. i.e., `(fn [node _] node)` is enough in such cases.

## Examples

In the following examples, we use sample-tree defined as follows:

    (def sample-tree
	  (let [c {:name "c"}
	        d {:name "d"}
			b {:name "b", :elements [c d]}
			e {:name "e"}
			a {:name "a", :elements [b e]}]
	   a))

And we define the common part of context:

    (def ctx-base
	  {:branch? :elements,
	   :children :elements,
	   :make-node (fn [node children] (assoc node :elements children)),
	   
	   :pre (fn [ctx node] [ctx node]),
	   :post (fn [ctx node] [ctx node])})

### Example 1. Count nodes

    (let [ctx (assoc ctx-base
               :pre (fn [ctx node] [(update-in ctx [:count] inc), node]),
               :count 0)
          [ctx' tree'] (twalk ctx sample-tree)]
      (:count ctx'))
    ===> 5

### Example 2. Collect names of nodes in pre-order

    (let [ctx (assoc ctx-base
               :pre (fn [ctx node]
                      [(update-in ctx [:names] #(conj % (:name node)))
                       node]),
               :names [])
          [ctx' tree'] (twalk ctx sample-tree)]
      (:names ctx'))
    ===> ["a" "b" "c" "d" "e"]

### Example 3. Collect names of nodes in post-order

    (let [ctx (assoc ctx-base
               :post (fn [ctx node]
                      [(update-in ctx [:names] #(conj % (:name node)))
                       node]),
               :names [])
          [ctx' tree'] (twalk ctx sample-tree)]
      (:names ctx'))
    ===> ["c" "d" "b" "e" "a"]

### Example 4. Set nodes their number in pre-order

    (let [ctx (assoc ctx-base
               :pre (fn [ctx node]
                      [(update-in ctx [:count] inc),
					   (assoc node :number (:count ctx))]),
               :count 0)
          [ctx' tree'] (twalk ctx sample-tree)]
      tree')
    ===> {:name "a", :number 0,
	      :elements ({:name "b", :number 1,
		              :elements ({:name "c", :number 2}
					             {:name "d", :number 3})}
					 {:name "e", :number 4})}

### Example 5. Insert new node and set nodes their number in pre-order

Insert node f as a new child of node b.
Because the insersion is done in `:pre` function,
the inserted node is also to be visited.

    (let [ctx (assoc ctx-base
               :pre (fn [ctx node]
			         (let [node (if (= "b" (:name node))
					             (update-in node [:elements]
								  #(concat % [{:name "f"}]))
								 node)]
                      [(update-in ctx [:count] inc),
					   (assoc node :number (:count ctx))])),
               :count 0)
          [ctx' tree'] (twalk ctx sample-tree)]
      tree')
    ===> {:name "a", :number 0,
	      :elements ({:name "b", :number 1,
		              :elements ({:name "c", :number 2}
					             {:name "d", :number 3}
					             {:name "f", :number 4})}
					 {:name "e", :number 5})}

## License

Copyright © 2014 sophistone

Distributed under MIT License.  See MIT-LICENSE for the details.
