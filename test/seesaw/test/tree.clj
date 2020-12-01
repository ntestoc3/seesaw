;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.tree
  (:use seesaw.tree)
  (:use [seesaw.core :only [listen]])
  (:use clojure.test
        ))

(defn- tree-listener
  "Dummy TreeModelListener that calls handler with the received event."
  [handler]
  (reify javax.swing.event.TreeModelListener
    (treeNodesChanged [this e] (handler e))
    (treeNodesInserted [this e] (handler e))
    (treeNodesRemoved [this e] (handler e))
    (treeStructureChanged [this e] (handler e))))

(deftest simple-tree-model-test
  (let [branch? (fn [node] (= node "dir"))
        children (fn [node] (when (= node "dir") [1 2 3]))
        m (simple-tree-model branch? children "dir")]
    (testing "should create a read-only tree model from branch? and children functions"
      (instance? javax.swing.tree.TreeModel m))
    (testing "should return the root"
      (= "dir" (.getRoot m)))
    (testing "should return isLeaf"
      (is (.isLeaf m "file"))
      (is (not (.isLeaf m "dir"))))
    (testing "should return the child count"
      (is (= 0 (.getChildCount m "file")))
      (is (= 3 (.getChildCount m "dir"))))
    (testing "should return a child by index"
      (= [1 2 3] (map #(.getChild m "dir" %) (range 3))))
    (testing "should retrieve the index of a child"
      (= [0 1 2] (map #(.getIndexOfChild m "dir" %) [1 2 3])))
    (testing "should allow a listener to be added"
      (let [called (atom nil)]
        (.addTreeModelListener m (tree-listener #(reset! called %)))
        (node-changed m [(.getRoot m)])
        (is @called)))
    (testing "should allow a listener to be removed"
      (let [called-a (atom nil)
            called-b (atom nil)
            listener-a (tree-listener #(reset! called-a %))
            listener-b (tree-listener #(reset! called-b %))]
        (.addTreeModelListener m listener-a)
        (.addTreeModelListener m listener-b)
        (.removeTreeModelListener m listener-a)
        (node-changed m [(.getRoot m)])
        (is (not @called-a))
        (is @called-b)))))

(defn- make-test-model []
  (simple-tree-model #(.isDirectory %) #(.listFiles %) (java.io.File. ".")))

(deftest fire-event-test
  (testing "fires nodes-changed events"
    (let [m (make-test-model)
          e (atom nil)
          root (.getRoot m)]
      (listen m :tree-nodes-changed #(reset! e %))
      (nodes-changed m [root] (map #(.getChild m root %) (range 4)))
      (is (not (nil? @e)))))
  (testing "fires node-changed events"
    (let [m (make-test-model)
          e (atom nil)
          root (.getRoot m)]
      (listen m :tree-nodes-changed #(reset! e %))
      (node-changed m [root])
      (is (not (nil? @e)))))
  (testing "fires nodes-inserted events"
    (let [m (make-test-model)
          e (atom nil)
          root (.getRoot m)]
      (listen m :tree-nodes-inserted #(reset! e %))
      (nodes-inserted m [root] (map #(.getChild m root %) (range 4)))
      (is (not (nil? @e)))))
  (testing "fires node-inserted events"
    (let [m (make-test-model)
          e (atom nil)
          root (.getRoot m)]
      (listen m :tree-nodes-inserted #(reset! e %))
      (node-inserted m [root])
      (is (not (nil? @e)))))

  (testing "fires node-structure-changed events"
    (let [m (make-test-model)
          e (atom nil)
          root (.getRoot m)]
      (listen m :tree-structure-changed #(reset! e %))
      (node-structure-changed m [root])
      (is (not (nil? @e)))))

  (testing "fires nodes-removed events"
    (let [m (make-test-model)
          e (atom nil)
          root (.getRoot m)]
      (listen m :tree-nodes-removed #(reset! e %))
      (nodes-removed m [root] (range 1 4) (map #(.getChild m root %) (range 1 4)))
      (is (not (nil? @e)))))

  (testing "fires node-removed events"
    (let [m (make-test-model)
          e (atom nil)
          root (.getRoot m)]
      (listen m :tree-nodes-removed #(reset! e %))
      (node-removed m [root] 2 (.getChild m root 2))
      (is (not (nil? @e))))))

