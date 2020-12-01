;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.event
  (:use seesaw.event
        [seesaw.util :only [root-cause]])
  (:require [seesaw.core :as sc])
  (:use clojure.test
        )
  (:import [javax.swing JPanel JTextField JButton JRadioButton JToggleButton]
           [javax.swing.event ChangeListener]
           [java.awt.event ComponentListener ItemListener MouseListener MouseMotionListener]))

(defn test-handler [_])

(defn verify-empty-listener
  [listener-type handler-key dispatch-fn]
  (let [listener (reify-listener listener-type (ref {}))]
    (dispatch-fn listener)
    true))

(defn verify-listener
  [listener-type handler-key dispatch-fn]
  (let [called (atom false)
        handler (fn [e] (reset! called true))
        listener (reify-listener listener-type (ref { handler-key [handler] }))]
    (dispatch-fn listener)
    (or @called
        (throw (RuntimeException. (str listener-type ", " handler-key " wasn't called!"))))))

(defn verify-listeners
  [listener-type & args]
  (every? true? (map (fn [[hk df]] (verify-listener listener-type hk df)) (partition 2 args))))

(deftest append-listener-test
  (testing "inserts necessary keys and creates an initial list"
    (let [listener  #(println %)]
      (is (= {:test-key [listener]} (append-listener {} :test-key listener)))))
  (testing "can insert additional listeners"
    (let [listener  #(println %)]
      (is (= {:test-key [:dummy listener]}
         (append-listener {:test-key [:dummy]} :test-key listener))))))

(deftest unappend-listener-test
  (testing "can remove a listener"
    (let [initial {:list-key [:a :b :c]}
          result  (unappend-listener initial :list-key :b)]
      (is (= {:list-key [:a :c]} result)))))


(deftest reify-listener-test
  (testing "for ComponentListener"
    (testing "instantiates a ComponentListener instance"
      (instance? ComponentListener (reify-listener ComponentListener (ref {}))))
    (testing "makes an instance that does nothing when there's no handler"
      (verify-empty-listener ComponentListener :component-resized #(.componentResized % nil)))
    (testing "makes an instance that calls expected methods"
      (verify-listeners ComponentListener
                        :component-hidden #(.componentHidden % nil)
                        :component-moved #(.componentMoved % nil)
                        :component-resized #(.componentResized % nil)
                        :component-shown #(.componentShown % nil))))
  (testing "for ChangeListener"
    (testing "instantiates a ChangeListener instance"
      (instance? ChangeListener (reify-listener ChangeListener (ref {}))))
    (testing "makes an instance that does nothing when there's no handler"
      (verify-empty-listener ChangeListener :state-changed #(.stateChanged % nil)))
    (testing "makes an instance that calls :state-changed"
      (verify-listener ChangeListener :state-changed #(.stateChanged % nil))))

  (testing "for ItemListener"
    (testing "instantiates an ItemListener instance"
      (instance? ItemListener (reify-listener ItemListener (ref {}))))
    (testing "makes an instance that does nothing when there's no handler"
      (verify-empty-listener ItemListener :item-state-changed #(.itemStateChanged % nil)))
    (testing "makes an instance that calls :item-state-changed"
      (verify-listener ItemListener :item-state-changed #(.itemStateChanged % nil))))

  (testing "for MouseListener"
    (testing "instantiates an MouseListener instance"
      (instance? MouseListener (reify-listener MouseListener (ref {}))))
    (testing "makes an instance that does nothing when there's no handlers"
      (verify-empty-listener MouseListener :mouse-clicked #(.mouseClicked % nil)))
    (testing "makes an instance that calls expected methods"
      (verify-listeners MouseListener
        :mouse-clicked #(.mouseClicked % nil)
        :mouse-entered #(.mouseEntered % nil)
        :mouse-exited #(.mouseExited % nil)
        :mouse-pressed #(.mousePressed % nil)
        :mouse-released #(.mouseReleased % nil))))

  (testing "for MouseMotionListener"
    (testing "instantiates an MouseMotionListener instance"
      (instance? MouseMotionListener (reify-listener MouseMotionListener (ref {}))))
    (testing "makes an instance that does nothing when there's no handlers"
      (verify-empty-listener MouseMotionListener :mouse-moved #(.mouseMoved % nil)))
    (testing "makes an instance that calls expected methods"
      (verify-listeners MouseMotionListener
        :mouse-moved #(.mouseMoved % nil)
        :mouse-dragged #(.mouseDragged % nil)))))


(deftest listen-test
  (testing "throws IllegalArgumentException if its event/handler pair list isn't even length"
    (try
      (listen :something-something (fn [_]))
      false
      (catch IllegalArgumentException e
        true)
      (catch RuntimeException e
        (instance? IllegalArgumentException (root-cause e)))))
  (testing "throws IllegalArgumentException if its first arguments isn't an event source"
    (try
      (listen :something-something (fn [_]) :another)
      false
      (catch IllegalArgumentException e
        true)
      ; TODO 1.2 event wrapping
      (catch RuntimeException e
        (instance? IllegalArgumentException (root-cause e)))))
  (testing "throws IllegalArgumentException if a handler isn't a function or var"
    (try
      (listen (javax.swing.JPanel.) :mouse "foo")
      false
      (catch IllegalArgumentException e
        true)
      ; TODO 1.2 event wrapping
      (catch RuntimeException e
        (instance? IllegalArgumentException (root-cause e)))))
  (testing "throws IllegalArgumentException for unknown event types"
    (try
      (listen (JPanel.) :something-something (fn [_]))
      false
      (catch IllegalArgumentException e
        true)
      ; TODO 1.2 event wrapping
      (catch RuntimeException e
        (instance? IllegalArgumentException (root-cause e)))))
  (testing "can install a listener with a var as handler"
    (let [panel        (JPanel.)
          f        (fn [e] (println "handled"))]
      (do
        (listen panel :mouse-clicked (var test-handler))
        (is (= 1 (-> panel .getMouseListeners count))))))
  (testing "can install a mouse-clicked listener"
    (let [panel        (JPanel.)
          f        (fn [e] (println "handled"))]
      (do
        (listen panel :mouse-clicked f)
        (is (= 1 (-> panel .getMouseListeners count)))
        (is (= f (-> (get-handlers panel :mouse) :mouse-clicked first))))))

  (testing "returns a function that will remove the installed listener"
    (let [panel (JPanel.)
          f     (fn [e] (println "handled"))
          remover (listen panel :mouse-clicked f)]
      (do
        (remover)
        (is (= 1 (-> panel .getMouseListeners count)))
        (is (= 0 (-> (get-handlers panel :mouse) :mouse-clicked count))))))

  (testing "can install two mouse-clicked listeners"
    (let [panel        (JPanel.)
          f        (fn [e] (println "handled"))
          g        (fn [e] (println "again!"))]
      (do
        (listen panel :mouse-clicked f :mouse-clicked g)
        (is (= 1 (-> panel .getMouseListeners count)))
        (is (= [g f] (-> panel (get-handlers :mouse) :mouse-clicked))))))

  (testing "can install a document listener"
    (let [field        (JTextField.)
          called   (atom false)
          f        (fn [e] (reset! called true))]
      (do
        (listen field :insert-update f)
        (.setText field "force a change")
        (is @called))))

  (testing "can register for a class of events"
      (let [field    (JTextField.)
            called   (atom 0)
            f        (fn [e] (swap! called inc))]
        (do
          (listen field :document f)
          (.setText field "force insert")
          (.setText field ""))
        (is (= 2 @called))))

  (testing "can register for multiple events"
    (let [field    (JTextField.)
          called   (atom 0)
          f        (fn [e] (swap! called inc))]
      (do
        (listen field #{:remove-update :insert-update} f)
        (.setText field "force insert")
        (.setText field ""))
      (is (= 2 @called))))
  (testing "can register handlers on multiple targets"
    (let [button0  (JButton.)
          button1  (JButton.)
          called   (atom 0)
          f        (fn [e] (swap! called inc))]
      (do
        (listen [button0 button1] :action f)
        (.doClick button0)
        (.doClick button1))
      (is (= 2 @called))))

  (testing "can register for window events on a frame"
    (let [f (javax.swing.JFrame.)]
      (listen f :window-closed (fn [e] nil))))

  (testing "registers events on all buttons in a ButtonGroup"
    (let [[a b c] [(JRadioButton.) (JToggleButton.) (JButton.)]
          bg (sc/button-group :buttons [a b c])]
      (is (= 0 (count (.getActionListeners a))))
      (is (= 0 (count (.getActionListeners b))))
      (is (= 0 (count (.getActionListeners c))))
      (listen bg :action (fn [e]))
      (is (= 1 (count (.getActionListeners a))))
      (is (= 1 (count (.getActionListeners b))))
      (is (= 1 (count (.getActionListeners c))))))

  (testing "can register a ListSelectionListener on a JList with :selection key"
    (let [jlist (javax.swing.JList.)
          called (atom false)]
      (do
        (is (= 0 (count (.getListSelectionListeners jlist))))
        (listen jlist :selection (fn [e] (reset! called true)))
        (is (= 1 (count (.getListSelectionListeners jlist))))
        (.. (first (.getListSelectionListeners jlist)) (valueChanged nil))
        (is @called))))

  (testing "can register a CaretListener on a JTextComponent with :selection key"
    (let [jtext (javax.swing.JTextField.)
          called (atom false)]
      (do
        (is (= 0 (count (.getCaretListeners jtext))))
        (listen jtext :selection (fn [e] (reset! called true)))
        (is (= 1 (count (.getCaretListeners jtext))))
        (.. (first (.getCaretListeners jtext)) (caretUpdate nil))
        (is @called))))

  (testing "can register a ListSelectionListener on a JTable with :selection key"
      (let [jtable (javax.swing.JTable. 5 1)
            called (atom false)]
        (do
          ; a mystery listener is added by JTable
          (is (= 1 (count (.. jtable getSelectionModel getListSelectionListeners))))
          (listen jtable :selection (fn [e] (reset! called true)))
          (is (= 2 (count (.. jtable getSelectionModel getListSelectionListeners))))
          (.. (first (.. jtable getSelectionModel getListSelectionListeners)) (valueChanged nil))
          (is @called))))

  (testing "can register a TreeSelectionListener on a JTree with :selection key"
    (let [tree (javax.swing.JTree.)
          called (atom false)]
      (do
        (is (= 0 (count (.getTreeSelectionListeners tree))))
        (listen tree :selection (fn [e] (reset! called true)))
        (is (= 1 (count (.getTreeSelectionListeners tree))))
        (.. (first (.getTreeSelectionListeners tree)) (valueChanged nil))
        (is @called))))
  (testing "can register an ActionListener on a JComboBox with :selection key"
    (let [cb (javax.swing.JComboBox.)
          called (atom false)]
      (do
        (is (= 0 (count (.getActionListeners cb))))
        (listen cb :selection (fn [e] (reset! called true)))
        (is (= 1 (count (.getActionListeners cb))))
        (.. (first (.getActionListeners cb)) (actionPerformed nil))
        (is @called))))

  (testing "can listen for tab panel changes with :selection key"
    (let [tp (sc/tabbed-panel :tabs [{:title "A" :content "A"}
                                     {:title "B" :content "B"}])
          called (atom nil)]
      (listen tp :selection (fn [e] (reset! called true)))
      (.setSelectedIndex tp 1)
      (is @called)))

  (testing "can register an ItemListener on an ItemSelectable (like a checkbox) with :selection key"
    (let [b (javax.swing.JToggleButton.)
          called (atom false)]
      (do
        (is (= 0 (count (.getItemListeners b))))
        (listen b :selection (fn [e] (reset! called true)))
        (is (= 1 (count (.getItemListeners b))))
        (.. (first (.getItemListeners b)) (itemStateChanged nil))
        (is @called))))
  (testing "can register a caret listener on a text component"
    (let [tc (javax.swing.JTextField. "some text")
          updated (atom nil)]
      (listen tc :caret-update #(reset! updated %))
      (.setCaretPosition tc 5)
      (is @updated)))
  (testing "can register a tree expansion listener"
    (let [tree (javax.swing.JTree.)
          expanded (atom false)
          collapsed (atom false)]
      (listen tree :tree-expanded #(reset! expanded %)
                   :tree-collapsed #(reset! collapsed %))
      (is (not (or @expanded @collapsed)))
      (.collapseRow tree 0)
      (is @collapsed)
      (.expandRow tree 0)
      (is @expanded)))
  (testing "can register a tree-will-expand listener"
    (let [tree (javax.swing.JTree.)
          will-expand(atom false)
          will-collapse (atom false)]
      (listen tree :tree-will-expand #(reset! will-expand %)
                   :tree-will-collapse #(reset! will-collapse %))
      (is (not (or @will-expand @will-collapse)))))
  (testing "can register a tree model listener"
      (let [root (javax.swing.tree.DefaultMutableTreeNode.)
            child (javax.swing.tree.DefaultMutableTreeNode.)
            model (javax.swing.tree.DefaultTreeModel. root)
            nodes-changed (atom nil)
            nodes-inserted (atom nil)
            nodes-removed (atom nil)
            structure-changed (atom nil)]
      (listen model :tree-nodes-changed #(reset! nodes-changed %)
                    :tree-nodes-inserted #(reset! nodes-inserted %)
                    :tree-nodes-removed #(reset! nodes-removed %)
                    :tree-structure-changed #(reset! structure-changed %))
      (is (not (or @nodes-changed @nodes-inserted @nodes-removed @structure-changed)))
      (.nodeChanged model root)
      (is @nodes-changed)
      (.insertNodeInto model child root 0)
      (is @nodes-inserted)
      (.removeNodeFromParent model child)
      (is @nodes-removed)
      (.nodeStructureChanged model root)
      (is @structure-changed)))
  (testing "can register a hyperlink listener"
    (let [editor (javax.swing.JEditorPane.)
          called (atom 0)]
      (listen editor :hyperlink-update (fn [_] (swap! called inc)))
      (listen editor :hyperlink (fn [_] (swap! called inc)))
      (is (= 0 @called))
      (.fireHyperlinkUpdate editor nil)
      (is (= 2 @called))))

  (testing "can register an ActionListener on a java.awt.MenuItem with :action key"
    (let [mi (java.awt.MenuItem.)
          called (atom false)]
      (do
        (is (= 0 (count (.getActionListeners mi))))
        (listen mi :action (fn [e] (reset! called true)))
        (is (= 1 (count (.getActionListeners mi))))
        (.. (first (.getActionListeners mi)) (actionPerformed nil))
        (is @called)))))

(deftest listen-to-property-test
  (testing "registers a property change listener"
    (let [b (javax.swing.JButton.)
          called (atom nil)
          remove-fn (listen-to-property b "text"
                                        (fn [e] (reset! called e)))]
      (.setText b "HI")
      (is @called)
      (reset! called nil)
      (remove-fn)
      (.setText b "BYE")
      (is (nil? @called)))))

