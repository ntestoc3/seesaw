;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.selection
  (:require [seesaw.core :as sc])
  (:use clojure.test
        seesaw.selection
        seesaw.action))

(deftest selection-test
  (testing "when given an Action"
      (testing "returns nil when the action is not selected"
        (nil? (selection (action) )))
      (testing "returns a single-element seq with true if the action is selected and multi? is given"
        (= [true] (selection (action :selected? true) {:multi? true})))
      (testing "returns a single-element seq with true if the action is selected"
        (= true (selection (action :selected? true)))))
  (testing "when given an AbstractButton (e.g. toggle or checkbox)"
    (testing "returns false when the button is not selected"
      (false? (selection (javax.swing.JCheckBox. "something" false))))
    (testing "returns true if it is selected"
      (let [b (javax.swing.JCheckBox. "something" true)]
        (is (true? (selection b)))))
    (testing "returns a single-element seq with true if it's selected and multi? is true"
      (let [b (javax.swing.JCheckBox. "something" true)]
        (is (= [true] (selection b {:multi? true}))))))

  (testing "when given a ButtonGroup"
    (testing "returns nil when no button is selected"
      (nil? (selection (sc/button-group :buttons [(sc/toggle) (sc/radio)]))))
    (testing "returns the first selected button in the group"
      (let [b (sc/toggle :selected? true)]
        (is (= b (selection (sc/button-group :buttons [(sc/toggle) b (sc/radio)])))))))

  (testing "when given a ComboBox"
    (testing "returns nil when nothing is selected"
      (nil? (selection (javax.swing.JComboBox.))))
    (testing "returns a single-element seq with the selected value when multi? is true"
      (= [1] (selection (javax.swing.JComboBox. (to-array [1 2 3 4])) {:multi? true}))))

  (testing "when given a JTree"
    (testing "returns nil when the selection is empty"
      (nil? (selection (javax.swing.JTree.))))
    (testing "returns the selection as a seq of paths when it isn't empty"
      (let [jtree (javax.swing.JTree. (to-array [1 2 3 4 5]))]
        (.setSelectionInterval jtree 1 3)
        ; Note. This kind of sucks because the JTree constructor used above
        ; creates a tree of JTree.DynamicUtilTreeNode rather than just ints.
        ; If a real TreeModel was used, it could be more reasonable.
        (is (= [["root" 2] ["root" 3] ["root" 4]]
                  (map (fn [path] (map #(.getUserObject %) path)) (selection jtree {:multi? true})))))))

  (testing "when given a JList"
    (testing "returns nil when the selection is empty"
      (nil? (selection (javax.swing.JList.))))
    (testing "returns the selection when it isn't empty"
      (let [jlist (javax.swing.JList. (to-array [1 2 3 4 5 6 7]))]
        (.setSelectionInterval jlist 1 3)
        (is (= 2 (selection jlist)))
        (is (= [2 3 4] (selection jlist {:multi? true}))))))

  (testing "when given a JSlider"
    (testing "returns the current value"
      (= 32 (selection (sc/slider :min 0 :max 100 :value 32)))))

  (testing "when given a JSpinner"
    (testing "returns the current value"
      (= 32 (selection (sc/spinner :model (sc/spinner-model 32 :from 30 :to 35))))))

  (testing "when given a JTextComponent"
    (testing "returns nil when the selection is empty"
      (nil? (selection (javax.swing.JTextField. "HELLO"))))
    (testing "returns a range vector [start end] when the selection is non-empty"
      (let [t (javax.swing.JTextField. "HELLO")]
        (.select t 2 4)
        (is (= [2 4] (selection t))))))

  (testing "when given a JTabbedPane"
    (testing "returns nil when there are no tabs"
      (nil? (selection (javax.swing.JTabbedPane.))))
    (testing "returns {:index i :title \"the title\" :content widget} for the selected tab"
      (let [a (sc/label :text "A")
            b (sc/label :text "B")
            c-title (sc/label :text "C title")
            c-content (sc/label :text "C content")
            tp (sc/tabbed-panel :tabs [{:title "A" :content a}
                                      {:title "B" :content b}
                                       {:title c-title :content c-content} ])]
        (.setSelectedIndex tp 1)
        (is (= {:title "B" :content b :index 1} (selection tp)))
        (.setSelectedIndex tp 0)
        (is (= {:title "A" :content a :index 0} (selection tp)))
        (.setSelectedIndex tp 2)
        (is (= {:title c-title :content c-content :index 2} (selection tp))))))

  (testing "when given a JTable"
    (testing "returns nil when no rows are selected"
      (nil? (selection (javax.swing.JTable.))))
    (testing "returns a seq of selected model row indices when selection is non-empty"
      (let [jtable (javax.swing.JTable. 5 3)]
        (.setRowSelectionInterval jtable 1 3)
        (is (= [1 2 3] (selection jtable {:multi? true})))
        (is (= 1 (selection jtable)))))))


(deftest selection!-test
  (testing "when given an AbstractButton (e.g. toggle or checkbox) and an argument"
    (testing "deselects the button if the argument is nil"
      (let [cb (javax.swing.JCheckBox. "something" true)]
        (do
          (is (= cb (selection! cb nil)))
          (is (false? (selection cb))))))
    (testing "selects the button if the argument is truthy"
      (let [cb (javax.swing.JCheckBox. "something" false)]
        (do
          (is (= cb (selection! cb "true")))
          (is (selection cb))))))

  (testing "when given a ButtonGroup and an argument"
    (testing "deselects the button if the argument is nil"
      (let [bg (sc/button-group :buttons [(sc/toggle) (sc/radio :selected? true) (sc/radio)])]
        (do
          (is (= bg (selection! bg nil)))
          (is (nil? (selection bg))))))
    (testing "selects a button if the argument is a button"
      (let [b (sc/radio)
            bg (sc/button-group :buttons [(sc/toggle :selected? true) b (sc/radio)])]
        (do
          (is (= bg (selection! bg b)))
          (is (= b (selection bg)))
          (is (.isSelected b))))))

  (testing "when given a ComboBox and an argument"
    (testing "sets the selection to that argument"
      (let [cb (javax.swing.JComboBox. (to-array [1 2 3 4]))]
        (do
          (is (= cb (selection! cb 3)))
          (is (= 3 (selection cb)))))))

  (testing "when given a JSlider and an argument"
    (testing "sets the slider value to that argument"
      (let [s (sc/slider :min 0 :max 100 :value 0)
            result (selection! s 32)]
        (is (= result s))
        (is (= 32 (.getValue s))))))

  (testing "when given a JSpinner and an argument"
    (testing "sets the spinner value to that argument"
      (let [s (sc/spinner :model (sc/spinner-model 30 :from 30 :to 35))
            result (selection! s 32)]
        (is (= result s))
        (is (= 32 (.getValue s))))))

  (testing "when given a JTree and an argument"
    (testing "Clears the selection when the argument is nil"
      (let [jtree (javax.swing.JTree. (to-array [1 2 3 4 5]))]
        (.setSelectionInterval jtree 1 3)
        (is (= jtree (selection! jtree nil)))
        (is (nil? (selection jtree))))))

  (testing "when given a JList and an argument"
    (testing "Clears the selection when the argument is nil"
      (let [jlist (javax.swing.JList. (to-array [1 2 3 4 5 6 7]))]
        (.setSelectionInterval jlist 1 3)
        (is (= jlist (selection! jlist nil)))
        (is (nil? (selection jlist)))))
    (testing "Selects the given *values* when argument is a non-empty seq"
      (let [jlist (javax.swing.JList. (to-array [1 "test" 3 4 5 6 7]))]
        (is (= jlist (selection! jlist {:multi? true} ["test" 4 6])))
        (is (= ["test" 4 6] (selection jlist {:multi? true})))
        (is (= "test" (selection jlist))))))

  (testing "when given a text component"
    (testing "Clears the selection when the argument is nil"
      (let [t (javax.swing.JTextArea. "This is some text with a selection")]
        (.select t 5 10)
        (selection! t nil)
        (is (nil? (selection t)))))
    (testing "sets the selection given a [start end] range vector"
      (let [t (javax.swing.JTextArea. "THis is more text with a selection")]
        (selection! t [4 9])
        (is (= [4 9] (selection t))))))

  (testing "when given a JTabbedPane"
    (testing "selects a tab by title when given a string"
      (let [tp (sc/tabbed-panel :tabs [{:title "A" :content "A"}
                                       {:title "B" :content "B"}])]
        (is (= 0 (.getSelectedIndex tp)))
        (selection! tp "B")
        (is (= 1 (.getSelectedIndex tp)))))
    (testing "selects a tab by index when given a number"
      (let [tp (sc/tabbed-panel :tabs [{:title "A" :content "A"}
                                       {:title "B" :content "B"}])]
        (is (= 0 (.getSelectedIndex tp)))
        (selection! tp 1)
        (is (= 1 (.getSelectedIndex tp)))))
    (testing "selects a tab by content when given a widget"
      (let [b (sc/label :text "B")
            tp (sc/tabbed-panel :tabs [{:title "A" :content "A"}
                                        {:title "B" :content b}])]
        (selection! tp b)
        (is (= 1 (.getSelectedIndex tp)))))
    (testing "selects a tab by map keys"
      (let [b (sc/label :text "B")
            tp (sc/tabbed-panel :tabs [{:title "A" :content "A"}
                                        {:title "B" :content b}])]
        (selection! tp {:index 1})
        (is (= 1 (.getSelectedIndex tp)))

        (selection! tp {:title "A"})
        (is (= 0 (.getSelectedIndex tp)))

        (selection! tp {:content b})
        (is (= 1 (.getSelectedIndex tp))))))

  (testing "when given a JTable and an argument"
    (testing "Clears the row selection when the argument is nil"
      (let [jtable (javax.swing.JTable. 5 3)]
        (.setRowSelectionInterval jtable 1 3)
        (is (= jtable (selection! jtable nil)))
        (is (nil? (selection jtable)))))
    (testing "selects the given rows when argument is a non-empty seq of row indices"
      (let [jtable (javax.swing.JTable. 10 2)]
        (is (= jtable (selection! jtable {:multi? true } [0 2 4 6 8 9])))
        (is (= [0 2 4 6 8 9] (selection jtable {:multi? true})))
        (is (= 0 (selection jtable)))))))

