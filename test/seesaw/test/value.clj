;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.value
  (:use [seesaw.value]
        [seesaw.core])
  (:use clojure.test
        ))

(deftest value*-test
  (testing "returns a map keyed by id for containers"
    (let [a (label :id :a :text "A")
          b (text  :id :b :text "B")
          p (horizontal-panel :items [a
                                      (vertical-panel :id :foo :items [b]) ])
          f (frame :content p)]
      (is (= {:a "A" :b "B"}
                 (value* f)))))

  (testing "returns the value of a progress bar"
    (= 99 (value* (progress-bar :min 0 :max 100 :value 99))))

  (testing "returns the value of a slider"
    (= 99 (value* (slider :min 0 :max 100 :value 99))))

  (testing "returns the selection of a spinner"
    (= 101 (value* (selection! (spinner :model [99 100 101 102]) 101))))

  (testing "returns the selection of a button-y thing (checkbox, button, menu, etc)"
    (is (value* (button :selected? true)))
    (is (false? (value* (button :selected? false)))))

  (testing "returns the selection of a listbox"
    (let [cb (listbox :model ["a" "b" "c"])]
      (selection! cb "b")
      (is (= "b" (value* cb)))))

  (testing "returns the selection of a combobox"
    (let [cb (combobox :model ["a" "b" "c"])]
      (selection! cb "c")
      (is (= "c" (value* cb)))))

  (testing "returns the selection of button-group"
    (let [a (radio)
          b (radio)
          g (button-group :buttons [a b])]
      (selection! g b)
      (is (= b (value* g)))))

  (testing "returns the text of a label"
    (= "bye" (value* (javax.swing.JLabel. "bye"))))

  (testing "returns the text of an editor pane"
    (= "bye" (value* (editor-pane :text "bye"))))

  (testing "returns the text of styled-text"
    (= "bye" (value* (styled-text :text "bye"))))

  (testing "returns the text of a text area"
    (= "bye" (value* (javax.swing.JTextArea. "bye"))))

  (testing "returns the text of a text field"
    (= "hi" (value* (javax.swing.JTextField. "hi")))))

(deftest value!*-test
  (testing "sets the values of widgets with a map keyed by id for containers"
    (let [a (label :id :a :text "")
          b (text  :id :b :text "")
          c (text :id :c :text "unchanged")
          d (text :id :d :text "something")
          p (horizontal-panel :items [a
                                      (vertical-panel :id :foo :items [b c d]) ])
          f (frame :content p)]
      (is (= f (value!* f {:a "A" :b "B" :d nil})))
      (is (= {:a "A" :b "B" :c "unchanged" :d ""} (value* f)))))

  (testing "sets the value of a progress-bar"
    (= 99 (-> (progress-bar :min 0 :max 100 :value 98)
            (value!* 99)
            value*)))

  (testing "sets the value of a slider"
    (= 99 (-> (slider :min 0 :max 100 :value 98)
            (value!* 99)
            value*)))

  (testing "sets the selection of a spinner"
    (= 101 (-> (spinner :model [99 100 101 102])
             (value!* 101)
             value)))

  (testing "sets the selection of a button-y thing (checkbox, button, menu, etc)"
    (is (not (-> (button :selected? true)
              (value!* false)
              value*)))
    (is (-> (button :selected? false)
              (value!* true)
              value*)))

  (testing "sets the selection of a listbox"
    (let [cb (listbox :model ["a" "b" "c"])]
      (is (nil? (value* cb)))
      (value!* cb "b")
      (is (= "b" (value* cb)))))

  (testing "sets the selection of a combobox"
    (let [cb (combobox :model ["a" "b" "c"])]
      (is (= "a" (selection cb)))
      (value!* cb "c")
      (is (= "c" (value* cb)))))
  (testing "sets the selection of button-group"
    (let [a (radio)
          b (radio)
          g (button-group :buttons [a b])]
      (is (nil? (selection g)))
      (value!* g b)
      (is (= b (value* g)))))
  (testing "sets the text of an editor-pane"
    (= "bar" (-> (editor-pane) (value!* "bar") text)))
  (testing "sets the text of a styled-text"
    (= "bar" (-> (styled-text) (value!* "bar") text)))
  (testing "sets the text of a text area"
    (= "bar" (-> (text :multi-line? true) (value!* "bar") text)))
  (testing "sets the text of a text field"
    (= "bar" (-> (text) (value!* "bar") text)))
  (testing "sets the text of a text field to \"\" if value is nil"
    (= "" (-> (text "foo") (value!* nil) text)))
  (testing "sets the text of a label"
    (= "bar" (-> (label) (value!* "bar") text))))

