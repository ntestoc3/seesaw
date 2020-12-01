;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.selector
  (:use seesaw.selector)
  (:require [seesaw.core :as core])
  (:use clojure.test
        ))

(deftest select-test
  (testing "should find a widget by type, loosely allowing for sub-classing"
    (let [c (core/label)
          d (core/label)
          b (core/toggle)
          p (core/flow-panel :items [c d b])
          f (core/frame :title "select by type" :content p)]
      (is (= [c d] (select f [:<javax.swing.JLabel>])))
      (is (= [b] (select f ["<javax.swing.AbstractButton>"])))))

  (testing "should find a widget by type, strictly"
    (let [c (proxy [javax.swing.JLabel] []) ; should be ignored
          d (javax.swing.JLabel.) ; not ignored
          b (core/toggle) ; should be ignored
          p (core/flow-panel :items [c d b])
          f (core/frame :title "select by type" :content p)]
      (is (= [d] (select f [:<javax.swing.JLabel!>])))
      (is (= nil (seq (select f ["<javax.swing.AbstractButton!>"]))))))

  (testing "should find a widget by Java class name"
    (let [c (proxy [javax.swing.JLabel] [])
          d (core/label)
          b (core/toggle)
          p (core/flow-panel :items [c d b])
          f (core/frame :title "select by type" :content p)]
      (is (= [d] (select f [:JLabel])))
      (is (= nil (seq (select f ["JRadioButton"]))))))

  (testing "should find a widget by class name"
    (let [c (proxy [javax.swing.JLabel] [])
          d (core/label :class :foo)
          b (core/toggle :class #{:foo :bar})
          p (core/flow-panel :items [c d b])
          f (core/frame :title "select by class" :content p)]
      (is (= [d b] (select f [:.foo])))
      (is (= [b] (seq (select f [".bar"]))))))

  (testing "should find all descendants of a widget"
    (let [c (proxy [javax.swing.JLabel] [])
          d (core/label)
          b (core/toggle)
          p2 (core/flow-panel :items [c])
          p (core/flow-panel :id :p :items [p2 d b])
          f (core/frame :title "select by type" :content p)]
      (is (= #{c d b p2} (apply hash-set (select f [:#p :*]))))))

  (testing "should find direct children of a widget"
    (let [c (proxy [javax.swing.JLabel] [])
          d (core/label)
          b (core/toggle)
          p2 (core/flow-panel :items [c])
          p (core/flow-panel :id :p :items [p2 d b])
          f (core/frame :title "select by type" :content p)]
      (is (= #{d b p2} (apply hash-set (select f [:#p :> :*]))))))

  (testing "should find a frame by #id and return it"
    (let [f (core/frame :id :my-frame)]
      (is (= [f] (select f [:#my-frame])))))

  (testing "should find a widget by #id and returns it"
    (let [c (core/label :id "hi")
          p (core/flow-panel :id :panel :items [c])
          f (core/frame :title "select by id" :content p)]
      (is (= [c] (select f [:#hi])))
      (is (= [p] (select f ["#panel"])))))

  (testing "should find menu items by id in a frame's menubar"
    (let [m (core/menu-item :id :my-menu :text "my-menu")
          f (core/frame :title "select menu item"
                   :menubar (core/menubar :items [(core/menu :text "File" :items [(core/menu :text "Nested" :items [m])])]))]
      (is (= [m] (select f [:#my-menu])))))

  (testing "should select all of the components in a tree with :*"
    (let [a (core/label) b (core/text) c (core/label)
          p (core/flow-panel :items [a b c])]
      (is (= [p a b c] (select p [:*]))))))

