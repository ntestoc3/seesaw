;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.mig
  (:use seesaw.mig seesaw.core)
  (:use clojure.test
        ))

(deftest mig-panel-test
  (testing "should create a panel with a MigLayout"
    (is (= net.miginfocom.swing.MigLayout (class (.getLayout (mig-panel))))))
  (testing "should set MigLayout layout constraints"
    (let [p (mig-panel :constraints ["wrap 4", "[fill]", "[nogrid]"])
          l (.getLayout p)]
      (is (= "wrap 4" (.getLayoutConstraints l)))
      (is (= "[fill]" (.getColumnConstraints l)))
      (is (= "[nogrid]" (.getRowConstraints l)))))
  (testing "should support the usual default options"
    (mig-panel :id :mig 
               :class :classy 
               :opaque? false
               :listen [:mouse-clicked (fn [_])]
               :foreground :red
               :background :black))
  #_(testing "shouldn't be a baby about hosting a styled-text"
    (let [p (mig-panel :items [[(styled-text) ""]])]
      (-> (frame :content (vertical-panel :items [p])) pack!))))

(deftest replace!-test
  (testing "when called on a panel with a mig layout"
    (testing "replaces the given widget with a new widget and maintains constraints"
      (let [l0 (label "l0")
            l1 (label "l1")
            l2 (label "l2")
            l3 (label "l3")
            p (mig-panel :items [[l0 ""] [l1 "wrap"] [l2 ""]])
            result (replace! p l1 l3)]
        (is (= p result))
        (is (= [l0 l3 l2] (vec (.getComponents p))))
        (is (= "wrap" (-> p .getLayout (.getComponentConstraints l3))))))))

