;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.layout
  (:use [seesaw.layout])
  (:use clojure.test
        ))

(deftest handle-structure-change-test
  (testing "should successfully handle all kinds of components"
    (handle-structure-change (proxy [java.awt.Component] []))))

(deftest realize-grid-bag-constraints-test
  (testing "should return a vector of widget/constraint pairs"
    (let [[[w0 c0] [w1 c1] & more] (realize-grid-bag-constraints [[:first :weightx 99 :weighty 555 :gridx :relative] [:second :weightx 100 :anchor :baseline]])]
      (is (nil? more))
      (is (= :first w0))
      (is (= 99.0 (.weightx c0)))
      (is (= 555.0 (.weighty c0)))
      (is (= :second w1))
      (is (= 100.0 (.weightx c1)))
      (is (= 555.0 (.weighty c1))))))
