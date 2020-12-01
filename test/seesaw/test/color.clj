;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.color
  (:use seesaw.color)
  (:use clojure.test
        )
  (:import [java.awt Color]))

(deftest get-rgba-test
  (testing "returns vector [r g b a] as integers"
    (= [1 2 3 4] (get-rgba (color 1 2 3 4)))))

(deftest color-test
  (testing "can create a color from rgb integers"
    (let [c (color 1 2 3)]
      (is (= (Color. 1 2 3) c))))
  (testing "can create a color from rgba integers"
    (let [c (color 1 2 3 4)]
      (is (= (Color. 1 2 3 4) c))))
  (testing "can create a color from a #-prefixed 6-digit rgb hex string"
    (let [c (color "#010203")]
      (is (= (Color. 1 2 3) c))))
  (testing "can create a color from a #-prefixed 3-digit rgb hex string"
    (let [c (color "#fed")]
      (is (= (Color. 0xff 0xee 0xdd) c))))
  (testing "can create a color from a #-prefixed rgb hex keyword"
    (let [c (color :#010203)]
      (is (= (Color. 1 2 3) c))))
  (testing "can create a color from a #-prefixed rgb hex string and alpha"
    (let [c (color "#010203" 23)]
      (is (= (Color. 1 2 3 23) c))))
  (testing "can create a color from a #-prefixed rgb hex keyword and alpha"
    (let [c (color :#010203 23)]
      (is (= (Color. 1 2 3 23) c))))
  (testing "can create a color from a CSS-style name"
    (is (= (Color. 240 248 255) (color "aliceblue"))))
  (testing "can create a color from a CSS-style keyword name"
    (is (= (Color. 0 255 127) (color :springgreen))))
  (testing "can create a color from a mixed-case CSS-style name"
    (is (= (Color. 240 248 255) (color "AlIceBlUe")))))

(deftest to-color-test
  (testing "returns nil for nil input"
    (nil? (to-color nil)))
  (testing "returns its input if its a color"
      (is (= Color/BLACK (to-color Color/BLACK)))))

(deftest default-color-test
  (testing "retrieve a default color from the UIManager"
    (let [name "Label.foreground"
          c (default-color name)
          expected (.getColor (javax.swing.UIManager/getDefaults) name)]
      (is (not (nil? c)))
      (is (= c expected)))))


