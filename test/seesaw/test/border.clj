;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.border
  (:use seesaw.border)
  (:use clojure.test
        )
  (:import [javax.swing.border EmptyBorder LineBorder MatteBorder TitledBorder]
           [java.awt Insets Color]))

(deftest empty-border-test
  (testing "creates a 1 pixel border by default"
      (let [b (empty-border)]
        (is (= EmptyBorder (class b)))
        (is (= (Insets. 1 1 1 1) (.getBorderInsets b)))))
  (testing "creates a border with same thickness on all sides"
    (let [b (empty-border :thickness 11)]
      (is (= EmptyBorder (class b)))
      (is (= (Insets. 11 11 11 11) (.getBorderInsets b)))))
  (testing "creates a border with specified sides"
    (let [b (empty-border :top 2 :left 3 :bottom 4 :right 5)]
      (is (= EmptyBorder (class b)))
      (is (= (Insets. 2 3 4 5) (.getBorderInsets b)))))
  (testing "creates a border with specified sides, defaulting to 0"
    (let [b (empty-border :left 3 )]
      (is (= EmptyBorder (class b)))
      (is (= (Insets. 0 3 0 0) (.getBorderInsets b))))))

(deftest line-border-test
  (testing "creates a black, one pixel border by default"
    (let [b (line-border)]
      (is (= LineBorder (class b)))
      (is (= 1 (.getThickness b)))
      (is (= Color/BLACK (.getLineColor b)))))
  (testing "creates a border with desired color (using to-color) and thickness"
    (let [b (line-border :thickness 12 :color "#FFFF00")]
      (is (= LineBorder (class b)))
      (is (= 12 (.getThickness b)))
      (is (= Color/YELLOW (.getLineColor b)))))
  (testing "creates a matte border with specified sides and color"
    (let [b (line-border :top 2 :left 3 :bottom 4 :right 5 :color "#FFFF00")]
      (is (= MatteBorder (class b)))
      (is (= (Insets. 2 3 4 5) (.getBorderInsets b)))
      (is (= Color/YELLOW (.getMatteColor b)))))
  (testing "creates a matte border with specified sides, defaulting to 0"
    (let [b (line-border :top 2)]
      (is (= MatteBorder (class b)))
      (is (= (Insets. 2 0 0 0) (.getBorderInsets b)))
      (is (= Color/BLACK (.getMatteColor b))))))

(deftest compound-border-test
  (testing "creates nested compound borders inner to outer"
    (let [in (line-border)
          mid (line-border)
          out (line-border)
          b (compound-border in mid out)]
      (is (= out (.getOutsideBorder b)))
      (is (= mid (.. b (getInsideBorder) (getOutsideBorder))))
      (is (= in (.. b (getInsideBorder) (getInsideBorder)))))))

(deftest to-border-test
  (testing "returns nil given nil"
    (nil? (to-border nil)))
  (testing "returns input if it's already a border"
    (let [b (line-border)]
      (is (= b (to-border b)))))
  (testing "creates an empty border with specified thickness for a number"
    (let [b (to-border 11)]
      (is (= EmptyBorder (class b)))
      (is (= (Insets. 11 11 11 11) (.getBorderInsets b)))))
  (testing "returns a titled border using a resource bundle if given an i18n keyword"
    (let [b (to-border ::titled-border-test)]
      (is (= TitledBorder (class b)))
      (is (= "Test value from border.properties" (.getTitle b)))))
  (testing "returns a titled border using str if it doesn't know what to do"
    (let [b (to-border "Test")]
      (is (= TitledBorder (class b)))
      (is (= "Test" (.getTitle b)))))
  (testing "creates a compound border out of multiple args"
      (let [b (to-border "Inner" "Outer")]
        (is (= "Outer" (.. b getOutsideBorder getTitle)))
        (is (= "Inner" (.. b getInsideBorder getTitle)))))
  (testing "creates a compound border out of a collection arg"
      (let [b (to-border ["Inner" "Outer"])]
        (is (= "Outer" (.. b getOutsideBorder getTitle)))
        (is (= "Inner" (.. b getInsideBorder getTitle))))))

(deftest custom-border-test
  (testing "creates a custom border implementation"
    (instance? javax.swing.border.Border (custom-border)))
  (testing "returns integer insets"
    (let [b (custom-border :insets 3)]
      (is (= (Insets. 3 3 3 3) (.getBorderInsets b nil)))))
  (testing "returns static vector insets"
    (let [b (custom-border :insets [1 2 3 4])]
      (is (= (Insets. 1 2 3 4) (.getBorderInsets b nil)))))
  (testing "calls a insets function"
    (let [b (custom-border :insets (constantly [1 2 3 4]))]
      (is (= (Insets. 1 2 3 4) (.getBorderInsets b nil)))))
  (testing "returns constant opaque? value"
    (let [b (custom-border :opaque? true)]
      (is (.isBorderOpaque b))))
  (testing "returns function opaque? value"
    (let [b (custom-border :opaque? (constantly true))]
      (is (.isBorderOpaque b))))
  (testing "calls provided paint function"
    (let [called (atom false)
          b (custom-border :paint (fn [c g x y w h] (reset! called true)))]
      (.paintBorder b nil nil 0 0 0 0)
      (is @called))))

