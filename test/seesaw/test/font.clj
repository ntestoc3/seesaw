;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.font
  (:use seesaw.font)
  (:use clojure.test
        )
  (:import [java.awt Font]))

(deftest font-test
  (testing "can create a font from a font-spec"
    (let [f (font "ARIAL-BOLD-18")]
      (is (= "ARIAL" (.getName f)))
      (is (= 18 (.getSize f)))
      (is (= Font/BOLD (.getStyle f)))))
  (testing "can create a bold font"
    (let [f (font :style :bold )]
      (is (= Font/BOLD (.getStyle f)))))
  (testing "can create a bold & italic font"
    (let [f (font :style #{:bold :italic} )]
      (is (= (bit-or Font/BOLD Font/ITALIC) (.getStyle f)))))
  (testing "can create a plain font"
    (let [f (font)]
      (is (= Font/PLAIN (.getStyle f)))))
  (testing "can create an italic font"
    (let [f (font :style :italic)]
      (is (= Font/ITALIC (.getStyle f)))))
  (testing "can create a font with a specific size"
    (let [f (font :size 40)]
      (is (= 40 (.getSize f)))))
  (testing "can create a font from a family keyword"
    (let [f (font :monospaced)]
      (is (= "Monospaced" (.getFamily f)))))
  (testing "can create a monospace font"
    (let [f (font :name :monospaced)]
      (is (= Font/MONOSPACED (.getName f)))))
  (testing "can create a serif font"
    (let [f (font :name :serif)]
      (is (= Font/SERIF (.getName f)))))
  (testing "can create a sans-serif font"
    (let [f (font :name :sans-serif)]
      (is (= Font/SANS_SERIF (.getName f)))))
  (testing "can create a font with a specific typeface"
    (let [f (font :name "Arial")]
      (is (= "Arial" (.getName f)))))
  (testing "can derive a font from another"
    (let [f (font :from (font :name "Arial") :size 33 :style :bold)]
      (is (= 33 (.getSize f)))
      (is (= Font/BOLD (.getStyle f)))
      (is (= "Arial" (.getName f))))))

(deftest to-font-test
  (testing "returns nil if its input is nil"
    (nil? (to-font nil)))
  (testing "returns its input if its a font"
    (let [f (font)]
      (is (= f (to-font f)))))
  (testing "returns a new font if its input is a font spec"
    (let [f (to-font "ARIAL-ITALIC-14")]
      (is (= Font/ITALIC (.getStyle f)))))
  (testing "returns a new font if its input is a map"
    (let [f (to-font {:style :italic})]
      (is (= Font/ITALIC (.getStyle f))))))

(deftest default-font-test
  (testing "retrieves a named from from the UIManager"
    (let [f (default-font "Label.font")
          expected (.getFont (javax.swing.UIManager/getDefaults) "Label.font")]
      (is (not (nil? f)))
      (is (= expected f)))))

