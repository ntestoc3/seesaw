;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.icon
  (:use seesaw.icon)
  (:require [seesaw.graphics :as g]
            [clojure.java.io :as jio])
  (:use clojure.test
        ))

(deftest icon-test
  (testing "returns nil given nil"
    (nil? (icon nil)))
  (testing "returns its input given an Icon"
    (let [i (javax.swing.ImageIcon.)]
      (is (= i (icon i)))))
  (testing "returns an icon given an image"
    (let [image (g/buffered-image 16 16)
          i (icon image)]
      (is (instance? javax.swing.ImageIcon i))
      (is (= image (.getImage i)))))
  (testing "returns an icon given a URL"
    (let [i (icon (jio/resource "seesaw/test/examples/rss.gif"))]
      (is (instance? javax.swing.ImageIcon i))))
  (testing "returns an icon given a path to an icon on the classpath"
    (let [i (icon "seesaw/test/examples/rss.gif")]
      (is (instance? javax.swing.ImageIcon i))))
  (testing "returns an icon given a File"
    (let [i (icon (java.io.File. "test/seesaw/test/examples/rss.gif"))]
      (is (instance? javax.swing.ImageIcon i))))
  (testing "returns an icon given a i18n keyword"
    (let [i (icon ::test-icon)]
      (is (instance? javax.swing.ImageIcon i)))))

