;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.keystroke
  (:use seesaw.keystroke)
  (:use clojure.test
        )
  (:import [javax.swing KeyStroke]
           [java.awt Toolkit]))

(deftest keystroke-test
  (testing "creates a keystroke from a descriptor string"
    (let [ks (keystroke "ctrl S")]
      (is (= KeyStroke (class ks)))
      (is (= java.awt.event.KeyEvent/VK_S (.getKeyCode ks))))))

(deftest keystroke-test
  (testing "returns nil for nil input"
    (nil? (keystroke nil)))
  (testing "returns input if it's a KeyStroke"
    (let [ks (KeyStroke/getKeyStroke "alt X")]
      (is (= ks (keystroke ks)))))
  (testing "returns a keystroke for a string"
    (let [ks (keystroke "alt X")]
      (is (= java.awt.event.KeyEvent/VK_X (.getKeyCode ks)))))
  (testing "substitute platform-specific menu modifier for \"menu\" modifier"
    (let [ks (keystroke "menu X")]
      (is (= java.awt.event.KeyEvent/VK_X (.getKeyCode ks)))
      (is (= (.. (Toolkit/getDefaultToolkit) getMenuShortcutKeyMask) (bit-and 7 (.getModifiers ks))))))
  (testing "returns a keystroke for a char"
    (let [ks (keystroke \A)]
      (is (= \A (.getKeyChar ks))))))

