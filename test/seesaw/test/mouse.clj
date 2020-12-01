;  Copyright (c) Dave Ray, 2012. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.mouse
  (:require [seesaw.mouse :as mouse])
  (:use clojure.test
        ))

(defn- fake-event [[x y] modex btn]
  (java.awt.event.MouseEvent.
    (javax.swing.JLabel.) ,
    0, 0 , modex,
    x, y, 1, false,
    btn))

(deftest mouse-location-test
  (testing "with no arguments"
    (testing "returns the [x y] mouse location on the whole screen"
      (let [[x y] (mouse/location)
            p     (.getLocation (java.awt.MouseInfo/getPointerInfo))]
        (is (= (.x p) x))
        (is (= (.y p) y)))))
  (testing "with a MouseEvent argument"
    (testing "returns the [x y] of the event"
      (let [e (fake-event [123 456] 0 0)]
        (is (= [123 456] (mouse/location e)))))))

(deftest mouse-button-down?-test
  (testing "with a MouseEvent"
    (testing "returns true if the button is down"
      (let [e (fake-event [0 0] java.awt.event.InputEvent/BUTTON2_DOWN_MASK 0)]
        (is (mouse/button-down? e :center))))))

(deftest mouse-button-test
  (testing "with a MouseEvent"
    (testing "returns the button whose state changed"
      (let [e (fake-event [0 0] 0 java.awt.event.MouseEvent/BUTTON3)]
        (is (= :right (mouse/button e)))))))
