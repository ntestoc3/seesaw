;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.timer
  (:use seesaw.timer)
  (:use clojure.test
        )
  (:import [javax.swing Action]))

(deftest timer-test
  (testing "Creates a timer for a handler function and calls it"
    (let [called (atom nil)
          t (timer #(inc (reset! called %)) :start? false :initial-value 99)]
      (.actionPerformed (first (.getActionListeners t)) nil)
      (is (= 99 @called))
      (.actionPerformed (first (.getActionListeners t)) nil)
      (is (= 100 @called))))

  (testing "Sets timer properties"
    (let [t (timer identity :start? false :initial-delay 123 :delay 456 :repeats? false)]
      (is (= 123 (.getInitialDelay t)))
      (is (= 456 (.getDelay t)))
      (is (not (.isRunning t)))
      (is (not (.isRepeats t))))))

