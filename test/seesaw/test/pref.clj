;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.pref
  (:use seesaw.pref)
  (:use clojure.test
        ))

(deftest preference-atom-test
  (testing "should return an atom with nil as its default value"
    (do (.remove (preferences-node) (pr-str "key"))
     (let [atom (preference-atom "key")]
       (is (nil? @atom)))))
  (testing "should return an atom with the specified default value"
    (do (.remove (preferences-node) (pr-str "key"))
        (let [atom (preference-atom "key" 'some-value)]
          (is (= @atom 'some-value)))))
  (testing "should keep pref in sync with atom"
    (do (.remove (preferences-node) (pr-str "key"))
        (let [atom (preference-atom "key")]
          (reset! atom 'new-value)
          (is (= (read-string (.get (preferences-node) (pr-str "key") "nil")) 'new-value))))))
