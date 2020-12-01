;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.options
  (:require [j18n.core :as j18n])
  (:use seesaw.options
        clojure.test
        
        ))

(deftest apply-options-test
  (testing "throws IllegalArgumentException if properties aren't even"
    (try
      (do (apply-options (javax.swing.JPanel.) [1 2 3]) false)
      (catch IllegalArgumentException e true)))
  (testing "throws IllegalArgumentException for an unknown property"
    (try
      (do (apply-options (javax.swing.JPanel.) [:unknown "unknown"]) false)
      (catch IllegalArgumentException e true)))
  (testing "throws IllegalArgumentException for a property with no setter"
    (try
      (do 
        (apply-options (javax.swing.JPanel.) 
                       [:no-setter "no-setter"]) false)
      (catch IllegalArgumentException e true))))

(deftest get-option-value-test
  (testing "throws IllegalArgumentException if target has no handler map"
    (try
      (get-option-value (javax.swing.JPanel.) :text) false
      (catch IllegalArgumentException e true)))
  (testing "throws IllegalArgumentException if option doesn't support getter"
    (try
      (get-option-value (javax.swing.JPanel.) :text [{:text (default-option :text nil nil)}]) false
      (catch IllegalArgumentException e true)))
  (testing "uses the getter of an option to retrieve a value"
    (= "hi" (get-option-value 
              (javax.swing.JPanel.) 
              :text 
              [{:text (default-option :text nil (constantly "hi"))}]))))

(deftest around-option-test
  (testing "calls the provided converter after calling the getter from the wrapped option"
    (= 100 (get-option-value nil 
                             :foo 
                             [{:foo (around-option 
                                     (default-option :foo identity (constantly 99))
                                     identity 
                                     inc)}])))
  (testing "calls the provided converter before calling the setter of the wrapped option"
    (let [result (atom nil)]
      (set-option-value nil 
                        :bar 
                        100
                        [{:bar (around-option
                                (default-option :foo #(reset! result %2))
                                inc
                                identity)}])
      (is (= 101 @result)))))

