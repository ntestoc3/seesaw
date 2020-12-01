;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.util
  (:use seesaw.util)
  (:use clojure.test
        ))

(deftest illegal-argument-test
  (testing "throws a formatted illegal argument exception"
    (try
      (illegal-argument "This %s a message with code %d" "is" 99) false
      (catch IllegalArgumentException e
        (= "This is a message with code 99" (.getMessage e))))))

(deftest check-args-test
  (testing "returns true if the condition is true"
    (check-args true "yes!"))
  (testing "returns throws IllegalArgumentException if condition is false"
    (try 
      (do (check-args false "no!") false)
      (catch IllegalArgumentException e true))))

(deftest cond-doto-test
  (testing "only executes forms with true conditions"
    (= "firstsecondfifth" (str (cond-doto (StringBuilder.) 
         true (.append "first") 
         (> 2 1) (.append "second")
         (< 2 1) (.append "third")
         false (.append "fourth")
         (= "HI" "HI") (.append "fifth"))))))

(deftest to-seq-test
  (testing "makes a non-seq into a single-element seq"
    (= (seq ["hi"]) (to-seq "hi"))
    (= (seq [:k]) (to-seq :k)))
  (testing "makes a collection into a seq"
    (= (seq #{:a :b}) (to-seq #{:a :b}))))


(deftest camelize-test
  (testing "turns dashes into camel humps"
    (= "onMouseClicked" (camelize "on-mouse-clicked"))))

(deftest boolean?-test
  (testing "returns true for true"
    (boolean? true))
  (testing "returns true for false"
    (boolean? false))
  (testing "returns false for nil"
    (not (boolean? nil)))
  (testing "returns false for non-boolean"
    (not (boolean? "hi"))))

(deftest try-cast-test
  (testing "returns its input if cast succeeds"
    (= "TEST" (try-cast java.lang.String "TEST")))
  (testing "returns nil if input is nil"
    (nil? (try-cast java.lang.String nil)))
  (testing "returns nil if cast fails"
    (nil? (try-cast java.lang.String 99))))

(deftest to-url-test
  (testing "returns its input if it is already a URL object"
    (let [u (java.net.URL. "http://google.com")]
      (is (identical? u (to-url u)))))
  (testing "returns a URL if (str input) is a valid URL"
    (= "http://darevay.com" (-> (to-url "http://darevay.com") .toExternalForm )))
  (testing "returns nil if (str input) is not a valid URL"
    (nil? (to-url "not a URL"))))

(deftest to-uri-test
  (testing "returns its input if it is already a URI object"
    (let [u (java.net.URI. "http://google.com")]
      (is (identical? u (to-uri u)))))
  (testing "returns a URI if (str input) is a valid URI"
    (= "http://darevay.com" (-> (to-uri "http://darevay.com") .toString )))
  (testing "returns nil if (str input) is not a valid URI"
    (nil? (to-url "not a URI"))))

(deftest to-dimension-test
  (testing "should throw an exception if it doesn't know what to do"
    (try
      (do (to-dimension {:a :map}) false)
      (catch IllegalArgumentException e true)))
  (testing "should return its input if its already a Dimension"
    (let [d (java.awt.Dimension. 10 20)]
      (is (= d (to-dimension d)))))
  (testing "should return a new Dimension if input is [width :by height]"
    (let [d (to-dimension [1 :by 2])]
      (is (= java.awt.Dimension (class d)))
      (is (= 1 (.width d)))
      (is (= 2 (.height d))))))

(deftest to-insets-test
  (testing "should throw an exception if it doesn't know what to do"
    (try
      (do (to-insets "a random string") false)
      (catch IllegalArgumentException e true)))
  (testing "should return its input if its already an Insets"
    (let [i (java.awt.Insets. 1 2 3 4)]
      (is (= i (to-insets i)))))
  (testing "should return uniform insets from a number"
    (= (java.awt.Insets. 9 9 9 9) (to-insets 9)))
  (testing "should return insets from a 4-element [top, left, bottom, right] vector"
    (= (java.awt.Insets. 1 2 3 4) (to-insets [1 2 3 4])))
  (testing "should return insets from a 2-element [top/bottom, left/right] vector"
    (= (java.awt.Insets. 5 6 5 6) (to-insets [5 6]))))

(deftest atom?-test
  (testing "should return true for an atom"
    (atom? (atom nil)))
  (testing "should return false for a non-atom"
    (not (atom? (ref nil)))))

(deftest to-mnemonic-keycode-test
  (testing "should pass through an integer key code"
    (= 99 (to-mnemonic-keycode 99)))
  (testing "should convert a character to an integer key code"
    (= (int \T) (to-mnemonic-keycode \T)))
  (testing "should convert a lower-case character to an integer key code"
    (= (int \X) (to-mnemonic-keycode \x))))

(deftest resource-key?-test
  (testing "returns true for resource keywords"
    (resource-key? ::hello))
  (testing "returns false for non-resource keywords"
    (not (resource-key? :hello))))

