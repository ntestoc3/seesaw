;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.style
  (:use seesaw.style
        [seesaw.core :only [border-panel label button config text]]
        [seesaw.color :only [to-color]])
  (:use clojure.test
        ))

(deftest apply-stylesheet-test
  (testing "returns its input"
    (let [lbl (label)]
      (is (= lbl (apply-stylesheet lbl {})))))

  (testing "changes styles of widget for rules that match"
    (let [lbl (label :id :lbl)
          btn-a (button :class :btn)
          btn-b (button :class :btn :id :btn-b)
          p (border-panel :center lbl :north btn-a :south btn-b)]
      (apply-stylesheet p
        {[:#lbl] { :background :aliceblue
                   :text "hi"}
         [:.btn] { :foreground :red }
         [:#btn-b] {:text "B"}})
      (is (= (to-color :aliceblue) (config lbl :background)))
      (is (= "hi" (text lbl)))
      (is (= "B" (text btn-b)))
      (is (= (to-color :red) (config btn-a :foreground)))
      (is (= (to-color :red) (config btn-b :foreground))))))
