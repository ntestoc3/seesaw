;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.action
  (:use [seesaw.action]
        [seesaw.core :only [config]]
        [seesaw.keystroke :only [keystroke]])
  (:use clojure.test
        )
  (:import [javax.swing Action]))

(deftest action-test
  (testing "sets the name, tooltip, and command"
    (let [a (action :name "Test" :tip "This is a tip" :command "Go!")]
      (is (instance? Action a))
      (is (.isEnabled a))
      (is (= "Test" (.getValue a Action/NAME)))
      (is (= "Go!" (.getValue a Action/ACTION_COMMAND_KEY)))
      (is (not (.getValue a Action/SELECTED_KEY)))
      (is (= "This is a tip" (.getValue a Action/SHORT_DESCRIPTION)))))
  (testing "sets the mnemonic of the action given an integer key code"
    (let [m (.getValue (action :mnemonic 99) Action/MNEMONIC_KEY)] 
      ; For Clojure 1.3, ensure that it's an Integer in there and not a Long
      (is (instance? java.lang.Integer m))
      (is (= 99 m))))
  (testing "sets the mnemonic of the action given a character"
    (let [m (.getValue (action :mnemonic \T) Action/MNEMONIC_KEY)]
      ; For Clojure 1.3, ensure that it's an Integer in there and not a Long
      (is (instance? java.lang.Integer m))
      (is (= (int \T) m))))
  (testing "sets the mnemonic of the action given a lower-case character"
    (let [m (.getValue (action :mnemonic \t) Action/MNEMONIC_KEY)] 
      ; For Clojure 1.3, ensure that it's an Integer in there and not a Long
      (is (instance? java.lang.Integer m))
      (is (= (int \T) m))))
  (testing "calls the handler when actionPerformed is called"
    (let [called (atom false)
          f (fn [e] (reset! called true))
          a (action :handler f)]
      (.actionPerformed a nil)
      (is @called)))
  (testing "does nothing when actionPerformed is called and no handler is installed"
    (let [a (action)]
      (.actionPerformed a nil)
      ; Just making sure no exception was thrown
      true))
  (testing "handles the :key option"
    (let [a (action :key "menu T")
          ks (.getValue a Action/ACCELERATOR_KEY)]
      (is (not (nil? ks)))
      (is (instance? javax.swing.KeyStroke ks))))
  (testing "handles the :enabled? option"
    (not (.isEnabled (action :enabled? false))))
  (testing "handles the :selected? option"
    (.getValue (action :selected? true) Action/SELECTED_KEY))

  (testing "loads resources by convention with :resource option"
    (let [a (action :resource ::my-action)]
      (is (instance? javax.swing.Icon (config a :icon)))
      (is (= (int \X) (config a :mnemonic)))
      (is (= "A command" (config a :command)))
      (is (= "A name" (config a :name)))
      (is (= "A tip" (config a :tip)))
      (is (= (keystroke "ctrl C") (config a :key)))))

  (testing "loads :icon from a resource"
    (is (instance? javax.swing.Icon (config (action :icon ::my-action.icon) :icon))))
  (testing "loads :mnemonic from a resource"
    (is (= (int \X) (config (action :mnemonic ::my-action.mnemonic) :mnemonic))))
  (testing "loads :command from a resource"
    (is (= "A command" (config (action :command ::my-action.command) :command))))
  (testing "loads :name from a resource"
    (is (= "A name" (config (action :name ::my-action.name) :name))))
  (testing "loads :key from a resource"
    (is (= (keystroke "ctrl C") (config (action :key ::my-action.key) :key))))
  (testing "loads :tip from a resource"
    (is (= "A tip" (config (action :tip ::my-action.tip) :tip)))))

