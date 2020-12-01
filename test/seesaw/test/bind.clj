(ns seesaw.test.bind
  (:refer-clojure :exclude [some filter])
  (:require [seesaw.core :as ssc])
  (:use seesaw.bind)
  (:use clojure.test
        ))

(deftest bind-test
  (testing "returns a composite bindable"
    (let [a (atom 0) b (atom 1) c (atom 2) d (atom 3)
          cb (bind a b c d)
          called (atom nil) ]
      (is (satisfies? Bindable cb))

      ; make sure that subscribing to the composite subscribes
      ; to the *end* of the chain!
      (subscribe cb (fn [v] (reset! called v)))
      (reset! d 10)
      (is (= 10 @called))))

  (testing "can chain bindables together"
    (let [a (atom 1)
          b (atom nil)]
      (bind a (transform + 5) b)
      (reset! a 5)
      (is (= 10 @b))))

  (testing "returns something function-like that can be called to undo the binding"
    (let [a (atom 0) b (atom 1) c (atom 2)
          cb (bind a b c)]
      (reset! a 5)
      (is (= [5 5 5] [@a @b @c]))
      (cb)
      (reset! a 6)
      (is (= [6 5 5] [@a @b @c]))))
  
  (testing "can chain bindables, including composites, together"
    (let [a (atom 1)
          b (atom nil)]
      (bind a (bind (transform + 5) (transform * 2) (atom nil)) b)
      (reset! a 2)
      (is (= 14 @b))))

  (testing "should sync the enabled? property of a widget with an atom"
    (let [v (atom true)
          b (ssc/button)]
      (bind (property b :enabled?) v)
      (ssc/config! b :enabled? false)
      (is (not @v))
      (reset! v true)))

  (testing "should sync the enabled? property of a widget with an atom"
    (let [v (atom true)
          b (ssc/button)]
      (bind (property b :enabled?) v)
      (ssc/config! b :enabled? false)
      (is (not @v))))

  (testing "should sync an atom to the enabled? property of a widget"
    (let [v (atom true)
          b (ssc/button)]
      (bind v (property b :enabled?))
      (reset! v false)
      (is (not (.isEnabled b)))))

  (testing "with a BoundedRangeModel"
    (testing "Updates an atom when the model changes"
      (let [a (atom -1)
            m (javax.swing.DefaultBoundedRangeModel. 50 0 2 100)]
        (bind m a)
        (.setValue m 51)
        (is (= 51 @a))))
    (testing "Updates the model when the atom changes"
      (let [a (atom -1)
            m (javax.swing.DefaultBoundedRangeModel. 50 0 2 100)]
        (bind a m)
        (reset! a 99)
        (is (= 99 (.getValue m))))))

  (testing "given a text field"
    (testing "should update an atom when the underlying document changes"
      (let [a (atom nil)
            t (ssc/text "initial")]
        (bind (.getDocument t) a)
        (ssc/text! t "foo")
        (is (= "foo" @a))))

    (testing "should update the underlying document when the atom changes"
      (let [a (atom "initial")
            t (ssc/text "")]
        (bind a (.getDocument t))
        (reset! a "foo")
        (is (= "foo" (ssc/text t))))))

  (testing "given a slider"
    (testing "should sync the value of the atom with the slider value, if slider value changed"
      (let [v (atom 15)
            sl (ssc/slider :value @v)]
        (bind (.getModel sl) v)
        (.setValue sl 20)
        (is (= @v 20))))
    (testing "should sync the value of the slider with the atom value, if atom value changed"
      (let [v (atom 15)
            sl (ssc/slider :value @v)]
        (bind v (.getModel sl))
        (reset! v 20)
        (is (= (.getValue sl) 20)))))
 
  (testing "given a toggle button (or any button/menu)"
    (testing "should sync the selection state of the button"
      (let [v (atom nil)
            b (ssc/toggle :selected? false)]
        (bind b v)
        (.setSelected b true)
        (is @v)))

    (testing "should sync the selection state of the button"
      (let [v (atom nil)
            b (ssc/toggle :selected? false)]
        (bind v b)
        (reset! v true)
        (is (.isSelected b)))))

  (testing "given a combobox"
    (testing "should sync the selection state of the combobox"
      (let [v (atom nil)
            b (ssc/combobox :model [1 2 3 4])]
        (bind b v)
        (ssc/selection! b 2)
        (is (= 2 @v))))

    (testing "should sync the selection state of the combobox"
      (let [v (atom nil)
            b (ssc/combobox :model [1 2 3 4])]
        (bind v b)
        (reset! v 4)
        (is (= 4 (ssc/selection b))))))

  (testing "given a spinner"
    (testing "should sync the value of the atom with the spinner value, if spinner value changed"
      (let [v (atom 15)
            sl (ssc/spinner :model @v)]
        (bind (.getModel sl) v)
        (.setValue sl 20)
        (is (= @v 20))))
    (testing "should sync the value of the spinner with the atom value, if atom value changed"
      (let [v (atom 15)
            sl (ssc/spinner :model @v)]
        (bind v (.getModel sl))
        (reset! v 20)
        (is (= (.getValue sl) 20)))))

  (testing "given an agent"

    (testing "should pass along changes to the agent's value"
      (let [start (agent nil)
            end   (atom nil)]
        (bind start end)
        (send start (constantly :called))
        (await start)
        (is (= :called @start))
        (is (= :called @end))))

    (testing "should throw an exception if you try to notify an agent"
      (let [start (atom nil)]
        (bind start (agent nil))
        (is (try
                  (reset! start 99)
                  false
                  ; In Clojure 1.3, the exception propagates correctly
                  (catch IllegalStateException e
                    true)
                  ; Unfortunately, in Clojure 1.2, IllegalStateException gets wrapped by reset!
                  (catch RuntimeException e
                    (= IllegalStateException (class (.getCause e)))))))))

  (testing "given a Ref"
    (testing "should pass along changes to the ref's value"
      (let [start (ref nil)
            end   (atom nil)]
        (bind start end)
        (dosync
         (alter start (constantly "foo")))
        (is (= "foo" @start))
        (is (= "foo" @end))))
    (testing "should pass update the ref's value when the source changes"
      (let [start (atom nil)
            end   (ref nil)]
        (bind start end)
        (reset! start "foo")
        (is (= "foo" @start))
        (is (= "foo" @end)))))        )

(deftest b-do*-test
  (testing "executes a function with a single argument and ends a chain"
    (let [start (atom 0)
          called (atom nil) ]
      (bind start (b-do* #(reset! called %)))
      (reset! start 5)
      (is (= 5 @called)))))

(deftest b-do-test
  (testing "executes body with a single argument and ends a chain"
    (let [start (atom [1 2])
          called (atom nil)]
      (bind start (b-do [[a b]] (reset! called (+ a b))))
      (reset! start [3 4])
      (is (= 7 @called)))))

(deftest tee-test
  (testing "creates a tee junction in a bind"
    (let [start (atom 0)
          end1  (atom 0)
          end2  (atom 0)]
      (bind start (tee (bind (transform * 2) end1)
                       (bind (transform * 4) end2)))
      (reset! start 5)
      (is (= 10 @end1))
      (is (= 20 @end2)))))

(deftest funnel-test
  (testing "create a funnel in a bind which listens to multiple source and produces a vector of values"
    (let [a (atom 0)
          b (atom 1)
          f (funnel a b)
          end (atom nil)]
      (bind f end)
      (reset! a 5)
      (is (= [5 nil] @end))
      (reset! b 6)
      (is (= [5 6] @end)))))

(deftest filter-test
  (testing "doesn't pass along value when predicate returns falsey"
    (let [start (atom :foo)
          end   (atom :bar)]
      (bind start (filter (constantly false)) end)
      (reset! start :something)
      (is (= :bar @end))))
  (testing "passes along value when predicate returns truthy"
    (let [start (atom :foo)
          end   (atom :bar)]
      (bind start (filter (constantly true)) end)
      (reset! start :something)
      (is (= :something @end)))))

(deftest some-test
  (testing "doesn't pass along falsey values returned by the predicate"
    (let [start (atom :foo)
          end   (atom :bar)]
      (bind start (some (constantly nil)) end)
      (reset! start :something)
      (is (= :bar @end))))
  (testing "passes along result of predicate when it returns truthy"
    (let [start (atom :foo)
          end   (atom :bar)]
      (bind start (some (constantly :yum)) end)
      (reset! start :something)
      (is (= :yum @end)))))

(deftest selection-test
  (testing "sends out selection changes on a widget"
    (let [lb (ssc/listbox :model [:a :b :c])
          output (atom nil)]
      (bind (selection lb) output)
      (ssc/selection! lb :b)
      (is (= :b @output))))
  (testing "maps its input to the selection of a widget"
    (let [input (atom nil)
          lb (ssc/listbox :model [:a :b :c])]
      (bind input (selection lb))
      (reset! input :b)
      (is (= :b (ssc/selection lb))))))

(deftest value-test
  (testing "maps its input to the value of a widget"
    (let [input (atom nil)
          lb (ssc/listbox :id :lb :model [:a :b :c])
          tb (ssc/text :id :text)
          p  (ssc/border-panel :north lb :center tb)]
      (bind input (value p))
      (reset! input {:lb :b :text "hi"})
      (is (= {:lb :b :text "hi"} (ssc/value p))))))

(deftest to-bindable-test
  (testing "returns arg if it's already bindable"
    (let [a (atom nil)]
      (is (= a (to-bindable a)))))
  (testing "converts a text component to its document"
    (let [t (ssc/text)]
      (is (= (.getDocument t) (to-bindable t)))))
  (testing "converts a slider to its model"
    (let [s (ssc/slider)]
      (is (= (.getModel s) (to-bindable s))))))

(deftest b-swap!-test
  (testing "acts like swap! passing the old value, new value, and additional args to a function"
    (let [start (atom nil)
          target (atom [])
          end (atom nil)]
      (bind start 
            (b-swap! target conj) 
            end)
      (reset! start 1)
      (reset! start 2)
      (reset! start 3)
      (is (= [1 2 3] @target))
      (is (= @end @target)))))

(deftest b-send-test
  (testing "acts like send passing the old value, new value, and additional args to a function"
    (let [start  (atom nil)
          target (agent [])]
      (bind start 
            (b-send target conj) )
      (reset! start 1)
      (reset! start 2)
      (reset! start 3)
      (await target)
      (is (= [1 2 3] @target)))))

(deftest b-send-off-test
  (testing "acts like sendoff passing the old value, new value, and additional args to a function"
    (let [start  (atom nil)
          target (agent [])]
      (bind start 
            (b-send-off target conj) )
      (reset! start 1)
      (reset! start 2)
      (reset! start 3)
      (await target)
      (is (= [1 2 3] @target)))))

(deftest notify-later-test
  (testing "passes incoming values to the swing thread with invoke-later"
    (let [start (atom nil)
          end   (atom nil)
          p     (promise)]
      (bind start
            (notify-later)
            (transform (fn [v] {:value v :edt? (javax.swing.SwingUtilities/isEventDispatchThread)}))
            end)
      (subscribe end (fn [v] (deliver p :got-it)))
      (reset! start 99)
      (is (= :got-it @p))
      (is (= {:value 99 :edt? true} @end)))))

(deftest notify-soon-test
  (testing "passes incoming values to the swing thread with invoke-soon"
    (let [start (atom nil)
          end   (atom nil)]
      (bind start
            (notify-soon)
            (transform (fn [v] {:value v :edt? (javax.swing.SwingUtilities/isEventDispatchThread)}))
            end)
      (ssc/invoke-now (reset! start 99))
      (is (= {:value 99 :edt? true} @end)))))

(deftest notify-now-test
  (testing "passes incoming values to the swing thread with invoke-now"
    (let [start (atom nil)
          end   (atom nil)]
      (bind start
            (notify-now)
            (transform (fn [v] {:value v :edt? (javax.swing.SwingUtilities/isEventDispatchThread)}))
            end)
      (reset! start 99)
      (is (= {:value 99 :edt? true} @end)))))

(deftest subscribe-test
  (testing "on an atom"
    (testing "should return a function that unsubscribes"
      (let [calls (atom 0)
            target (atom "hi")
            unsubscribe (subscribe target (fn [_] (swap! calls inc)))]
        (reset! target "a")
        (is (= 1 @calls))
        (unsubscribe)
        (reset! target "b")
        (is (= 1 @calls)))))

  (testing "on an agent"
    (testing "should return a function that unsubscribes"
      (let [calls (atom 0)
            target (agent "hi")
            unsubscribe (subscribe target (fn [_] (swap! calls inc)))]
        (await (send target (fn [_] "a")))
        (is (= 1 @calls))
        (unsubscribe)
        (await (send target (fn [_] "b")))
        (is (= 1 @calls)))))
  
  (testing "on a javax.swing.text.Document"
    (testing "should return a function that unsubscribes"
      (let [calls (atom 0)
            target (.getDocument (ssc/text))
            unsub  (subscribe target (fn [_] (swap! calls inc)))]
        (.insertString target 0 "hi" nil)
        (is (= 1 @calls))
        (unsub)
        (.insertString target 0 "bye" nil)
        (is (= 1 @calls))
        )))
  (testing "on a javax.swing.BoundedRangeModel"
    (testing "should return a function that unsubscribes"
      (let [calls (atom 0)
            target (javax.swing.DefaultBoundedRangeModel. 50 0 2 100)
            unsub  (subscribe target (fn [_] (swap! calls inc)))]
        (.setValue target 1)
        (is (= 1 @calls))
        (unsub)
        (.setValue target 2)
        (is (= 1 @calls)))))
  
  (testing "on a funnel"
    (testing "should return a function that unsubscribes"
      (let [calls  (atom 0)
            a      (atom "hi")
            target (funnel a)
            unsub  (subscribe target (fn [_] (swap! calls inc)))]
        (reset! a "bye")
        (is (= 1 @calls))
        (unsub)
        (reset! a "hi")
        (is (= 1 @calls))
        )))
  
  (testing "on a widget property"
    (testing "should return a function that unsubscribes"
      (let [calls  (atom 0)
            w      (ssc/text)
            target (property w :enabled?)
            unsub  (subscribe target (fn [_] (swap! calls inc)))]
        (.setEnabled w false)
        (is (= 1 @calls))
        (unsub)
        (.setEnabled w true)
        (is (= 1 @calls))
        )))
  (testing "on a transform"
    (testing "should return a function that unsubscribes"
      (let [calls  (atom 0)
            target (transform identity)
            unsub  (subscribe target (fn [_] (swap! calls inc)))]
        (notify target "hi")
        (is (= 1 @calls))
        (unsub)
        (notify target "bye")
        (is (= 1 @calls))
        )))
  (testing "on a filter"
    (testing "should return a function that unsubscribes"
      (let [calls  (atom 0)
            target (filter (constantly true))
            unsub  (subscribe target (fn [_] (swap! calls inc)))]
        (notify target "hi")
        (is (= 1 @calls))
        (unsub)
        (notify target "bye")
        (is (= 1 @calls)))))
  (testing "on a some"
    (testing "should return a function that unsubscribes"
      (let [calls  (atom 0)
            target (some (constantly true))
            unsub  (subscribe target (fn [_] (swap! calls inc)))]
        (notify target "hi")
        (is (= 1 @calls))
        (unsub)
        (notify target "bye")
        (is (= 1 @calls))
        ))))

