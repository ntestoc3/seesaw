;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.core
  (:require [seesaw.selector :as selector]
            [seesaw.cursor :as cursor]
            seesaw.meta
            clojure.java.io)
  (:use seesaw.core
        seesaw.font
        seesaw.graphics
        seesaw.cells
        [seesaw.util :only (to-dimension children root-cause)]
        [seesaw.color :only (color)]
        [seesaw.options :only [apply-options]])
  (:use clojure.test
        [clojure.string :only (capitalize split)])
  (:import [javax.swing SwingConstants
                        ScrollPaneConstants
                        Action
                        JFrame
                        JToolBar JTabbedPane
                        JPanel JLabel JButton JTextField JTextArea Box Box$Filler BoxLayout JTextPane
                        JToggleButton JCheckBox JRadioButton
                        JScrollPane
                        JSplitPane]
           [javax.swing.text StyleConstants]
           [java.awt Insets Color Dimension FlowLayout BorderLayout]
           [java.awt.event ActionEvent]))

(deftest id-of-test
  (testing "returns nil if a widget doesn't have an id"
    (nil? (id-of (label))))
  (testing "coerces to a widget before getting the id"
    (let [b (button :id :my-button)
          e (java.awt.event.ActionEvent. b java.awt.event.ActionEvent/ACTION_PERFORMED "")]
      (is (= :my-button (id-of e)))))
  (testing "returns the correct id, as a keyword, if a widget has an id"
    (= (keyword "id of the label") (id-of (label :id "id of the label")))))

(deftest user-data-test
  (testing "returns the value of the :user-data option"
    (= :got-it
       (user-data (combobox :user-data :got-it)))))

(deftest default-options-test
  (testing "the :id option"
    (testing "does nothing when omitted"
      (is (nil? (-> (JPanel.) (apply-options {}) id-of))))
    (testing "sets the component's id as a keyword if given"
      (is (= :hi (-> (JLabel.) (apply-options {:id "hi"}) id-of))))
    (testing "throws IllegalStateException if the widget's id is already set"
      (try
        (do (config! (label :id :foo) :id :bar) false)
        (catch IllegalStateException e true))))

  (testing "the :class option"
    (testing "does nothing when omitted"
      (is (nil? (-> (JPanel.) (apply-options {}) selector/class-of))))
    (testing "sets the class of the widget"
      (is (= #{"foo"} (selector/class-of (flow-panel :class :foo)))))
    (testing "sets the classes of a widget"
      (is (= #{"foo" "bar"} (selector/class-of (flow-panel :class #{:foo :bar}))))))

  (testing "the :paint option"
    (testing "sets the paint-property to a function"
      (let [f (fn [g c])
            b (button :paint f)
            pp (seesaw.meta/get-meta b "seesaw-paint")]
        (is (= f (:after pp))))))
  (testing "the :user-data option"
    (testing "associates user data with the widget"
      (is (= "I'm some user data"
                 (-> (label :user-data "I'm some user data")
                   (config :user-data))))))

  (testing "the :layout option"
    (testing "sets the layout of the widget"
      (let [layout (java.awt.BorderLayout.)
            b (button :layout layout)]
        (is (= layout (config b :layout))))))

  (testing "the :focusable? option"
    (testing "makes a widget focusable"
      (.isFocusable (label :text "focusable" :focusable? true))))

  (testing "the :preferred-size option"
    (testing "set the component's preferred size using to-dimension"
      (let [p (apply-options (JPanel.) {:preferred-size [10 :by 20]})]
        (is (= (Dimension. 10 20) (.getPreferredSize p))))))

  (testing "the :minimum-size option"
    (testing "set the component's minimum size using to-dimension"
      (let [p (apply-options (JPanel.) {:minimum-size [10 :by 20]})]
        (is (= (Dimension. 10 20) (.getMinimumSize p))))))

  (testing "the :maximum-size option"
    (testing "set the component's maximum size using to-dimension"
      (let [p (apply-options (JPanel.) {:maximum-size [10 :by 20]})]
        (is (= (Dimension. 10 20) (.getMaximumSize p))))))

  (testing "the :size option"
    (testing "set the component's min, max, and preferred size using to-dimension"
      (let [p (apply-options (JPanel.) {:size [11 :by 21]})
            d (Dimension. 11 21)]
        (is (= d (.getPreferredSize p)))
        (is (= d (.getMinimumSize p)))
        (is (= d (.getMaximumSize p))))))

  (testing "the :location option"
    (testing "sets the component's location with a two-element vector"
      (let [p (apply-options (JPanel.) {:location [23 45]})
            l (.getLocation p)]
        (is (= [23 45] [(.x l) (.y l)]))))
    (testing "sets the component's location with a two-element vector, where :* means keep the old value "
      (let [p (apply-options (JPanel.) {:location [23 :*]})
            l (.getLocation p)]
        (is (= [23 0] [(.x l) (.y l)]))))
    (testing "sets the component's location with a java.awt.Point"
      (let [p (apply-options (JPanel.) {:location (java.awt.Point. 23 45)})
            l (.getLocation p)]
        (is (= [23 45] [(.x l) (.y l)]))))
    (testing "sets the component's location with a java.awt.Rectangle"
      (let [p (apply-options (JPanel.) {:location (java.awt.Rectangle. 23 45 99 100)})
            l (.getLocation p)]
        (is (= [23 45] [(.x l) (.y l)])))))
  (testing "the :bounds option"
    (testing "sets the component's bounds with a [x y width height] vector"
      (let [p (apply-options (JPanel.) {:bounds [23 45 67 89]})
            b (.getBounds p)]
        (is (= [23 45 67 89] [(.x b) (.y b) (.width b) (.height b)]))))
    (testing "sets the component's bounds with a [x y width height] vector, where :* means keep the old value"
      (let [p (label :bounds [23 45 67 89])
            p (config! p :bounds [24 :* :* 90])
            b (.getBounds p)]
        (is (= [24 45 67 90] [(.x b) (.y b) (.width b) (.height b)]))))
    (testing "sets the component's bounds to its preferred size if given :preferred, preserving x and y"
      (let [p (label :bounds [23 45 67 89])
            ps (.getPreferredSize p)
            p (config! p :bounds :preferred)
            b (.getBounds p)]
        (is (= [23 45 (.width ps) (.height ps)] [(.x b) (.y b) (.width b) (.height b)]))))
    (testing "sets the component's bounds with a java.awt.Dimension, preserving x and y"
      (let [p (label :bounds [23 45 67 89])
            p (config! p :bounds (java.awt.Dimension. 80 90))
            b (.getBounds p)]
        (is (= [23 45 80 90] [(.x b) (.y b) (.width b) (.height b)]))))
    (testing "sets the component's bounds with a java.awt.Rectangle"
      (let [p (apply-options (JPanel.) {:bounds (java.awt.Rectangle. 23 45 67 89)})
            b (.getBounds p)]
        (is (= [23 45 67 89] [(.x b) (.y b) (.width b) (.height b)])))))

  (testing "the :cursor option"
    (testing "sets the widget's cursor when given a cursor"
      (let [c (cursor/cursor :hand)
            p (apply-options (JPanel.) {:cursor c})]
        (is (= c (.getCursor p)))))
    (testing "sets the widget's cursor when given a cursor type keyword"
      (let [p (apply-options (JPanel.) {:cursor :hand})]
        (is (= java.awt.Cursor/HAND_CURSOR (.getType (.getCursor p)))))))

  (testing "setting enabled option"
    (testing "does nothing when omitted"
      (let [c (apply-options (JPanel.) {})]
        (is (.isEnabled c))))
    (testing "sets enabled when provided"
      (let [c (apply-options (JPanel.) {:enabled? false})]
        (is (not (.isEnabled c)))))
    (testing "sets enabled when provided a truthy value"
      (let [c (apply-options (JPanel.) {:enabled? "something"})]
        (is (.isEnabled c))))
    (testing "sets enabled when provided a falsey value"
      (let [c (apply-options (JPanel.) {:enabled? nil})]
        (is (= false (.isEnabled c))))))
  (testing "setting visible? option"
    (testing "does nothing when omitted"
      (let [c (apply-options (JPanel.) {})]
        (is (.isVisible c))))
   (testing "sets visible when provided"
      (let [c (apply-options (JPanel.) {:visible? false})]
        (is (not (.isVisible c)))))
    (testing "sets visible when provided a truthy value"
      (let [c (apply-options (JPanel.) {:visible? "something"})]
        (is (.isVisible c))))
    (testing "sets not visible when provided a falsey value"
      (let [c (apply-options (JPanel.) {:visible? nil})]
        (is (= false (.isVisible c))))))

  (testing "setting opaque? option"
    (testing "does nothing when omitted"
      (let [c (apply-options (JPanel.) {})]
        (is (.isOpaque c))))
    (testing "sets opacity when provided"
      (let [c (apply-options (JPanel.) {:opaque? false})]
        (is (not (.isOpaque c))))))
  (testing "the :model property"
    (testing "sets the model when provided"
      (let [model  (javax.swing.DefaultButtonModel.)
            widget (button :model model)]
        (is (= model (.getModel widget))))))
  (testing "sets background using to-color when provided"
    (let [c (apply-options (JPanel.) {:background "#000000" })]
      (is (= Color/BLACK (.getBackground c)))))
  (testing "sets opaque when background provided"
    (let [c (apply-options (JLabel.) {:background "#000000" })]
      (is (= true (.isOpaque c)))))
  (testing "sets foreground when provided"
    (let [c (apply-options (JPanel.) {:foreground "#00FF00" })]
      (is (= Color/GREEN (.getForeground c)))))
  (testing "sets border when provided using to-border"
    (let [c (apply-options (JPanel.) {:border "TEST"})]
      (is (= "TEST" (.. c getBorder getTitle)))))
  (testing "sets cursor when provided"
    (let [c (apply-options (JPanel.) {:cursor :hand})]
      (is (= java.awt.Cursor/HAND_CURSOR (.getType (.getCursor c)))))))

(deftest show!-test
  (testing "makes a widget visible and returns it"
    (.isVisible (show! (doto (JPanel.) (.setVisible false))))))

(deftest hide!-test
  (testing "hides a widget and returns it"
    (not (.isVisible (hide! (doto (JPanel.) (.setVisible true)))))))

(deftest make-widget-test
  (testing "throws an exception for unsupported arguments"
    (try (make-widget 99) false (catch Exception e true)))
  (testing "returns nil if input is nil"
    (= nil (make-widget nil)))
  (testing "returns input if it's already a widget"
    (let [c (JPanel.)]
      (is (= c (make-widget c)))))
  (testing "returns input if it's a JFrame"
    (let [c (JFrame.)]
      (is (= c (make-widget c)))))
  (testing "returns a label for string input"
    (let [c (make-widget "TEST")]
      (is (= "TEST" (.getText c)))))
  (testing "returns a button if input is an Action"
    (let [a (action :handler #(println "HI") :name "Test")
          c (make-widget a)]
      (is (instance? javax.swing.JButton c))
      (is (= "Test" (.getText c)))))
  (testing "creates a separator for :separator"
    (instance? javax.swing.JSeparator (make-widget :separator)))
  (testing "creates horizontal glue for :fill-h"
    (let [c (make-widget :fill-h)]
      (is (instance? javax.swing.Box$Filler c))
      (is (= 32767.0 (.. c getMaximumSize getWidth)))))
  (testing "creates vertical glue for :fill-v"
    (let [c (make-widget :fill-v)]
      (is (instance? javax.swing.Box$Filler c))
      (is (= 32767.0 (.. c getMaximumSize getHeight)))))
  (testing "creates a vertical strut for [:fill-v N]"
    (let [c (make-widget [:fill-v 99])]
      (is (instance? javax.swing.Box$Filler c))
      (is (= 32767.0 (.. c getMaximumSize getWidth)))
      (is (= 99.0 (.. c getMaximumSize getHeight)))
      (is (= 99.0 (.. c getPreferredSize getHeight)))))
  (testing "creates a horizontal strut for [:fill-h N]"
    (let [c (make-widget [:fill-h 88])]
      (is (instance? javax.swing.Box$Filler c))
      (is (= 32767.0 (.. c getMaximumSize getHeight)))
      (is (= 88.0 (.. c getMaximumSize getWidth)))
      (is (= 88.0 (.. c getPreferredSize getWidth)))))
  (testing "creates a rigid area for a Dimension"
    (let [c (make-widget (Dimension. 12 34))]
      (is (instance? javax.swing.Box$Filler c))
      (is (= 12.0 (.. c getMaximumSize getWidth)))
      (is (= 34.0 (.. c getMaximumSize getHeight)))
      (is (= 12.0 (.. c getPreferredSize getWidth)))
      (is (= 34.0 (.. c getPreferredSize getHeight)))))
  (testing "creates a rigid area for a [N :by N]"
    (let [c (make-widget [12 :by 34])]
      (is (instance? javax.swing.Box$Filler c))
      (is (= 12.0 (.. c getMaximumSize getWidth)))
      (is (= 34.0 (.. c getMaximumSize getHeight)))
      (is (= 12.0 (.. c getPreferredSize getWidth)))
      (is (= 34.0 (.. c getPreferredSize getHeight))))))

(deftest to-widget-test
  (testing "returns nil for unknown inputs"
      (= nil (to-widget "a string")))
  (testing "returns nil if input is nil"
      (= nil (to-widget nil)))
  (testing "returns input if it's already a widget"
    (let [c (JPanel.)]
      (is (= c (to-widget c)))))
  (testing "returns input if it's a JFrame"
    (let [c (JFrame.)]
      (is (= c (to-widget c)))))
  (testing "converts an event to its source"
    (let [b (button)
          e (ActionEvent. b 0 "hi")]
      (is (= b (to-widget e))))))

(deftest to-document-test
  (testing "returns nil if input is nil"
    (nil? (to-document nil)))
  (testing "returns input if it's already a document"
    (let [d (javax.swing.text.PlainDocument.)]
      (is (= d (to-document d)))))
  (testing "returns the document of text component"
    (let [t (text)]
      (is (= (.getDocument t) (to-document t))))))

(defmacro verify-config [target key getter]
  (let [t (gensym "t")]
    `(testing ~(str "can retrieve the value of " key " from a widget")
     (let [~t ~target
           expected# ~(if (symbol? getter) `(. ~t ~getter) getter)
           actual# (config ~t ~key)]
       (is (= expected# actual#))))))

(deftest config-test
  (testing "throws IllegalArgumentException for an unknown option"
    (try
      (config (text "HI") :textish)
      false
      (catch IllegalArgumentException e true)))
  (testing "can retrieve the :id of a widget"
    (= :foo (config (text :id :foo) :id)))
  (testing "can retrieve the :class of a widget"
    (= #{"foo"} (config (text :class :foo) :class)))

  (verify-config (text :drag-enabled? true) :drag-enabled? true)
  (verify-config (tree :drag-enabled? true) :drag-enabled? true)
  (verify-config (listbox :drag-enabled? true) :drag-enabled? true)
  (verify-config (table :drag-enabled? true) :drag-enabled? true)
  (verify-config (table :model [:columns [:a :b :c] :rows [[1 2 3]]] :column-widths [20 30 40]) :column-widths [20 30 40])

  (verify-config (text :drop-mode :insert) :drop-mode :insert)
  (verify-config (tree :drop-mode :on-or-insert) :drop-mode :on-or-insert)
  (verify-config (listbox :drop-mode :on-or-insert) :drop-mode :on-or-insert)
  (verify-config (table :drop-mode :on-or-insert) :drop-mode :on-or-insert)

  (verify-config (text :text "HI") :text "HI")
  (verify-config (button :text "button") :text "button")
  (verify-config (label :text "label") :text "label")
  (verify-config (text :opaque? false) :opaque? false)
  (verify-config (text :opaque? true) :opaque? true)
  (verify-config (text) :enabled? isEnabled)
  (verify-config (text :size [100 :by 101]) :size getSize)
  (verify-config (text :preferred-size [100 :by 101]) :preferred-size getPreferredSize)
  (verify-config (text :minimum-size [100 :by 101]) :minimum-size getMinimumSize)
  (verify-config (text :maximum-size [100 :by 101]) :maximum-size getMaximumSize)
  (verify-config (text) :foreground getForeground)
  (verify-config (text) :background getBackground)
  (verify-config (text :focusable? true) :focusable? true)
  (verify-config (text :focusable? false) :focusable? false)
  (verify-config (text :visible? true) :visible? true)
  (verify-config (text :visible? false) :visible? false)
  (verify-config (border-panel :border 1) :border getBorder)
  (verify-config (border-panel :location [100 200]) :location (java.awt.Point. 100 200))
  (verify-config (border-panel :bounds [100 200 300 400]) :bounds (java.awt.Rectangle. 100 200 300 400))
  (verify-config (border-panel :font :monospace) :border getBorder)
  (verify-config (border-panel :tip "A tool tip") :tip "A tool tip")
  (verify-config (border-panel :cursor :hand) :cursor getCursor)
  (verify-config (button) :model getModel)
  (verify-config (text) :model getDocument)
  (verify-config (combobox) :model getModel)
  (verify-config (listbox) :model getModel)
  (verify-config (table) :model getModel)
  (verify-config (tree) :model getModel)
  (verify-config (progress-bar) :model getModel)
  (verify-config (slider) :model getModel)
          )

(deftest config!-test
  (testing "configures the properties given to it on a single target"
    (let [p (JPanel.)
          result (config! p :foreground Color/RED :background Color/BLUE :enabled? false)]
      (is (= p result))
      (is (= Color/RED (.getForeground p)))
      (is (= Color/BLUE (.getBackground p)))
      (is (not (.isEnabled p)))))
  (testing "configures the properties given to it on a multiple targets"
    (let [targets [(JPanel.) (JPanel.)]
          result (config! targets :foreground Color/RED :background Color/BLUE :enabled? false)]
      (is (= targets result))
      (is (= Color/RED (.getForeground (first targets))))
      (is (= Color/BLUE (.getBackground (first targets))))
      (is (not (.isEnabled (first targets))))
      (is (= Color/RED (.getForeground (second targets))))
      (is (= Color/BLUE (.getBackground (second targets))))
      (is (not (.isEnabled (second targets))))))
  (testing "configures a target with type-specific properties"
    (let [t (toggle :text "hi" :selected? false)]
      (is (.isSelected (config! t :selected? true)))))
  (testing "can configure a frame"
    (let [f (frame)]
      (config! f :title "I set the title")
      (is (= "I set the title" (.getTitle f)))))
  (testing "can configure a dialog"
    (let [f (dialog)]
      (config! f :title "I set the title")
      (is (= "I set the title" (.getTitle f)))))
  (testing "can configure an action"
    (let [a (action :name "foo")]
      (config! a :name "bar")
      (is (= "bar" (.getValue a Action/NAME))))))

(deftest flow-panel-test
  (testing "should create a FlowLayout of :items list"
    (let [[a b c] [(JPanel.) (JPanel.) (JPanel.)]
          p (flow-panel :items [a b c] :align :trailing :hgap 99 :vgap 12 :align-on-baseline? true)
          l (.getLayout p)]
      (is (= java.awt.FlowLayout (class l)))
      (is (= FlowLayout/TRAILING (.getAlignment l)))
      (is (= 99 (.getHgap l)))
      (is (= 12 (.getVgap l)))
      (is (.getAlignOnBaseline l))
      (is (= [a b c] (seq (.getComponents p))))))
  (testing "should returns :items with config"
    (let [items [(JPanel.) (JPanel.) (JPanel.)]
          p     (flow-panel :items items)]
      (is (= items (config p :items)))))
  (testing "should throw IllegalArgumentException if a nil item is given"
    (try (flow-panel :items [nil]) false (catch IllegalArgumentException e true))))

(deftest xyz-panel-test
  (testing "should create a JPanel"
    (instance? JPanel (xyz-panel)))
  (testing "should create a JPanel with a nil layout"
    (nil? (.getLayout (xyz-panel))))
  (testing "should add items"
    (let [[a b c :as items] [(label :text "a") (label :text "b") (button :text "c")]
          p (xyz-panel :items items)]
      (is (= items (vec (.getComponents p)))))))

(deftest border-panel-test
  (testing "should create a BorderLayout with given h and v gaps"
    (let [p (border-panel :hgap 99 :vgap 12)
          l (.getLayout p)]
      (is (= java.awt.BorderLayout (class l)))
      (is (= 99 (.getHgap l)))
      (is (= 12 (.getVgap l)))))
  (testing "should create a BorderLayout using direction options"
    (let [[n s e w c] [(JPanel.) (JPanel.) (JPanel.)(JPanel.)(JPanel.)]
          p (border-panel :hgap 99 :vgap 12 :north n :south s :east e :west w :center c)
          l (.getLayout p)]
      (is (= java.awt.BorderLayout (class l)))
      (is (= 99 (.getHgap l)))
      (is (= 12 (.getVgap l)))
      (is (= #{n s e w c} (apply hash-set (.getComponents p))))))
  (testing "should create a BorderLayout using list of items with direction constraints"
    (let [[n s e w c] [(JPanel.) (JPanel.) (JPanel.)(JPanel.)(JPanel.)]
          p (border-panel :hgap 99 :vgap 12 :items [[n :north] [s :south][e :east][w :west][c :center]])
          l (.getLayout p)]
      (is (= java.awt.BorderLayout (class l)))
      (is (= #{n s e w c} (apply hash-set (.getComponents p))))))
  (testing "should return its :items with config"
    (let [[n s e w c] [(JPanel.) (JPanel.) (JPanel.)(JPanel.)(JPanel.)]
           items [[n :north] [s :south][e :east][w :west][c :center]]
          p (border-panel :hgap 99 :vgap 12 :items items)]
      (is (= items (config p :items))))))

(deftest horizontal-panel-test
  (testing "should create a horizontal box of :items list"
    (let [[a b c] [(JPanel.) (JPanel.) (JPanel.)]
          p (horizontal-panel :items [a b c])]
      (is (= BoxLayout/X_AXIS (.. p getLayout getAxis)))
      (is (= [a b c] (seq (.getComponents p))))))
  (testing "should get :items with config"
    (let [items [(JPanel.) (JPanel.) (JPanel.)]
          p (horizontal-panel :items items)]
      (is (= items (config p :items))))))

(deftest vertical-panel-test
  (testing "should create a vertical box of :items list"
    (let [[a b c] [(JPanel.) (JPanel.) (JPanel.)]
          p (vertical-panel :items [a b c])]
      (is (= BoxLayout/Y_AXIS (.. p getLayout getAxis)))
      (is (= [a b c] (seq (.getComponents p))))))
  (testing "should get :items with config"
    (let [items [(JPanel.) (JPanel.) (JPanel.)]
          p (vertical-panel :items items)]
      (is (= items (config p :items))))))

(deftest grid-panel-test
  (testing "should default to 1 column"
    (let [g (grid-panel)
          l (.getLayout g)]
      (is (= 0 (.getRows l)))
      (is (= 1 (.getColumns l)))))
  (testing "should set number of rows"
    (let [g (grid-panel :rows 12)
          l (.getLayout g)]
      (is (= 12 (.getRows l)))
      (is (= 0 (.getColumns l)))))
  (testing "should set the hgap and vgap"
    (let [g (grid-panel :hgap 2 :vgap 3)
          l (.getLayout g)]
      (is (= 2 (.getHgap l)))
      (is (= 3 (.getVgap l)))))
  (testing "should add the given items to the panel"
    (let [[a b c] [(label :text "A") (label :text "B") (label :text "C")]
          g (grid-panel :items [a b c])]
      (is (= [a b c] (seq (.getComponents g))))))
  (testing "should get :items with config"
    (let [items [(label :text "A") (label :text "B") (label :text "C")]
          g (grid-panel :items items)]
      (is (= items (config g :items))))))

(deftest form-panel-test
  (testing "should create a JPanel with a GridBagLayout"
    (= java.awt.GridBagLayout (class (.getLayout (form-panel)))))
  (testing "should add an item with grid bag constraints"
    (let [p (form-panel :items [["hi" :weighty 999 :gridwidth 1]])
          w (first (.getComponents p))
          gbcs (.getConstraints (.getLayout p) w)]
      (is (instance? JLabel w))
      (is (= java.awt.GridBagConstraints (class gbcs)))
      #_(is (= 999.0 (.weighty gbcs))))))

(deftest font-test
  (testing "should support the :font property"
    (let [f (font "ARIAL-BOLD-18")
          l (label :font f)]
      (is (= f (.getFont l))))))

(deftest label-test
  (testing "should create an empty label"
    (let [l (label)]
      (is (instance? JLabel l))
      (is (= "" (.getText l)))))
  (testing "should create a label with tooltip"
    (is (= "HI" (.getToolTipText (label :tip "HI")))))
  (testing "should create a label with text when given a single argument"
    (is (= "test label" (.getText (label "test label")))))
  (testing "should create a label with text"
    (is (= "test label" (.getText (label :text "test label")))))
  (testing "should create a label with horizontal alignment"
    (= SwingConstants/LEFT (.getHorizontalAlignment (label :halign :left))))
  (testing "should create a label with horizontal text position"
    (= SwingConstants/LEFT (.getHorizontalTextPosition (label :h-text-position :left))))
  (testing "should create a label with vertical text position"
    (= SwingConstants/BOTTOM (.getVerticalTextPosition (label :v-text-position :bottom))))
  (testing "should create a label with vertical alignment"
    (= SwingConstants/BOTTOM (.getVerticalAlignment (label :valign :bottom)))))

(deftest text!-test
  (testing "should throw IllegalArgumentException if it doesn't know what to do"
    (try
      (do (text! (border-panel) "no") false)
      (catch IllegalArgumentException e true)))
  (testing "should set the text of the document in a document event"
    (let [doc (javax.swing.text.PlainDocument.)
          evt (javax.swing.text.AbstractDocument$DefaultDocumentEvent. doc 0 0
                                                 javax.swing.event.DocumentEvent$EventType/CHANGE)]
      (text! evt "Hello")
      (is (= "Hello" (text evt)))))
  (testing "should set the text of a text Document"
    (let [d (javax.swing.text.PlainDocument.)
          _ (.insertString d 0 "HI" nil)
          r (text! d "BYE!")]
      (is (= d r))
      (is (= "BYE!" (text d)))))
  (testing "With nil input it should set the text of a text widget to \"\""
    (= "" (text (text! (text "HI") nil))))
  (testing "should set the text of a single text widget argument"
    (= "BYE" (text (text! (text "HI") "BYE"))))
  (testing "should set the text of a single button argument"
    (= "BYE" (text (text! (button :text "HI") "BYE"))))
  (testing "should set the text of a seq of widget arguments"
    (let [[a b] [(text "HI") (text "BYE")]
          result (text! [a b] "YUM")]
      (is (= [a b] result))
      (is (= "YUM" (text a)))
      (is (= "YUM" (text b)))))
  (testing "should set the text of a widget to an integer"
    (= "99" (text (text! (text "initial") 99))))
  (testing "should set the text of a widget to a double"
    (= (str 3.14) (text (text! (text "initial") 3.14))))
  (testing "should set the text of a widget to a rational"
    (= (str 1/3) (text (text! (text "initial") 1/3))))
  (testing "should set the text of a widget to the contents of a non-string 'slurpable'"
    (let [t (text :multi-line? true)]
      (text! t (clojure.java.io/resource "seesaw/test/core.text.txt"))
      ; Be careful editing the test file with vim. It will silently add
      ; a trailing newline on save.
      (is (= "Some text in a resource" (text t))))))

(deftest text-test
  (testing "should throw IllegalArgumentException if argument is nil"
    (try
      (do (text nil) false)
      (catch IllegalArgumentException e true)))
  (testing "should throw IllegalArgumentException if it doesn't know what to do"
    (try
      (do (text (border-panel)) false)
      (catch IllegalArgumentException e true)))
  (testing "should return the text of a single text widget argument"
    (= "HI" (text (text "HI"))))
  (testing "should return the text of the selection in a combobox"
    (let [cb (combobox :model ["a" "b"])]
      (selection! cb "a")
      (is (= "a" (text cb)))))
  (testing "should return the text of a text Document argument"
    (let [d (javax.swing.text.PlainDocument.)]
      (.insertString d 0 "HI" nil)
      (is (= "HI" (text d)))))
  (testing "should return the text of the document in a document event"
    (let [doc (javax.swing.text.PlainDocument.)
          evt (javax.swing.text.AbstractDocument$DefaultDocumentEvent. doc 0 0
                                                 javax.swing.event.DocumentEvent$EventType/CHANGE)]
      (.insertString doc 0 "Hello" nil)
      (is (= "Hello" (text evt)))))
  (testing "should return the text of a button argument"
    (= "HI" (text (button :text "HI"))))
  (testing "should return the text of a label argument"
    (= "HI" (text (label "HI"))))
  (testing "should return the text of a seq of widget arguments"
    (= ["HI" "BYE"] (text [(text "HI") (button :text "BYE")])))
  (testing "should create a text field given a string argument"
    (let [t (text "HI")]
      (is (instance? JTextField t))
      (is (= "HI" (.getText t)))))
  (testing "should create a text field by default"
    (let [t (text :text "HI")]
      (is (instance? JTextField t))
      (is (= "HI" (.getText t)))))
  (testing "should create a text field with the given :columns"
    (let [t (text :text "HI" :columns 55)]
      (is (= 55 (.getColumns t)))))
  (testing "should create a text area when multi-line? is true"
    (let [t (text :text "HI" :multi-line? true)]
      (is (instance? JTextArea t))
      (is (= "HI" (.getText t)))))
  (testing "should create a text area with the given :columns"
    (let [t (text :text "HI" :multi-line? true :columns 91)]
      (is (= 91 (.getColumns t)))))
  (testing "should default line wrapping to false"
    (not (.getLineWrap (text :multi-line? true))))
  (testing "should enable line wrapping on words when :wrap-lines? is true"
    (let [t (text :multi-line? true :wrap-lines? true)]
      (is (.getLineWrap t))
      (is (.getWrapStyleWord t))))
  (verify-config (text :multi-line? true :wrap-lines? true) :wrap-lines? true)
  (testing "should set tab size with :tab-size"
    (= 22 (.getTabSize (text :multi-line? true :tab-size 22))))
  (testing "should set number of rows with :rows"
    (= 123 (.getRows (text :multi-line? true :rows 123))))
  (verify-config (-> (text :text "hello there")
                     (config! :caret-position 5))
                 :caret-position 5)
  (testing "should set the :caret-color"
    (= Color/ORANGE (.getCaretColor (text :caret-color Color/ORANGE))))
  (testing "should set the :disabled-text-color"
    (= Color/ORANGE (.getDisabledTextColor (text :disabled-text-color Color/ORANGE))))
  (testing "should set the :selected-text-color"
    (= Color/ORANGE (.getSelectedTextColor (text :selected-text-color Color/ORANGE))))
  (testing "should set the :selection-color"
    (= Color/ORANGE (.getSelectionColor (text :selection-color Color/ORANGE))))
  (testing "should handle the :margin option with to-insets"
    (let [t (text :margin 1)
          i   (.getMargin t)]
      (is (= [1 1 1 1] [(.top i) (.left i) (.bottom i) (.right i)]))))
  (testing "should honor the editable property"
    (let [t (text :text "HI" :editable? false :multi-line? true)]
      (is (not (.isEditable t))))))

(deftest styled-text-test
  (testing "should create a text pane"
    (let [t (styled-text :text "HI")]
      (is (instance? JTextPane t))
      (is (= "HI" (text t))))
    (verify-config (styled-text :wrap-lines? true) :wrap-lines? true)
    (verify-config (styled-text :wrap-lines? false) :wrap-lines? false))
  (testing "should add styles"
    (let [t (styled-text :text "HI"
                    :styles [[:big :size 30]
                            [:small :size 3]])
          style (.getStyle t "big")]
      (is (instance? javax.swing.text.Style style))
      (is (.containsAttribute style StyleConstants/FontSize (Integer. 30)))))
  (testing "should set the FontFamily attr as a string from a keyword"
    (let [t (styled-text :styles [[:fonty :font :Arial]])
          s (.getStyle t "fonty")
          v (.getAttribute s StyleConstants/FontFamily)]
      (is (string? v))
      (is (= "Arial" v))))
  (testing "should set the FontFamily attr as a string from a string"
    (let [t (styled-text :styles [[:fonty :font "Arial"]])
          s (.getStyle t "fonty")
          v (.getAttribute s StyleConstants/FontFamily)]
      (is (string? v))
      (is (= "Arial" v))))
  (testing "should set the FontSize attr as an Integer"
    (let [t (styled-text :styles [[:big :size 30]])
          s (.getStyle t "big")
          v (.getAttribute s StyleConstants/FontSize)]
      (is (instance? Integer v))
      (is (= 30 v))))
  (testing "should override getScrollableTracksViewportWidth with :wrap-lines?"
    (not (.getScrollableTracksViewportWidth (styled-text))))
  (testing "should override getScrollableTracksViewportWidth with :wrap-lines?"
    (.getScrollableTracksViewportWidth (styled-text :wrap-lines? true))))

(deftest style-text!-test
  (let [t (styled-text :text "HI"
                       :styles [[:big :size 30]
                                [:small :size 3]])]
    (testing "should style the text"
        (is (= t (style-text! t :big 0 2)))
        (is (.containsAttribute (.getStyle t "big")
                                StyleConstants/FontSize (int 30))))))

(deftest password-test
  (testing "should create a JPasswordField"
    (instance? javax.swing.JPasswordField (password)))
  (testing "should set the initial text"
    (= "secret" (text (password :text "secret"))))
  (testing "should set the columns"
    (= 30 (.getColumns (password :columns 30))))
  (testing "should set the echo char with a char"
    (= \S (.getEchoChar (password :echo-char \S)))))

(deftest with-password*-test
  (testing "should call the handler with the password in a character array"
    (let [s (atom nil)
          p (password :text "secret")]
      (with-password* p (fn [chars] (reset! s (apply str chars))))
      (is (= "secret" @s))))

  (testing "should return the return value of the handler"
    (= "HEY!" (with-password* (password) (fn [chars] "HEY!"))))

  (testing "should fill the password character array with zeroes after then handler has completed"
    (let [s (atom nil)
          p (password :text "secret")]
      (with-password* p (fn [chars] (reset! s chars)))
      (is (= [\0 \0 \0 \0 \0 \0] (vec @s))))))

(deftest editor-pane-test
  (testing "should create a JEditorPane"
    (instance? javax.swing.JEditorPane (editor-pane))))

(deftest button-group-test
  (testing "should create a ButtonGroup"
    (instance? javax.swing.ButtonGroup (button-group)))
  (testing "should create a button group with a list of buttons"
    (let [[a b c] [(radio) (checkbox) (toggle)]
          bg (button-group :buttons [a b c])]
      (is (= [a b c] (enumeration-seq (.getElements bg))))))
  (testing "should return buttons in the group with config :buttons"
    (let [buttons [(radio) (checkbox) (toggle)]
          bg (button-group :buttons buttons)]
      (is (= buttons (config bg :buttons)))
      ;(is (= [a b c] (enumeration-seq (.getElements bg))))
      )))

(deftest button-test
  (testing "should create a JButton"
    (let [b (button :text "HI")]
      (is (instance? JButton b))
      (is (= "HI" (.getText b)))))
  (testing "should handle the :margin option with to-insets"
    (let [b (button :margin 1)
          i   (.getMargin b)]
      (is (= [1 1 1 1] [(.top i) (.left i) (.bottom i) (.right i)]))))


  (testing "should add the button to a button group specified with the :group option"
    (let [bg (button-group)
          b  (button :group bg)]
      (is (= b (first (enumeration-seq (.getElements bg)))))))

  (testing "should create a button from an action"
    (let [a (action :handler println)
          b (button :action a)]
      (is (instance? JButton b))
      (is (= a (.getAction b)))))

  (testing "should set the :mnemonic of the button given an integer keycode"
    (= 100 (.getMnemonic (button :mnemonic 100))))
  (testing "should set the :mnemonic of the button given a char"
    (= (int \Y) (.getMnemonic (button :mnemonic \Y))))
  (testing "should set the :mnemonic of the button given a lower-case char"
    (= (int \Z) (.getMnemonic (button :mnemonic \z)))))

(deftest toggle-test
  (testing "should create a JToggleButton"
    (let [t (toggle :text "HI")]
      (is (instance? JToggleButton t))
      (is (= "HI" (.getText t)))
      (is (not (.isSelected t)))))
  (testing "should honor the :selected property"
    (let [t (toggle :text "HI" :selected? true)]
      (is (.isSelected t)))))

(deftest checkbox-test
  (testing "should create a JCheckBox"
    (let [t (checkbox :text "HI")]
      (is (instance? JCheckBox t))
      (is (= "HI" (.getText t)))
      (is (not (.isSelected t)))))
  (testing "should honor the :selected property"
    (let [t (checkbox :text "HI" :selected? true)]
      (is (.isSelected t)))))

(deftest radio-test
  (testing "should create a JRadioButton"
    (let [t (radio :text "HI")]
      (is (instance? JRadioButton t))
      (is (= "HI" (.getText t)))
      (is (not (.isSelected t)))))
  (testing "should honor the :selected property"
    (let [t (radio :text "HI" :selected? true)]
      (.isSelected t))))

(deftest listbox-test
  (verify-config (listbox :selection-mode :single) :selection-mode :single)
  (verify-config (listbox :layout-orientation :vertical) :layout-orientation :vertical)
  (verify-config (listbox :layout-orientation :horizontal-wrap) :layout-orientation :horizontal-wrap)
  (verify-config (listbox :layout-orientation :vertical-wrap) :layout-orientation :vertical-wrap)
  (testing "should create a JList"
    (instance? javax.swing.JList (listbox)))
  (testing "should create a JList with :fixed-cell-height set"
    (= 98 (.getFixedCellHeight (listbox :fixed-cell-height 98))))
  (testing "should create a JList and set the selection mode"
    (is (= javax.swing.ListSelectionModel/SINGLE_SELECTION (.getSelectionMode (listbox :selection-mode :single)))))
  (testing "should create a JList using a seq as its model"
    (let [lb (listbox :model [1 2 3 4])
          model (.getModel lb)]
      (= [1 2 3 4] (map #(.getElementAt model %1) (range (.getSize model))))))
  (testing "should set the list's cell renderer, if given"
    (let [render-fn (fn [renderer info] nil)
          renderer (default-list-cell-renderer render-fn)
          lb (listbox :renderer renderer)]
      (is (= renderer (.getCellRenderer lb)))
      (is (= renderer (config lb :renderer))))))

(deftest combobox-test
  (testing "should create a JComboBox"
    (let [lb (combobox)]
      (instance? javax.swing.JComboBox lb)))
  (testing "the :editable? property"
    (testing "should create a non-editable JComboBox when false"
      (not (.isEditable (combobox :editable? false))))
    (testing "should create an editable JComboBox when true"
      (.isEditable (combobox :editable? true))))
  (testing "should set the combobox's cell renderer, if given"
      (let [render-fn (fn [renderer info] nil)
            renderer (default-list-cell-renderer render-fn)
            lb (combobox :renderer renderer)]
        (is (= renderer (.getRenderer lb)))
        (is (= renderer (config lb :renderer)))))
  (testing "should create a JComboBox using a seq as its model"
    (let [lb (combobox :model [1 2 3 4])
          model (.getModel lb)]
      (is (= [1 2 3 4] (map #(.getElementAt model %1) (range (.getSize model)))))
      (is (= 1 (.getSelectedItem model))))))

(deftest spinner-model-test
  (testing "should create a number spinner model"
    (let [m (spinner-model 3.5 :from 1.5 :to 4.5 :by 0.5)]
      (is (instance? javax.swing.SpinnerNumberModel m))
      (is (= 3.5 (.getValue m)))
      (is (= 0.5 (.getStepSize m)))
      (is (= 4.5 (.getMaximum m)))
      (is (= 1.5 (.getMinimum m)))))
  (testing "should create a date spinner model"
      (let [s (java.util.Date. (long 0))
            v (java.util.Date. (long (* 10 24 3600)))
            e (java.util.Date. (long (* 20 24 3600)))
            m (spinner-model v :from s :to e :by :day-of-month)]
        (is (instance? javax.swing.SpinnerDateModel m))
        (is (= java.util.Calendar/DAY_OF_MONTH (.getCalendarField m)))
        (is (= v (.getValue m)))
        (is (= s (.getStart m)))
        (is (= e (.getEnd m))))))

(deftest spinner-test
  (testing "should create a JSpinner"
    (instance? javax.swing.JSpinner (spinner)))
  (testing "should set the model with the :model option"
    (let [model (javax.swing.SpinnerListModel.)
          s     (spinner :model model)]
      (is (= model (.getModel s)))))
  (testing "creates a list model from a sequence"
    (let [s (spinner :model [1 2 3])
          m (config s :model)]
      (is (instance? javax.swing.SpinnerListModel m))))
  (testing "creates a date model from a java.util.Date"
    (let [d (java.util.Date.)
          s (spinner :model d)
          m (config s :model)]
      (is (instance? javax.swing.SpinnerDateModel m))
      (is (= d (.getValue m)))))
  (testing "creates a numeric model from a number"
    (let [s (spinner :model 3.3)
          m (config s :model)]
      (is (instance? javax.swing.SpinnerNumberModel m))
      (is (= 3.3 (.getValue m)))))
  (testing "supports the :selection event"
    (let [s (spinner :model [1 2 3])
          called (atom nil)]
      (listen s :selection (fn [e] (reset! called e)))
      (selection! s 2)
      (is (= 2 (selection s)))
      (is @called)))
  )

(deftest table-test
  (testing "should create a JTable"
    (instance? javax.swing.JTable (table)))
  (testing "should create a JTable with :single selection-mode set"
    (= javax.swing.ListSelectionModel/SINGLE_SELECTION (.. (table :selection-mode :single) getSelectionModel getSelectionMode)))
  (testing "should create a JTable with :multi-interval selection-mode set"
    (= javax.swing.ListSelectionModel/MULTIPLE_INTERVAL_SELECTION (.. (table :selection-mode :multi-interval) getSelectionModel getSelectionMode)))
  (testing "should fill viewport height by default"
    (.getFillsViewportHeight (table)))
  (testing "should set the table's model from a TableModel"
    (let [m (javax.swing.table.DefaultTableModel.)
          t (table :model m)]
      (= m (.getModel t))))
  (testing "should set the table's model using seesaw.table/table-model"
    (let [t (table :model [:columns [:a :b] :rows [[23 24] [25 26]]])
          m (.getModel t)]
      (is (= 2 (.getRowCount m)))))
  (verify-config (table :selection-mode :single) :selection-mode :single)
  (verify-config (table :fills-viewport-height? true) :fills-viewport-height? true)
  (verify-config (table :fills-viewport-height? false) :fills-viewport-height? false)
  (verify-config (table :show-grid? true) :show-grid? true)
  (verify-config (table :show-grid? false) :show-grid? false)
  (verify-config (table :show-vertical-lines? true) :show-vertical-lines? true)
  (verify-config (table :show-vertical-lines? false) :show-vertical-lines? false)
  (verify-config (table :show-horizontal-lines? true) :show-horizontal-lines? true)
  (verify-config (table :show-horizontal-lines? false) :show-horizontal-lines? false)
  (testing "should honor :auto-resize :off"
    (= javax.swing.JTable/AUTO_RESIZE_OFF (.getAutoResizeMode (table :auto-resize :off))))
  (testing "should honor :auto-resize :next-column"
    (= javax.swing.JTable/AUTO_RESIZE_NEXT_COLUMN (.getAutoResizeMode (table :auto-resize :next-column))))
  (testing "should honor :auto-resize :subsequent-columns"
    (= javax.swing.JTable/AUTO_RESIZE_SUBSEQUENT_COLUMNS (.getAutoResizeMode (table :auto-resize :subsequent-columns))))
  (testing "should honor :auto-resize :last-column"
    (= javax.swing.JTable/AUTO_RESIZE_LAST_COLUMN (.getAutoResizeMode (table :auto-resize :last-column))))
  (testing "should honor :auto-resize :all-columns"
    (= javax.swing.JTable/AUTO_RESIZE_ALL_COLUMNS (.getAutoResizeMode (table :auto-resize :all-columns)))))

(deftest tree-test
  (testing "should create a JTree"
    (instance? javax.swing.JTree (tree)))
  (verify-config (tree :expands-selected-paths? true) :expands-selected-paths? true)
  (verify-config (tree :expands-selected-paths? false) :expands-selected-paths? false)
  (verify-config (tree :scrolls-on-expand? true) :scrolls-on-expand? true)
  (verify-config (tree :scrolls-on-expand? false) :scrolls-on-expand? false)
  (verify-config (tree :shows-root-handles? true) :shows-root-handles? true)
  (verify-config (tree :shows-root-handles? false) :shows-root-handles? false)
  (verify-config (tree :toggle-click-count 2) :toggle-click-count 2)
  (verify-config (tree :toggle-click-count 1) :toggle-click-count 1)
  (verify-config (tree :visible-row-count 20) :visible-row-count 20)
  (verify-config (tree :selection-mode :single) :selection-mode :single)
  (testing "should create a JTree with :root-visible? true"
    (.isRootVisible (tree :root-visible? true)))
  (testing "should create a JTree with :root-visible? false"
    (not (.isRootVisible (tree :root-visible? false))))
  (testing "should create a JTree with :shows-root-handles? true"
    (.getShowsRootHandles (tree :shows-root-handles? true)))
  (testing "should create a JTree with :shows-root-handles? false"
    (not (.getShowsRootHandles (tree :shows-root-handles? false))))
  (testing "should create a JTree with :single selection-mode set"
    (= javax.swing.tree.TreeSelectionModel/SINGLE_TREE_SELECTION (.. (tree :selection-mode :single) getSelectionModel getSelectionMode)))
  (testing "should create a JTree with :discontiguous selection-mode set"
    (= javax.swing.tree.TreeSelectionModel/DISCONTIGUOUS_TREE_SELECTION (.. (tree :selection-mode :discontiguous) getSelectionModel getSelectionMode)))
  (testing "should create a JTree with :contiguous selection-mode set"
    (= javax.swing.tree.TreeSelectionModel/CONTIGUOUS_TREE_SELECTION (.. (tree :selection-mode :contiguous) getSelectionModel getSelectionMode)))
  (testing "should set the tree's model from a TreeModel"
    (let [m (javax.swing.tree.DefaultTreeModel. nil)
          t (tree :model m)]
      (= m (.getModel t)))))

(deftest scrollable-test
  (testing "should create a JScrollPane"
    (let [l (label :text "Test")
          s (scrollable l)]
      (is (instance? JScrollPane s))
      (is (= l (.. s getViewport getView)))))
  (testing "should create a scroll pane with horizontal policy"
    (is (= ScrollPaneConstants/HORIZONTAL_SCROLLBAR_NEVER (.getHorizontalScrollBarPolicy (scrollable (text) :hscroll :never)))))
  (testing "should create a scroll pane with vertical policy"
    (is (= ScrollPaneConstants/VERTICAL_SCROLLBAR_NEVER (.getVerticalScrollBarPolicy (scrollable (text) :vscroll :never)))))
  (testing "should create a JScrollPane with a :row-header-view"
    (let [hv (label)
          s (scrollable (button) :row-header hv)]
      (is (= hv (.. s getRowHeader getView)))))
  (testing "should create a JScrollPane with a :column-header-view"
    (let [hv (label)
          s (scrollable (button) :column-header hv)]
      (is (= hv (.. s getColumnHeader getView)))))
  (testing "should create a JScrollPane with corners"
    (let [[ll lr ul ur :as corners] [(label) (label) (label) (label)]
          s (scrollable (button) :lower-left ll :lower-right lr :upper-left ul :upper-right ur)]
      (is (= corners [(.getCorner s ScrollPaneConstants/LOWER_LEFT_CORNER)
                          (.getCorner s ScrollPaneConstants/LOWER_RIGHT_CORNER)
                          (.getCorner s ScrollPaneConstants/UPPER_LEFT_CORNER)
                          (.getCorner s ScrollPaneConstants/UPPER_RIGHT_CORNER)]))))
  (testing "should create a JScrollPane with options"
    (let [l (label :text "Test")
          s (scrollable l :id "MY-ID")]
      (is (= (keyword "MY-ID") (id-of s))))))

(deftest splitter-test
  (testing "should create a JSplitPane with with two panes"
    (let [left (label :text "Left")
          right (label :text "Right")
          s (splitter :left-right left right)]
      (is (instance? javax.swing.JSplitPane s))
      (is (= left (.getLeftComponent s)))
      (is (= right (.getRightComponent s)))))
  (verify-config (splitter :top-bottom "top" "bottom" :divider-location 99) :divider-location 99)
  (testing "should set the divider location to an absolute pixel location with an int"
    (let [s (splitter :top-bottom "top" "bottom" :divider-location 99)]
      (is (= 99 (.getDividerLocation s)))))
  (testing "should set the divider location to a percentage location with a double (eventually)"
    (let [s (splitter :top-bottom "top" "bottom" :divider-location 0.5)]
      ; We can't really test this since the expected divider location (in pixels)
      ; is pretty hard to predict and because of the JSplitPane visibility hack
      ; that's required, it won't actually happen until it's displayed in a frame :(
      (is true)))
  (testing "should set the divider location to a percentage location with a rational (eventually)"
    (let [s (splitter :top-bottom "top" "bottom" :divider-location 1/2)]
      ; We can't really test this since the expected divider location (in pixels)
      ; is pretty hard to predict and because of the JSplitPane visibility hack
      ; that's required, it won't actually happen until it's displayed in a frame :(
      (is true)))
  (testing "should set the :divider-side"
    (= 93 (.getDividerSize (splitter :left-right (label) (label) :divider-size 93))))
  (testing "should set the :resize-weight"
    (= 0.75 (.getResizeWeight (splitter :left-right (label) (label) :resize-weight 0.75))))
  (testing "should set :one-touch-expandable?"
    (.isOneTouchExpandable (splitter :left-right (label) (label) :one-touch-expandable? true))))

(deftest menu-item-test
  (testing "should create a JMenuItem"
      (is (instance? javax.swing.JMenuItem (menu-item))))
  (testing "should create a menu item with an accelerator key"
    (let [ks (seesaw.keystroke/keystroke "ctrl S")
          mi (menu-item :key ks)]
      (is (= ks (.getAccelerator mi)))))
  (testing "should create a JMenuItem from an action"
    (let [a (action)
          mi (menu-item :action a)]
      (is (= a (.getAction mi))))))

(deftest menu-test
  (testing "should create a JMenu"
    (is (instance? javax.swing.JMenu (menu))))
  (testing "should create a JMenu with the given items"
    (let [a (action)
          b :separator
          c (menu-item)
          d "Just a string"
          m (menu :items [a b c d])
          [ia ib ic id] (.getMenuComponents m)]
      (is (= a (.getAction ia)))
      (is (instance? javax.swing.JPopupMenu$Separator ib))
      (is (= c ic))
      (is (= "Just a string" (.getText id))))))

(deftest popup-test
  (testing "should create a JPopupMenu"
    (is (instance? javax.swing.JPopupMenu (popup))))
  (testing "should create a JPopupMenu with the given items"
    (let [a (action)
          b :separator
          c (menu-item)
          d "Just a string"
          m (popup :items [a b c d])
          ; Separator isn't included in sub elements :(
          [ia ic id]  (map #(.getComponent %) (.getSubElements m))]
      (is (= a (.getAction ia)))
      (is (= c ic))
      (is (= "Just a string" (.getText id))))))

(deftest menubar-test
  (testing "should create a JMenuBar"
    (instance? javax.swing.JMenuBar (menubar)))
  (testing "should create a JMenuBar with the given items"
    (let [a (menu)
          b (menu)
          mb (menubar :items [a b])]
      (is (= [a b] (vec (.getComponents mb)))))))

(deftest toolbar-test
  (testing "should create a JToolBar with the given items"
    (let [tb (toolbar :items ["a" "b" "c"])
          items (.getComponents tb)]
      (is (instance? javax.swing.JToolBar tb))
      (is (= ["a" "b" "c"] (map #(.getText %) items)))))
  (testing "should set the floatable? property"
    (let [tb (toolbar :floatable? true)]
      (is (.isFloatable tb))))
  (testing "should set the floatable property to false"
    (let [tb (toolbar :floatable? false)]
      (is (not (.isFloatable tb)))))
  (testing "should set the orientation property"
    (let [tb (toolbar :orientation :vertical)]
      (is (= SwingConstants/VERTICAL (.getOrientation tb)))))
  (testing "can create a toolbar separator with the :separator keyword"
    (let [tb (toolbar :items [:separator])]
      (is (instance? javax.swing.JToolBar$Separator (.getComponent tb 0))))))

(deftest separator-test
  (testing "should create a horizontal JSeparator by default"
    (let [s (separator)]
      (is (instance? javax.swing.JSeparator s))
      (is (= SwingConstants/HORIZONTAL (.getOrientation s)))))
  (testing "should create a horizontal JSeparator when :orientation is specified"
    (let [s (separator :orientation :horizontal)]
      (is (instance? javax.swing.JSeparator s))
      (is (= SwingConstants/HORIZONTAL (.getOrientation s)))))
  (testing "should create a vertical JSeparator when :orientation is specified"
    (let [s (separator :orientation :vertical)]
      (is (instance? javax.swing.JSeparator s))
      (is (= SwingConstants/VERTICAL (.getOrientation s))))))

(deftest tabbed-panel-test
  (testing "should create a JTabbedPane with desired tab placement and layout"
    (let [tp (tabbed-panel :placement :bottom :overflow :wrap)]
      (is (instance? JTabbedPane tp))
      (is (= JTabbedPane/BOTTOM (.getTabPlacement tp)))
      (is (= JTabbedPane/WRAP_TAB_LAYOUT (.getTabLayoutPolicy tp)))))
  (testing "should add tabs from the tabs property"
    (let [a (label "A tab")
          b (label "B tab")
          tp (tabbed-panel :tabs [{ :title "A" :content a :tip "tip A" }
                                  { :title "B" :content b :tip "tip B" }])]
      (is (= ["A" "B"]         [(.getTitleAt tp 0) (.getTitleAt tp 1)]))
      (is (= ["tip A" "tip B"] [(.getToolTipTextAt tp 0) (.getToolTipTextAt tp 1)]))
      (is (= [a b]             [(.getComponentAt tp 0) (.getComponentAt tp 1)])))))

(deftest canvas-test
  (testing "should create a subclass of JPanel with no layout manager"
    (let [c (canvas)]
      (is (instance? JPanel c))
      (is (nil? (.getLayout c)))))
  (testing "should call :before and :after functions given to the :paint property"
    (let [called (atom 0)
          before (fn [c g] (swap! called inc))
          after (fn [c g] (swap! called inc))
          c (canvas :paint { :before before :after after })]
      (.paintComponent c (.getGraphics (buffered-image 100 100))) ; fake with buffered image
      (is (= 2 @called))))
  (testing "should call a single function given to the :paint property"
    (let [called (atom 0)
          paint (fn [c g] (swap! called inc))
          c (canvas :paint paint)]
      (.paintComponent c (.getGraphics (buffered-image 100 100))) ; fake with buffered image
      (is (= 1 @called)))))

(deftest window-test
  (testing "should create a jwindow"
    (instance? javax.swing.JWindow (window)))
  (testing "should create a window with an id"
    (= :my-frame (id-of (window :id :my-frame))))
  (testing "should create a window with 0 width and height"
    (= (java.awt.Dimension. 0 0) (.getSize (window))))
  (testing "should create a window and set its title, width, and height"
    (let [f (window :width 99 :height 88)]
      (is (= 99 (.getWidth f)))
      (is (= 88 (.getHeight f)))))
  (testing "should set the windows's size with the :size option"
    (let [f (window :size [123 :by 456])]
      (is (= 123 (.getWidth f)))
      (is (= 456 (.getHeight f)))))
  (testing "should create a JFrame and set its content pane"
    (let [c (label :text "HI")
          f (window :content c)]
      (is (= c (.getContentPane f))))))

(deftest frame-test
  (testing "should create a frame with an id"
    (= :my-frame (id-of (frame :id :my-frame))))
  (testing "should create a frame with 0 width and height"
    (= (java.awt.Dimension. 0 0) (.getSize (frame))))
  (testing "should create a JFrame and set its title, width, and height"
    (let [f (frame :title "Hello" :width 99 :height 88)]
      (is (instance? javax.swing.JFrame f))
      (is (= "Hello" (.getTitle f)))
      (is (= 99 (.getWidth f)))
      (is (= 88 (.getHeight f)))))
  (testing "should set the frame's size with the :size option"
    (let [f (frame :title "Hello" :size [123 :by 456])]
      (is (instance? javax.swing.JFrame f))
      (is (= "Hello" (.getTitle f)))
      (is (= 123 (.getWidth f)))
      (is (= 456 (.getHeight f)))))
  (testing "should set the frame's default close operation"
    (let [f (frame :on-close :dispose)]
      (= javax.swing.JFrame/DISPOSE_ON_CLOSE (.getDefaultCloseOperation f))))
  (testing "should create a JFrame and make is not resizable"
    (let [f (frame :title "Hello" :resizable? false)]
      (is (not (.isResizable f)))))
  (testing "should create an undecorated JFrame"
    (let [f (frame :title "Hello" :undecorated? true)]
      (is (.isUndecorated f))))
  (testing "should create a JFrame and set its menu bar"
    (let [mb (menubar)
          f (frame :menubar mb)]
      (is (= mb (.getJMenuBar f)))))
  (testing "should create a JFrame and set its content pane"
    (let [c (label :text "HI")
          f (frame :content c)]
      (is (= c (.getContentPane f)))))
  (testing "should set the frame's icon from an image"
    (let [i (buffered-image 16 16)
          f (frame :icon i)]
      (is (= i (.getIconImage f)))))
  (testing "should, by default, set location by platform to true"
    (.isLocationByPlatform (frame))))

(deftest to-root-test
  (testing "should convert a widget to its parent applet"
    (let [c (label :text "HI")
          a (javax.swing.JApplet.)]
      (.add a c)
      (is (= a (to-root c)))))

  (testing "should convert a widget to its parent frame"
    (let [c (label :text "HI")
          f (frame :content c)]
      (is (= f (to-root c)))))
  (testing "should return nil for an un-parented widget"
    (let [c (label :text "HI")]
      (is (nil? (to-root c))))))

(deftest to-frame-test
  (testing "should be an alias for to-root"
    (= to-frame to-root)))

(letfn [(test-dlg-blocking
          [dlg & {:keys [future-fn] :or {future-fn #(Thread/sleep 100)}}]
          (let [v (atom nil)]
            (future
              (future-fn)
              (swap! v #(if % % 'dialog-is-blocking))
              (invoke-now (.dispose dlg)))
            (invoke-now
             (let [r (show! dlg)]
               (swap! v #(if % % r))))
            @v))]

  (deftest custom-dialog-test
    (testing "argument passing"
      (testing "should create a dialog with an id"
        (= :my-dialog (id-of (custom-dialog :id :my-dialog))))
      (testing "should create a JDialog and set its title, width, and height"
        (let [f (custom-dialog :title "Hello" :width 99 :height 88)]
          (is (instance? javax.swing.JDialog f))
          (is (= "Hello" (.getTitle f)))))
      (testing "should set the dialog's default close operation"
        (let [f (custom-dialog :on-close :dispose)]
          (= javax.swing.JDialog/DISPOSE_ON_CLOSE (.getDefaultCloseOperation f))))
      (testing "should create a JDialog and make is not resizable"
        (let [f (custom-dialog :title "Hello" :resizable? false)]
          (is (not (.isResizable f)))))
      (testing "should create a JDialog that is modal"
        (let [f (custom-dialog :title "Hello" :modal? true)]
          (is (.isModal f))))
      (testing "should create a JDialog that is not modal"
        (let [f (custom-dialog :title "Hello" :modal? false)]
          (is (not (.isModal f)))))
      (testing "should create a JDialog and set its menu bar"
        (let [mb (menubar)
              f (custom-dialog :menubar mb)]
          (is (= mb (.getJMenuBar f)))))
      (testing "should create a JDialog and set its content pane"
        (let [c (label :text "HI")
              f (custom-dialog :content c)]
          (is (= c (.getContentPane f))))))
    (testing "blocking"
      (testing "should block until dialog is being disposed of"
        (let [dlg (custom-dialog :content "Nothing" :modal? true)]
          (is (= (test-dlg-blocking dlg) 'dialog-is-blocking))))
      (testing "should not block if :modal? is false"
        (let [dlg (custom-dialog :content "Nothing" :modal? false)]
          (is (= (test-dlg-blocking dlg) dlg))))
      (testing "should return value passed to RETURN-FROM-DIALOG"
        (let [dlg (custom-dialog :content "Nothing" :modal? true)]
          (is (= (test-dlg-blocking
                  dlg :future-fn #(do
                                    (Thread/sleep 90)
                                    (return-from-dialog dlg :ok)
                                    (Thread/sleep 50))) :ok))))))


  (deftest dialog-test
    (testing "should block until dialog is being disposed of"
      (let [dlg (dialog :content "Nothing" :modal? true)]
        (is (= (test-dlg-blocking dlg) 'dialog-is-blocking))))
    (testing "should not block"
      (let [dlg (dialog :content "Nothing" :modal? false)]
        (is (= (test-dlg-blocking dlg) dlg))))
    (testing "return-from-dialog"
      (let [ok (make-widget (action :name "Ok" :handler (fn [e] (return-from-dialog e :ok))))
            cancel (make-widget (action :name "Cancel" :handler (fn [e] (return-from-dialog e :cancel))))
            dlg #(dialog :content "Nothing"
                         :options (map make-widget [ok cancel]))]
        (testing "should return value passed to RETURN-FROM-DIALOG from clicking on ok button"
          (is (= (test-dlg-blocking
                  (dlg)
                  :future-fn #(do
                                (Thread/sleep 90)
                                (invoke-now (.doClick ok))
                                (Thread/sleep 50))) :ok)))
        (testing "should return value passed to RETURN-FROM-DIALOG from clicking on cancel button"
          (is (= :cancel
                 (test-dlg-blocking
                  (dlg)
                  :future-fn #(do
                                (Thread/sleep 90)
                                (invoke-now (.doClick cancel))
                                (Thread/sleep 50))))))))))


(deftest slider-test
  (testing "should create a slider with a min, max, and value"
    (let [s (slider :min 40 :max 99 :value 55)]
      (is (instance? javax.swing.JSlider s))
      (is (= 40 (.getMinimum s)))
      (is (= 99 (.getMaximum s)))
      (is (= 55 (.getValue s)))))

  (testing "supports the :selection event"
    (let [s (slider)
          called (atom nil)]
      (listen s :selection (fn [e] (reset! called e)))
      (selection! s 49)
      (is @called)))

  (verify-config (slider :snap-to-ticks? true) :snap-to-ticks? true)
  (verify-config (slider :snap-to-ticks? false) :snap-to-ticks? false)
  (verify-config (slider :paint-ticks? true) :paint-ticks? true)
  (verify-config (slider :paint-ticks? false) :paint-ticks? false)
  (verify-config (slider :paint-track? true) :paint-track? true)
  (verify-config (slider :paint-track? false) :paint-track? false)
  (verify-config (slider :inverted? true) :inverted? true)
  (verify-config (slider :inverted? false) :inverted? false))

(deftest progress-bar-test
  (testing "should create a JProgressBar"
    (is (instance? javax.swing.JProgressBar (progress-bar))))
  (testing "should set the progress bars min, max and initial value"
    (let [pb (progress-bar :value 5 :min 1 :max 6)]
      (is (= 5 (.getValue pb)))
      (is (= 1 (.getMinimum pb)))
      (is (= 6 (.getMaximum pb))))))

(deftest select-test
  (testing "should throw an exception if selector is not a vector"
    (try (do (select nil 99) false) (catch IllegalArgumentException e true)))

  (testing "when performing an #id query"
    (testing "should return a single widget"
      (let [f (frame :id :my-frame)]
        (is (= f (select f [:#my-frame])))))

    (testing "should return a single widget"
      (let [c (label :id "hi")
            p (flow-panel :id :panel :items [c])
            f (frame :title "select by id" :content p)]
        (is (= c (select f [:#hi])))
        (is (= p (select f ["#panel"])))))))

(deftest select-with-test
  (testing "should return the equivalent of (partial select widget)"
    (let [lbl (label :id :foo)
          p (border-panel :center lbl)
          $ (select-with p)]
      (is (= lbl ($ [:#foo])))))
  (testing "calling (to-widget) on the result should return the (to-widget input)"
    (let [p (border-panel)
          $ (select-with p)]
      (is (= p (to-widget $))))))

(deftest group-by-id-test
  (testing "should return a map of widgets, keyed by id"
    (let [a (label :id :a)
          b (button :id :b)
          c (border-panel :north a :south b)
          d (listbox :id :d)
          e (grid-panel :items [c d])
          f (frame :id :f :content e)
          result (group-by-id f)]
      (is (= result {:a a :b b :d d :f f})))))

(deftest with-widgets-test
  (testing "should create a binding for the :id of each new widget"
    (let [a-value 99
          b-value 100
          c-value 101]
      (with-widgets [(vector :id :a :value a-value)
                     (vector :id :b :value b-value)
                     (vector :value c-value :id :c)]
        (is (= a [:id :a :value a-value]))
        (is (= b [:id :b :value b-value]))
        (is (= c [:value c-value :id :c])))))
  (testing "should throw IllegalArgumentException for a missing :id"
    (try
      (eval `(with-widgets [(vector :value 99)])) false
      (catch IllegalArgumentException e true)
      ; IllegalArgumentException may be wrapped.
      (catch Exception e
        (instance? IllegalArgumentException (root-cause e))))))

(deftest add!-test
  (testing "When called on a panel with a FlowLayout"
    (testing "adds a widget to the end of the panel"
      (let [p (flow-panel)
            l (label)
            result (add! p l)]
        (is (= result p))
        (is (= l (first (.getComponents p))))))
    (testing "adds a widget to the end of the panel"
      (let [p (flow-panel)
            label0 (label)
            label1 (label)
            result (add! p [label0 nil] label1 )]
        (is (= result p))
        (is (= label0 (first (.getComponents p))))
        (is (= label1 (second (.getComponents p)))))))

  (testing "When called on a panel with a BoxLayout"
    (testing "adds a widget to the end of the panel"
      (let [p (vertical-panel)
            l (label)
            result (add! p l)]
        (is (= result p))
        (is (= l (first (.getComponents p)))))))

  (testing "When called on a panel with a BorderLayout"
    (testing "adds a widget at the given location"
      (let [p (border-panel)
            l (label)
            result (add! p [l :north])]
        (is (= result p))
        (is (= BorderLayout/NORTH (.getConstraints (.getLayout p) l)))))))

(deftest remove!-test
  (testing "removes widgets from a container"
    (let [l0 (label) l1 (label)
          p (border-panel :north l0 :south l1)
          result (remove! p l0 l1)]
      (is (= p result))
      (is (= 0 (count (.getComponents p)))))))

(deftest replace!-test
  (testing "when called on a panel with a generic layout (e.g. flow)"
    (testing "replaces the given widget with a new widget"
      (let [l0 (label "l0")
            l1 (label "l1")
            l2 (label "l2")
            p (flow-panel :items [l0 l1])
            result (replace! p l1 l2)]
        (is (= p result))
        (is (= [l0 l2] (vec (.getComponents p)))))))
  (testing "when called on a panel with a border layout"
    (testing "replaces the given widget with a new widget and maintains constraints"
      (let [l0 (label "l0")
            l1 (label "l1")
            l2 (label "l2")
            p (border-panel :north l0 :south l1)
            result (replace! p l1 l2)]
        (is (= p result))
        (is (= [l0 l2] (vec (.getComponents p))))
        (is (= BorderLayout/SOUTH (-> p .getLayout (.getConstraints l2))))))))

(deftest selection-test
  (testing "should get the selection from a button-group"
    (let [a (radio)
          b (radio :selected? true)
          bg (button-group :buttons [a b])]
      (is (= b (selection bg))))))

(deftest selection!-test
  (testing "should set the selection of a button-group"
    (let [a (radio)
          b (radio)
          bg (button-group :buttons [a b])]
      (is (nil? (selection bg)))
      (selection! bg b)
      (is (= b (selection bg))))))

(deftest dispose!-test
  (testing "should dispose of a JFrame"
    (let [f (pack! (frame :title "dispose!"))]
      (is (.isDisplayable f))
      (let [result (dispose! f)]
        (is (= result f))
        (is (not (.isDisplayable f))))))
  (testing "should dispose of a JDialog"
    (let [f (pack! (dialog :title "dispose!"))]
      (is (.isDisplayable f))
      (let [result (dispose! f)]
        (is (= result f))
        (is (not (.isDisplayable f)))))))

(deftest assert-ui-thread-test
  ; TODO test non-exception case
  (testing "should throw an IllegalStateException if not called on the Swing UI thread"
     @(future
        (try
          (do (assert-ui-thread "some message") false)
          (catch IllegalStateException e true)))))

(deftest move!-test
  (testing "should move the widget to the back of the z order"
      (let [a (label)
            b (label)
            p (xyz-panel :items [a b])]
        (is (= 0 (.getComponentZOrder p a)))
        (move! a :to-back)
        (is (= 1 (.getComponentZOrder p a)))))
  (testing "should move the widget to the front of the z order"
      (let [a (label)
            b (label)
            p (xyz-panel :items [a b])]
        (is (= 1 (.getComponentZOrder p b)))
        (move! b :to-front)
        (is (= 0 (.getComponentZOrder p b)))))
  (testing "should set the absolute location of a widget with a vector"
      (let [lbl (label)
            point [101 102]
            result (move! lbl :to point)
            new-loc (.getLocation lbl)]
        (is (= (java.awt.Point. 101 102) new-loc))))
  (testing "should set the absolute location of a widget with a vector, where :* means to keep the old value"
      (let [lbl (label :location [5 6])
            point [:* 102]
            result (move! lbl :to point)
            new-loc (.getLocation lbl)]
        (is (= (java.awt.Point. 5 102) new-loc))))
  (testing "should set the absolute location of a widget with a Point"
    (let [lbl (label)
          point (java.awt.Point. 99 100)
          result (move! lbl :to point)
          new-loc (.getLocation lbl)]
      (is (= point new-loc))))
  (testing "should set the absolute location of a widget with the upper left corner of a Rectangle"
    (let [lbl (label)
          point (java.awt.Rectangle. 99 100 123 456)
          result (move! lbl :to point)
          new-loc (.getLocation lbl)]
      (is (= (.getLocation point) new-loc))))
  (testing "should set the relative location of a widget with a vector"
      (let [lbl (label)
            point [101 102]
            _ (move! lbl :to [5 40])
            result (move! lbl :by point)
            new-loc (.getLocation lbl)]
        (is (= (java.awt.Point. 106 142) new-loc))))
  (testing "should set the relative location of a widget with a Point"
    (let [lbl (label)
          point (java.awt.Point. 99 100)
          _ (move! lbl :to [5 40])
          result (move! lbl :by point)
          new-loc (.getLocation lbl)]
      (is (= (java.awt.Point. 104 140) new-loc)))))

(defmacro test-paintable [func expected-class]
  `(testing ~(str "creates a paintable " expected-class " for (paintable " func " :paint nil)")
      (let [p# (paintable ~expected-class :paint nil :id :test)]
        (is (instance? ~expected-class p#))
        (is (= :test (id-of p#)))
        (is (= p# (config! p# :paint (fn [~'g ~'c] nil)))))))

(deftest paintable-test
  ; exercise paintable on all the widget types
  (test-paintable flow-panel   javax.swing.JPanel)
  (test-paintable label        javax.swing.JLabel)
  (test-paintable button       javax.swing.JButton)
  (test-paintable toggle       javax.swing.JToggleButton)
  (test-paintable checkbox     javax.swing.JCheckBox)
  (test-paintable radio        javax.swing.JRadioButton)
  (test-paintable text         javax.swing.JTextField)
  (test-paintable password     javax.swing.JPasswordField)
  (test-paintable editor-pane  javax.swing.JEditorPane)
  (test-paintable listbox      javax.swing.JList)
  (test-paintable table        javax.swing.JTable)
  (test-paintable tree         javax.swing.JTree)
  (test-paintable combobox     javax.swing.JComboBox)
  (test-paintable separator    javax.swing.JSeparator)
  (test-paintable menu         javax.swing.JMenu)
  (test-paintable popup        javax.swing.JPopupMenu)
  (test-paintable menubar      javax.swing.JMenuBar)
  (test-paintable toolbar      javax.swing.JToolBar)
  (test-paintable tabbed-panel javax.swing.JTabbedPane)
  (test-paintable slider       javax.swing.JSlider)
  (test-paintable progress-bar javax.swing.JProgressBar)

  (testing "creates a paintable subclass given a class name"
    (let [lbl (paintable javax.swing.JLabel :paint nil :id :foo)]
      (is (instance? javax.swing.JLabel lbl))
      (is (= :foo (id-of lbl)))))

  (testing "creates a label subclass given the label function and args."
    (let [lbl (paintable javax.swing.JLabel :paint nil :id :foo)]
      (is (instance? javax.swing.JLabel lbl))
      (is (= :foo (id-of lbl)))))

  (testing "creates a button subclass"
    (instance? javax.swing.JButton (paintable javax.swing.JButton :paint nil))))

(deftest width-test
  (testing "returns the width of a widget"
    (= 100 (width (xyz-panel :bounds [0 0 100 101])))))

(deftest height-test
  (testing "returns the height of a widget"
    (= 101 (height (xyz-panel :bounds [0 0 100 101])))))

(deftest card-panel-test
  (testing "creates a panel with a CardLayout"
    (let [p (card-panel :hgap 4 :vgap 3 :items [["Label" :first] [(button) :second]])]
      (is (instance? javax.swing.JPanel p))
      (is (instance? java.awt.CardLayout (.getLayout p)))
      (is (= 4 (.. p getLayout getHgap)))
      (is (= 3 (.. p getLayout getVgap)))
      (is (= 2 (count (.getComponents p))))))
  (testing "supports adding cards with add!"
    (let [p (card-panel)
          a (text "A")
          b (text "B")]
      (add! p [a :a])
      (is (= a (first (.getComponents p))))
      (add! p [b "b"])
      (is (= b (second (.getComponents p))))
      (show-card! p "b")
      (is (visible? b)))))

(deftest show-card!-test
  (testing "sets the visible card in a card panel"
    (let [a (label)
          b (button)
          c (checkbox)
          p (card-panel :items [[a :first] [b :second] [c "third"]])]
      (show-card! p :second)
      (is (visible? b))
      (show-card! p "third")
      (is (visible? c)))))

