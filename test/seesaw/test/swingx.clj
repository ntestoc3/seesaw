;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.swingx
  (:require [seesaw.core :as core])
  (:require [seesaw.icon :as icon])
  (:require [seesaw.graphics :as graphics])
  (:use seesaw.swingx)
  (:use clojure.test
        ))

(deftest p-built-in-test
  (testing "returns built-in predicates by key"
    (= org.jdesktop.swingx.decorator.HighlightPredicate/ROLLOVER_ROW (p-built-in :rollover-row))))

(deftest p-and-test
  (testing "creates an AndHighlightPredicate with the given parts"
    (let [expected-parts [:always :never :even :odd]
          a (apply p-and expected-parts)
          actual-parts (seq (.getHighlightPredicates a))]
      (is (= actual-parts (map p-built-in expected-parts)))))
  (testing "auto-converts regex to pattern predicate"
    (let [pat #"yum"
          p (p-and pat)]
      (is (= pat (.getPattern (first (.getHighlightPredicates p))))))))

(deftest p-or-test
  (testing "creates an OrHighlightPredicate with the given parts"
    (let [expected-parts [:rollover-row :always :even :odd]
          a (apply p-or expected-parts)
          actual-parts (seq (.getHighlightPredicates a))]
      (is (= actual-parts (map p-built-in expected-parts))))))

(deftest p-not-test
  (testing "creates a NotHighlightPredicate with the given target"
    (= (p-built-in :even) (.getHighlightPredicate (p-not :even)))))

(deftest p-type-test
  (testing "creates a TypeHighlightPredicate with the given class"
    (= String (.getType (p-type String)))))

(deftest p-eq-test
  (testing "creates a EqualsHighlightPredicate with the given value"
    (= "HOWDY" (.getCompareValue (p-eq "HOWDY")))))

(deftest p-column-names-test
  (testing "creates a IdentifierHighlighPredicate with the given column ids"
    (= ["a" :b 3] (->> (p-column-names "a" :b 3) .getIdentifiers seq))))

(deftest p-column-indexes-test
  (testing "creates a ColumnHighlighPredicate with the given column indexes"
    (= [1 2 4] (->> (p-column-indexes 1 2 4) .getColumns seq))))

(deftest p-row-group-test
  (testing "creates a RowGroupHighlightPredicate with the given count"
    (= 6 (.getLinesPerGroup (p-row-group 6)))))

(deftest p-depths-test
  (testing "creates a DepthHighlighPredicate with the given depths"
    (= [1 2 4] (->> (p-depths 1 2 4) .getDepths seq))))

(deftest p-pattern-test
  (testing "creates a PatternHighlighPredicate with the given pattern"
    (let [pat #"hi"]
      (is (= pat (.getPattern (p-pattern pat))))))

  (testing "creates a PatternHighlighPredicate with the given pattern and columns"
    (let [pat (p-pattern #"hi" :test-column 123 :highlight-column 456)]
      (is (= 123 (.getTestColumn pat)))
      (is (= 456 (.getHighlightColumn pat))))))

(deftest p-fn-test
  (testing "creates a Highlighter that calls a two-arg function"
    (let [called (atom false)
          p (p-fn (fn [_ _] (reset! called true)))]
      (is (instance? org.jdesktop.swingx.decorator.HighlightPredicate p))
      (.isHighlighted p nil nil)
      (is @called))))

(deftest hl-color-test
  (testing "returns a function that creates a highlighter with always predicate"
    (let [f (hl-color)
          h1 (f) ]
      (is (instance? org.jdesktop.swingx.decorator.ColorHighlighter h1))
      (is (= (p-built-in :always) (.getHighlightPredicate h1)))))

  (testing "returns a function that creates a highlight with given predicate"
    (let [f (hl-color)
          h1 (f :never) ]
      (is (instance? org.jdesktop.swingx.decorator.ColorHighlighter h1))
      (is (= (p-built-in :never) (.getHighlightPredicate h1)))))
  (testing "can control the color of the highlighter"
    (let [f (hl-color :background java.awt.Color/RED
                     :foreground java.awt.Color/ORANGE
                     :selected-background java.awt.Color/GREEN
                     :selected-foreground java.awt.Color/BLUE)
          h (f)]
      (is (= java.awt.Color/RED (.getBackground h)))
      (is (= java.awt.Color/ORANGE (.getForeground h)))
      (is (= java.awt.Color/GREEN (.getSelectedBackground h)))
      (is (= java.awt.Color/BLUE (.getSelectedForeground h))))))

(deftest hl-icon-test
  (testing "returns a function that creates a highlighter with always predicate"
    (let [f (hl-icon nil)
          h1 (f) ]
      (is (instance? org.jdesktop.swingx.decorator.IconHighlighter h1))
      (is (= (p-built-in :always) (.getHighlightPredicate h1)))))

  (testing "returns a function that creates a highlight with given predicate"
    (let [f (hl-icon nil)
          h1 (f :never) ]
      (is (instance? org.jdesktop.swingx.decorator.IconHighlighter h1))
      (is (= (p-built-in :never) (.getHighlightPredicate h1)))))
  (testing "can control the icon of the highlighter"
    (let [i (icon/icon (graphics/buffered-image 16 16))
          f (hl-icon i)
          h (f)]
      (is (= i (.getIcon h))))))

(deftest hl-shade-test
  (testing "returns a function that creates a highlighter with always predicate"
    (let [f (hl-shade)
          h1 (f) ]
      (is (instance? org.jdesktop.swingx.decorator.ShadingColorHighlighter h1))
      (is (= (p-built-in :always) (.getHighlightPredicate h1)))))

  (testing "returns a function that creates a highlight with given predicate"
    (let [f (hl-shade)
          h1 (f :never) ]
      (is (instance? org.jdesktop.swingx.decorator.ShadingColorHighlighter h1))
      (is (= (p-built-in :never) (.getHighlightPredicate h1))))))

(deftest hl-simple-striping-test
  (testing "returns an simple striping"
    (let [h (hl-simple-striping)]
      (is (instance? org.jdesktop.swingx.decorator.Highlighter h))))
  (testing "returns an simple striping with color"
    (let [h (hl-simple-striping :background :red)]
      (is (instance? org.jdesktop.swingx.decorator.Highlighter h))))
  (testing "returns an simple striping with lines-per-stripe"
    (let [h (hl-simple-striping :lines-per-stripe 6)]
      (is (instance? org.jdesktop.swingx.decorator.Highlighter h)))))

(defmacro verify-highlighter-host [widget]
  `(let [w# ~widget
         hl# (org.jdesktop.swingx.decorator.HighlighterFactory/createSimpleStriping)]
     (set-highlighters w# [hl#])
     (is (= [hl#] (get-highlighters w#)))
     (set-highlighters w# [])
     (is (= nil (get-highlighters w#)))
     (add-highlighter w# hl#)
     (is (= [hl#] (get-highlighters w#)))
     (remove-highlighter w# hl#)
     (is (= nil (get-highlighters w#)))
     (core/config! w# :highlighters [hl#])
     (is (= [hl#] (core/config w# :highlighters)))
     true))

(deftest button-x-test
  (testing "creates a JXButton"
    (instance? org.jdesktop.swingx.JXButton (button-x)))
  (testing "can set text"
    (= "HI" (core/text (button-x :text "HI"))))
  (testing "can set painters"
    (let [p (org.jdesktop.swingx.painter.BusyPainter.)]
      (is (= p (core/config
                     (button-x :background-painter p)
                     :background-painter)))
      (is (= p (core/config
                     (button-x :foreground-painter p)
                     :foreground-painter))))))
(deftest label-x-test
  (testing "creates a JXLabel"
    (instance? org.jdesktop.swingx.JXLabel (label-x)))
  (testing "can set text"
    (= "HI" (core/text (label-x :text "HI"))))
  (testing "does not wrap lines by default"
    (not (core/config (label-x :text "HI") :wrap-lines?)))
  (testing "can set wrap-lines? option"
    (core/config (label-x :wrap-lines? true) :wrap-lines?))
  (testing "can set rotation option"
    (= (Math/toRadians 60.0) (core/config (label-x :text-rotation (Math/toRadians 60.0)) :text-rotation)))
  (testing "can set painters"
    (let [p (org.jdesktop.swingx.painter.BusyPainter.)]
      (is (= p (core/config
                     (label-x :background-painter p)
                     :background-painter)))
      (is (= p (core/config
                     (label-x :foreground-painter p)
                     :foreground-painter))))))

(deftest busy-label-test
  (testing "creates a JXBusyLabel"
    (instance? org.jdesktop.swingx.JXBusyLabel (busy-label)))
  (testing ":busy? defaults to false"
    (not (core/config (busy-label) :busy?)))
  (testing "can set :busy?"
    (core/config (busy-label :busy? true) :busy?))
  (testing "can set the text of the label"
    (= "Processing" (core/text (busy-label :text "Processing")))))

; hyperlink gets grouchy when run on travis with no desktop.
(when (java.awt.Desktop/isDesktopSupported)
  (deftest hyperlink-test
    (testing "creates a JXHyperlink with a URI"
      (let [hl (hyperlink :uri (java.net.URI. "http://google.com"))]
        (is (instance? org.jdesktop.swingx.JXHyperlink hl))))
    (testing "creates a JXHyperlink with a string URI"
      (let [hl (hyperlink :uri "http://google.com")]
        (is (instance? org.jdesktop.swingx.JXHyperlink hl))))))

(deftest task-pane-test
  (testing "creates a JXTaskPane with a title and icon"
    (let [i (icon/icon (graphics/buffered-image 16 16))
          tp (task-pane :title "HI" :icon i)]
      (is (instance? org.jdesktop.swingx.JXTaskPane tp))
      (is (= "HI" (core/config tp :title)))
      (is (= i (core/config tp :icon)))))
  (testing "create a JXTaskPane with actions"
    (let [a  (core/action :name "A")
          b  (core/action :name "B")
          tp (task-pane :actions [a b] )]
      (is (= 2 (.getComponentCount (.getContentPane tp)))))))

(deftest task-pane-container-test
  (testing "creates a JXTaskPaneContainer with some items"
    (let [tpc (task-pane-container)]
      (is (instance? org.jdesktop.swingx.JXTaskPaneContainer tpc)))))

(deftest color-selection-button-test
  (testing "creates a JXColorSelectionButton"
    (instance? org.jdesktop.swingx.JXColorSelectionButton (color-selection-button)))
  (testing "can set the initial color"
    (is (= java.awt.Color/RED
               (core/config
                 (color-selection-button :selection java.awt.Color/RED)
                 :selection))))
  (testing "can retrieve the current selection with (seesaw.core/selection)"
    (is (= java.awt.Color/RED
               (core/selection
                 (color-selection-button :selection java.awt.Color/RED)))))
  (testing "can set the current selection with (seesaw.core/selection!)"
    (let [csb (color-selection-button)]
      (core/selection! csb java.awt.Color/BLACK)
      (is (= java.awt.Color/BLACK (core/selection csb)))))
  (testing "fires :selection event when selection changes"
    (let [called (atom nil)
          csb (color-selection-button :listen [:selection (fn [e] (reset! called e))])]
      (core/selection! csb java.awt.Color/YELLOW)
      (is @called)
      (is (= csb (core/to-widget @called)))))
  (testing "can remove selection event listener"
    (let [called (atom nil)
          csb (color-selection-button)
          remove-fn (core/listen csb :selection (fn [e] (reset! called e)))]
      (remove-fn)
      (core/selection! csb java.awt.Color/YELLOW)
      (is (nil? @called)))))

(deftest header-test
  (testing "creates a JXHeader"
    (instance? org.jdesktop.swingx.JXHeader (header)))
  (testing "can set a title"
    (= "The title" (core/config (header :title "The title") :title)))
  (testing "can set a description"
    (= "The description" (core/config (header :description "The description") :description)))
  (testing "can set an icon"
    (let [i (icon/icon (graphics/buffered-image 16 16))
          h (header :icon i)]
      (is (= i (core/config h :icon))))))

(deftest listbox-x-test
  (testing "creates a JXList"
    (instance? org.jdesktop.swingx.JXList (listbox-x)))
  (testing "creates a JXList with rollover enabled"
    (.isRolloverEnabled (listbox-x)))
  (testing "creates a JXList with a default model"
    (let [lb (listbox-x :model [1 2 3])]
      (is (= 3 (.getSize (core/config lb :model))))))
  (testing "takes a comparator to auto-sort the view"
    (let [lb (listbox-x :sort-with < :model [2 1 3 0])]
      (is (= 3 (.convertIndexToModel lb 0)))))
  (testing "does not sort by default"
    (let [lb (listbox-x :model [2 1 3 0])]
      (is (= 0 (.convertIndexToModel lb 0)))))
  (testing "can set the sort order"
    (let [lb (listbox-x :sort-order :ascending)]
      (is (= javax.swing.SortOrder/ASCENDING (.getSortOrder lb)))
      (core/config! lb :sort-order :descending)
      (is (= javax.swing.SortOrder/DESCENDING (.getSortOrder lb)))))
  (testing "can get the selection when sorted"
    (let [lb (listbox-x :sort-with < :model [2 1 3 0])]
      (core/selection! lb 2)
      (is (= 2 (core/selection lb)))))
  (testing "is a highlighter host"
    (verify-highlighter-host (listbox-x))))

(deftest titled-panel-test
  (testing "creates a JXTitledPanel"
    (instance? org.jdesktop.swingx.JXTitledPanel (titled-panel)))
  (testing "sets the :title of the panel"
    (= "HI" (.getTitle (titled-panel :title "HI"))))
  (testing "sets the :title-color of the panel"
    (= java.awt.Color/RED (.getTitleForeground (titled-panel :title-color :red))))
  (testing "sets the :content of the panel"
    (let [c (core/label "HI")
          tp (titled-panel :content c)]
      (is (= c (.getContentContainer tp)))))
  (testing "passes :content through make-widget"
    (let [tp (titled-panel :content "HI")]
      (is (instance? javax.swing.JLabel (.getContentContainer tp)))))
  (testing "sets the left and right decorations of the panel"
    (let [left (core/label "HI")
          right (core/button :text "BYE")
          tp (titled-panel :left-decoration left :right-decoration right)]
      (is (= left (.getLeftDecoration tp)))
      (is (= right (.getRightDecoration tp))))))

(deftest tree-x-test
  (testing "creates a JXTree"
    (instance? org.jdesktop.swingx.JXTree (tree-x)))
  (testing "creates a JXTree with rollover enabled"
    (.isRolloverEnabled (tree-x)))
  (testing "is a highlighter host"
    (verify-highlighter-host (tree-x))))

(deftest table-x-test
  (testing "creates a JTable"
    (instance? org.jdesktop.swingx.JXTable (table-x)))
  (testing "creates a JXTable with rollover enabled"
    (.isRolloverEnabled (table-x)))
  (testing "creates a JXTable with column control visible"
    (.isColumnControlVisible (table-x)))
  (testing "creates a sortable JXTable"
    (.isSortable (table-x)))
  (testing "can enable horizontal scrollbar"
    (core/config (table-x :horizontal-scroll-enabled? true) :horizontal-scroll-enabled?))
  (testing "can show the column control"
    (not (core/config (table-x :column-control-visible? false) :column-control-visible?)))
  (testing "can set the column margin"
    (= 99 (core/config (table-x :column-margin 99) :column-margin)))
  (testing "is a highlighter host"
    (verify-highlighter-host (table-x))))

(deftest border-panel-x-test
  (testing "creates a JXPanel with border-panel"
    (is (instance? java.awt.BorderLayout
                       (.getLayout (border-panel-x :alpha 0.5))))))

(deftest flow-panel-x-test
  (testing "creates a JXPanel with flow-panel"
    (is (instance? java.awt.FlowLayout
                       (.getLayout (flow-panel-x :alpha 0.5))))))

(deftest horizontal-panel-x-test
  (testing "creates a JXPanel with horizontal-panel"
    (is (instance? javax.swing.BoxLayout
                       (.getLayout (horizontal-panel-x :alpha 0.5))))))

(deftest vertical-panel-x-test
  (testing "creates a JXPanel with vertical-panel"
    (is (instance? javax.swing.BoxLayout
                       (.getLayout (vertical-panel-x :alpha 0.5))))))

(deftest grid-panel-x-test
  (testing "creates a JXPanel with grid-panel"
    (is (instance? java.awt.GridLayout
                       (.getLayout (grid-panel-x :alpha 0.5))))))

(deftest xyz-panel-x-test
  (testing "creates a JXPanel with xyz-panel"
    (is (nil? (.getLayout (xyz-panel-x :alpha 0.5))))))

(deftest card-panel-x-test
  (testing "creates a JXPanel with card-panel"
    (is (instance? java.awt.CardLayout
                       (.getLayout (card-panel-x :alpha 0.5))))))
