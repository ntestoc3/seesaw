;  Copyright (c) Dave Ray, 2012. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Support for RSyntaxTextArea: http://fifesoft.com/rsyntaxtextarea/index.php"
      :author "Dave Ray"}
  seesaw.rsyntax
  (:use [seesaw.util :only [resource resource-key?]])
  (:require [seesaw.core :as core]
            [seesaw.util :as util]
            [seesaw.options :as options]
            [seesaw.widget-options :as widget-options]
            [clojure.java.io :as io]
            clojure.reflect
            clojure.string)
  (:import [org.fife.ui.rsyntaxtextarea
            AbstractTokenMakerFactory
            RSyntaxTextAreaUI
            RSyntaxTextArea
            Theme]
           [org.fife.ui.rtextarea
            RTextAreaUI
            RTextScrollPane]
           javax.swing.UIManager))

;; helper functions
(defn enable-templates!
  [enabled]
  (RSyntaxTextArea/setTemplatesEnabled enabled))

(defn templates-enabled?
  []
  (RSyntaxTextArea/getTemplatesEnabled))

(def rtextarea-keymap
  (util/class-private-field 'RTextAreaUI/RTEXTAREA_KEYMAP_NAME))

(def rtextarea-action-map
  (util/class-private-field 'RTextAreaUI/SHARED_ACTION_MAP_NAME))

(def rtextarea-input-map
  (util/class-private-field 'RTextAreaUI/SHARED_INPUT_MAP_NAME))

(def rsyntax-textarea-input-map
  (util/class-private-field 'RSyntaxTextAreaUI/SHARED_INPUT_MAP_NAME))

(def rsyntax-textarea-action-map
  (util/class-private-field 'RSyntaxTextAreaUI/SHARED_ACTION_MAP_NAME))

(defmacro with-rsyntax-input-action-map-context
  "fix loaded RSyntaxTextArea library multiple times input issue:
  https://github.com/bobbylight/RSyntaxTextArea/issues/269"
  [& body]
  `(let [old-keymap# (javax.swing.text.JTextComponent/getKeymap rtextarea-keymap)
         old-syntax-action# (UIManager/get rsyntax-textarea-action-map)
         old-syntax-input# (UIManager/get rsyntax-textarea-input-map)
         old-action# (UIManager/get rtextarea-action-map)
         old-input# (UIManager/get rtextarea-input-map)]
     (javax.swing.text.JTextComponent/removeKeymap rtextarea-keymap)
     (UIManager/put rsyntax-textarea-action-map nil)
     (UIManager/put rsyntax-textarea-input-map nil)
     (UIManager/put rtextarea-action-map nil)
     (UIManager/put rtextarea-input-map nil)
     (try
       ~@body
       (finally
         (javax.swing.text.JTextComponent/addKeymap rtextarea-keymap old-keymap#)
         (UIManager/put rsyntax-textarea-action-map old-syntax-action#)
         (UIManager/put rsyntax-textarea-input-map old-syntax-input#)
         (UIManager/put rtextarea-action-map old-action#)
         (UIManager/put rtextarea-input-map old-input#)))))

;;; Go through the available syntax highlighting modes,
;;; e.g. "text/clojure" and then for backwards compatibility map them to
;;; keywords without the text/ namespace, e.g. :clojure
(def ^{:private true} syntax-table
  (let [keys (.keySet (AbstractTokenMakerFactory/getDefaultInstance))]
    (into {} (map (juxt (comp keyword name keyword) identity) keys))))

(def ^{:private true} default-themes
  {:dark "org/fife/ui/rsyntaxtextarea/themes/dark.xml"
   :default-alt "org/fife/ui/rsyntaxtextarea/themes/default-alt.xml"
   :default "org/fife/ui/rsyntaxtextarea/themes/default.xml"
   :druid "org/fife/ui/rsyntaxtextarea/themes/druid.xml"
   :eclipse "org/fife/ui/rsyntaxtextarea/themes/eclipse.xml"
   :idea "org/fife/ui/rsyntaxtextarea/themes/idea.xml"
   :monokai "org/fife/ui/rsyntaxtextarea/themes/monokai.xml"
   :vs "org/fife/ui/rsyntaxtextarea/themes/vs.xml"})

(defn- default-theme-key?
  [k]
  (get default-themes k))

(defn ^Theme theme
  "load a theme
    org.fife.ui.rsyntaxtextarea.Theme - return the theme
    default theme key - Load the theme from fife default theme
    an i18n keyword  - Load the theme from the resource bundle specified theme file path
    URL string       - Load the theme from the given URL
    java.io.File     - Load the theme from the File
  "
  [p & base-font]
  (let [load-theme (fn [p]
                     (with-open [r (io/input-stream (if-let [url (io/resource p)]
                                                      url
                                                      p))]
                       (Theme/load r base-font)))]
    (cond
      (nil? p) nil

      (default-theme-key? p) (load-theme (get default-themes p))

      (resource-key? p) (load-theme (resource p))

      :else
      (load-theme p))))

(defn set-theme
  [ta theme-cfg]
  (let [t (if (seq? theme-cfg)
            (apply theme theme-cfg)
            (theme theme-cfg))]
    (.apply ^Theme t ta)))

(defn get-theme
  [ta]
  (Theme. ta))

(def text-area-options
  (merge
   core/text-area-options
   (options/option-map
    (options/bean-option
     [:syntax :syntax-editing-style]
     RSyntaxTextArea
     syntax-table
     nil
     (keys syntax-table))

    (options/default-option :theme set-theme get-theme ["set theme use path or [path base-font]"
                                                        "path can be resource or file path"])
    (options/bean-option [:anti-aliasing? :anti-aliasing-enabled] RSyntaxTextArea)
    (options/bean-option [:highlight-current-line? :highlight-current-line] RSyntaxTextArea)
    (options/bean-option [:auto-indent? :auto-indent-enabled?] RSyntaxTextArea)
    (options/bean-option [:code-folding? :code-folding-enabled?] RSyntaxTextArea)
    (options/bean-option [:margin-line? :margin-line-enabled?] RSyntaxTextArea)
    (options/bean-option [:mark-occurrences? :mark-occurrences] RSyntaxTextArea)
    (options/bean-option [:close-curly-braces? :close-curly-braces] RSyntaxTextArea)
    (options/bean-option [:clear-whitespace-lines? :clear-whitespace-lines-enabled?] RSyntaxTextArea)
    (options/bean-option [:bracket-matching? :bracket-matching-enabled?] RSyntaxTextArea)
    (options/bean-option :whitespace-visible? RSyntaxTextArea)
    (options/bean-option [:tabs-emulated? :tabs-emulated] RSyntaxTextArea)
    (options/bean-option [:animate-bracket-matching? :animate-bracket-matching] RSyntaxTextArea)
    (options/bean-option [:close-markup-tags? :close-markup-tags] RSyntaxTextArea)
    (options/bean-option [:eol-markers-visible? :e-o-l-markers-visible] RSyntaxTextArea)
    (options/bean-option [:highlight-secondary-languages? :highlight-secondary-languages] RSyntaxTextArea)
    (options/bean-option [:hyperlinks? :hyperlinks-enabled] RSyntaxTextArea)
    (options/bean-option [:paint-tab-lines? :paint-tab-lines] RSyntaxTextArea)
    (options/bean-option [:use-focusable-tips? :use-focusable-tips] RSyntaxTextArea)
    (options/bean-option [:paint-matched-bracket-pair? :paint-matched-bracket-pair] RSyntaxTextArea)
    (options/bean-option [:paint-mark-occurrences-border? :paint-mark-occurrences-border] RSyntaxTextArea)
    (options/bean-option [:mark-occurrences? :mark-occurrences] RSyntaxTextArea)
    (options/bean-option [:fractional-font-metrics? :fractional-font-metrics-enabled] RSyntaxTextArea)

    (options/bean-option :hyperlink-foreground RSyntaxTextArea)
    (options/bean-option :mark-occurrences-color RSyntaxTextArea)
    (options/bean-option [:matched-bracket-bg-color :matched-bracket-b-g-color] RSyntaxTextArea)
    (options/bean-option :matched-bracket-border-color RSyntaxTextArea)
    (options/bean-option :use-selected-text-color RSyntaxTextArea)
    (options/bean-option :tab-line-color RSyntaxTextArea)
    (options/bean-option :highlighter RSyntaxTextArea)
    (options/bean-option :tab-size RSyntaxTextArea))))

(widget-options/widget-option-provider
  RSyntaxTextArea
  text-area-options)

(defn text-area
  "Create a new RSyntaxTextArea.

  In addition to normal seesaw.core/text stuff, supports the following:

    :syntax The syntax highlighting. Defaults to :none. Use
            seesaw.dev/show-options to get full list.

  See:
    (seesaw.core/text)
    http://javadoc.fifesoft.com/rsyntaxtextarea/
  "
  [& opts]
  (apply core/config! (RSyntaxTextArea.) opts))

;;;;;;;;;;; RTextScrollPane
(def text-scroll-options
  (merge
   core/scrollable-options
   (options/option-map
    (options/bean-option [:line-numbers? :line-numbers-enabled] RTextScrollPane)
    (options/bean-option [:fold-indicator? :fold-indicator-enabled?] RTextScrollPane)
    (options/bean-option [:icon-row-header? :icon-row-header-enabled?] RTextScrollPane))))

(widget-options/widget-option-provider
 RTextScrollPane
 text-scroll-options)

(defn text-scroll
  "Wrap target in a RTextScrollPane and return the scroll pane.

  The first argument is always the widget that should be scrolled. It's followed
  by zero or more options *for the scroll pane*.

  Additional Options:

    :line-numbers? - Whether or not line numbers are visible.
    :fold-indicator? - Whether the fold indicator is enabled.
    :icon-row-header? -  Whether the icon row header (used for breakpoints, bookmarks, etc.) is enabled

  See:
    http://javadoc.fifesoft.com/rsyntaxtextarea/org/fife/ui/rtextarea/RTextScrollPane.html
  "
  [target & opts]
  (let [^RTextScrollPane tsp (core/construct RTextScrollPane)]
    (.setViewportView tsp (core/make-widget target))
    (options/apply-options tsp opts)))
