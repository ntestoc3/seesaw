(ns ^{:doc "Support for AutoComplete: https://github.com/bobbylight/AutoComplete"
      }
    seesaw.complete
  (:require [seesaw.core :as core]
            [seesaw.util :as util]
            [seesaw.options :as options]
            [seesaw.widget-options :as widget-options]
            [seesaw.config :only [Configurable config* config!*]]
            [clojure.java.io :as io]
            clojure.reflect
            clojure.string)
  (:import [org.fife.ui.autocomplete
            DefaultCompletionProvider
            AutoCompletion
            CompletionProvider
            BasicCompletion
            AbstractCompletion
            FunctionCompletion
            MarkupTagCompletion
            ShorthandCompletion
            TemplateCompletion
            VariableCompletion]
           javax.swing.text.JTextComponent))


(defn add-provider-completions
  [^DefaultCompletionProvider provider completions]
  (doseq [[comp-type comps] completions]
    (case comp-type
      :basic
      (doseq [{:keys [text desc summary]
               :or {desc ""
                    summary ""}} comps]
        (->> (BasicCompletion. provider text desc summary)
             (.addCompletion provider)))

      :shorthand
      (doseq [{:keys [text replace desc summary]
               :or {desc ""
                    summary ""}} comps]
        (->> (ShorthandCompletion. provider text replace desc summary)
             (.addCompletion provider)))

      :template
      (doseq [{:keys [text def-text template desc summary]
               :or {desc ""
                    summary ""}} comps]
        (->> (TemplateCompletion. provider text def-text template desc summary)
             (.addCompletion provider))))))

(defn default-completion-provider
  [{:keys [activation-rules
           ac-words
           any-letters
           completions]
    :or {activate-rules "abcdefghijklmnopqrstuvwxyz.:/-"
         any-letters false
         ac-words []}
    :as opts}]
  (doto  (proxy [DefaultCompletionProvider] [(into-array String ac-words)]
           (isValidChar [^Character ch]
             (-> (or (Character/isLetterOrDigit ch)
                     (#{\. \_ \' \/ \- \:} ch))
                 boolean)))
    (.setAutoActivationRules any-letters activation-rules)
    (add-provider-completions completions)))

(def auto-complete-options
  (options/option-map
   (options/bean-option [:single-choices? :auto-complete-single-choices] AutoCompletion)
   (options/bean-option [:show-desc-window? :show-desc-window] AutoCompletion)
   (options/bean-option [:auto-activation? :auto-activation-enabled?] AutoCompletion)
   (options/bean-option [:enabled? :auto-complete-enabled?] AutoCompletion)
   (options/bean-option [:parameter-assistance? :parameter-assistance-enabled?] AutoCompletion)

   (options/bean-option :trigger-key AutoCompletion seesaw.keystroke/keystroke nil "auto completion trigger key")
   (options/bean-option [:delay :auto-activation-delay] AutoCompletion int nil "the delay between when the user types a character and when the code completion popup should automatically appear (if applicable).")
   (options/bean-option [:provider :completion-provider] AutoCompletion)
   (options/default-option :choices-window-size (fn [^AutoCompletion ac [w h]]
                                                  (.setChoicesWindowSize ac w h)))
   (options/default-option :target (fn [^AutoCompletion ac ^JTextComponent target]
                                     (.install ac target))
                           nil
                           "install autocomplete to JTextComponent target.")
   (options/default-option :description-window-size (fn [^AutoCompletion ac [w h]]
                                                      (.setDescriptionWindowSize ac w h)))))

(extend-protocol seesaw.config/Configurable
  AutoCompletion
  (config* [target name] (options/get-option-value target name))
  (config!* [target args] (options/apply-options target args)))

(options/option-provider AutoCompletion auto-complete-options)

(defn- to-provider
  [arg]
  (cond
    (instance? CompletionProvider arg)
    arg

    (map? arg)
    (default-completion-provider arg)

    :else
    (throw (ex-info "Invalid provider argument" {:arg arg}))))

(defn completion
  "Create AutoCompletion object
  `provider` can be CompletionProvider instance or args passed to default-completion-provider"
  [provider & opts]
  (apply core/config! (-> (to-provider provider)
                          (AutoCompletion.)) opts))
