(defproject ntestoc/seesaw "0.1.8"
  :description "A Swing wrapper/DSL for Clojure with virtual dom. You want seesaw.core, FYI. See http://seesaw-clj.org for more info."

  :url "http://seesaw-clj.org"

  :license {:name         "Eclipse Public License - v 1.0"
            :url          "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments     "same as Clojure"}

  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass
                                    :sign-releases false}]]

  :warn-on-reflection true

  ; To run the examples:
  ;
  ;   $ lein examples
  ;
  :aliases {"examples" ["run" "-m" "seesaw.test.examples.launcher"]}

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.miglayout/miglayout "3.7.4"]
                 [com.jgoodies/forms "1.3.0"]
                 [org.swinglabs.swingx/swingx-core "1.6.5-1"]
                 [org.clojure/core.rrb-vector "0.1.2"]
                 [j18n "1.0.2"]
                 [com.fifesoft/autocomplete "3.1.0"]
                 [com.fifesoft/rsyntaxtextarea "3.1.1"]]
  :plugins [[lein-ancient "0.6.15"]
            [lein-cljfmt "0.6.4"]]
  :profiles {:dev {:dependencies [[lein-autodoc "0.9.0"]]}}
  :repositories [["stuartsierra-releases" "https://stuartsierra.com/maven2"]]
  :autodoc {
            :name       "Seesaw",
            :page-title "Seesaw API Documentation"
            :copyright  "Copyright 2012, Dave Ray"}
  :java-source-paths ["jvm"])

