(defproject texdata "0.1.6-SNAPSHOT"
  :description "Converts Clojure data to TeX strings."
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [expound "0.8.4"]
                 [org.apache.pdfbox/pdfbox "2.0.19"]]
  :repl-options {:init-ns texdata.core}
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]])


