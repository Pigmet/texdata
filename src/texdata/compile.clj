(ns texdata.compile
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh with-sh-dir]])
  (:import (java.nio.file Files Paths)))

(defn- file-data [f]
  (let [f (if (string? f) (io/file f) f)
        parent (.getParent f)
        siblings (->> parent io/file (.listFiles) (map str))]
    {:parent parent
     :name (.getName f)
     :absolute-path (.getAbsolutePath f)
     :siblings siblings}))

(defn compile-tex [path &{:keys [cmd] :or {cmd "pdflatex"}}]
  (let [f (file path)
        {parent :parent s :name} (file-data f)
        {:keys [exit out]}    (with-sh-dir parent
                                (sh cmd s))]
    (if-not (zero? exit)
      (throw (Exception. out))
      "success!")))

(defn open-file [path]
  (sh "open" path))

