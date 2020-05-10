(ns texdata.compile
  (:require [clojure.java.io :refer [file]]
            [clojure.java.shell :refer [sh with-sh-dir]])
  (:import (java.nio.file Files Paths)))

(def temp-file (file "resources/temp.tex"))

(defn- file-data [f]
  {:absolute-path (.getAbsolutePath f)
   :canonical-path (.getCanonicalPath f)
   :parent (.getParent f)
   :directory? (.isDirectory f)
   :name (.getName f)})

(defn- get-siblings [f]
  (let [{:keys [directory? parent]} (file-data f)]
    (if directory?
      (.listFiles f)
      (-> parent file (.listFiles) ))))

(comment
  (->> temp-file
       get-siblings
       (map file-data)
       (map :name)))



