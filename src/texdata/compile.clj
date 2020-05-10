(ns texdata.compile
  (:require [clojure.java.io :refer [file]]
            [clojure.java.shell :refer [sh with-sh-dir]])
  (:import (java.nio.file Files Paths)))

(defn- file-data-1 [f]
  (let [f (if (string? f) (file f) f)]
    {:absolute-path (.getAbsolutePath f)
     :canonical-path (.getCanonicalPath f)
     :parent (.getParent f)
     :directory? (.isDirectory f)
     :name (.getName f)
     :exists? (.exists f)}))

(defn- get-siblings
  "Returns coll of files residing in the same directory as f.
  If f is a directory, returns the files therein."
  [f]
  (let [{:keys [directory? parent]} (file-data-1 f)]
    (if directory?
      (.listFiles f)
      (-> parent file (.listFiles) ))))

(defn file-data [f]
  (merge (file-data-1 f)
         {:siblings (->> f get-siblings (map str))}))

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

