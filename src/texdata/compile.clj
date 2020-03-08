(ns texdata.compile
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh])
  (:import (java.awt Desktop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file provides some helper macros for ease of development using seesaw. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro debug
  "prints out args."
  [& args]
  `(do
     (println "--debug--")
     ~@(map (fn [x]   `(println (format "%s -> %s" '~x ~x))) args)
     (println "---------")))

(defn replace-suffix [s suffix repl]
  (if (.endsWith s suffix)
    (str (.substring s 0 (- (count s) (count suffix))) repl)
    s))

(defn pdf-filename
  "Changes the file extension of path from tex to pdf."
  [path]
  (replace-suffix path ".tex" ".pdf"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile tex from Clojure ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(def path "/Users/naka/Documents/work/tex/misc/demo/test.pdf")

(defn- open-file
  "Takes path to a PDF file. Opens it with the default application for
  viewing PDF."
  [path]
  (let [f (io/file path )]
    (try
      (when (Desktop/isDesktopSupported)
        (-> (Desktop/getDesktop) (.open f)))
      (catch Exception e
        (println "No application for PDF is registered.")))))

(defn- file-data [path]
  (let [f (io/file path)]
    {:path (.getAbsolutePath f)
     :dir (.getParent f)}))

(defn compile-tex*
  "Takes path to a tex file. Compiles it and produces pdf in the same directory.
  Returns the path to the resulting pdf."
  [path]
  (let [{:keys [path dir]} (file-data path)
        {:keys [exit out]}(sh/with-sh-dir dir
                            (sh/sh "pdflatex" path ))
        success? (=  exit 0)]
    (if success?
      (pdf-filename path)
      (throw (Exception.
              (format "tex compile error:\n %s" out))))))

(defn update-compile*
  "Updates f by writing s and tex compiles it.
  Returns the path to the resulting PDF if the compilation was successful."
  [f s]
  (spit f s)
  (compile-tex* f))

(defn compile-and-view* [f s]
  (let [pdf (update-compile* f s)]
    (open-file pdf)))





