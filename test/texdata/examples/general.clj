(ns texdata.example.general
  (:require [texdata.core :refer :all]
            [texdata.compile :refer [compile-and-view]]
            [clojure.string :refer [join]]))

(def standard-packages
  {:amsmath []
   :xcolor []
   :amssymb []
   :multirow []
   :enumerate []
   :geometry ["left = 20mm" "right = 20mm"]
   :graphicx []})

(defn- package-text []
  (reduce-kv
   (fn [acc k v] (str acc  (tex-apply :package (name k) v) "\n"))
   ""
   standard-packages))

(def test-path "test/texdata/examples/out/test.tex")

(defn- demo-text [s &{:keys [font] :or {font :huge}}]
  (tex [:documentclass "article"]
       (package-text)
       [:document [font s]]))

(defn- demo [& args]
  (let [s (demo-text (tex args))]
    (compile-and-view test-path s)))

(example :includegraphics)

(demo
"Graphics demo"
 [:includegraphics "5cm" "Sample.png"])
