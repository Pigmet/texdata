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

(def test-path "test/texdata/examples/out/test.tex")

(defn- demo-string [coll]
  (tex [:documentclass "article"]
       (reduce-kv
        (fn [acc k v]
          (str acc (tex [:usepckage {:opt v} (name k)])))
        ""
        standard-packages)
       [:document (tex coll)]))

(defn view [& args]
  (compile-and-view
   test-path
   (demo-string args)))

(def items
  {"apple" {:price 100 :calory 200}
   "banana" {:price 50 :calory 54}
   "mikan" {:price 34 :calory 6}})

  (defn items-string [m]
    (join " " (for [[k {:keys [price calory]}] items]
                (tex k :amp price :amp calory :next  :hline))))
  
(def t5
  (tex->>
   (items-string items)
   [:tabular "|c|c|c|"]
   [[:caption "table demo"]]
   :Huge
   :center
   [:table "h"]))

(view t5)


