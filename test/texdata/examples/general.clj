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
   :graphicx []
   :mathtools[]})

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

(view
 (tex->
  [[:frac :partial [:partial "\\lambda_1"]]
   [:sum {:from ["r" :eq 1] :to :infty}
    :log
    [:paren :round
     [1 "-" :exp
      [:paren :round "-r" [:lambda :sub 1] "-" [:mu :sub 1]]]]]
   :eq
   [:sum {:from ["r" :eq 1] :to :infty}
    [:frac "r"
     [:exp [:paren :curly "r" [:lambda :sub 1] "+" [:mu :sub 1]] "-1"]]]]
  :math
  :huge))



