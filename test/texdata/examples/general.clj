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

(comment

  (def t1
    (tex->
     ["x" :amp :eq 1 :next "y" :amp :eq 2]
     :align*
     :Huge))

  (def t2
    (tex-> [[:frac 1 2] [:int {:from 0 :to :infty} "f(x) dx"] ]
           :math
           :huge))

  (def t3
    (tex-> [[:left :curly]
            [:array "cc"
             "x" :amp :eq 1 :next
             "y" :amp :eq 2 ]
            [:right :none]]
           :equation
           :Huge))

  (view t3)
  
  )
