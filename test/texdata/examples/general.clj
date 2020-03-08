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
          (str acc (tex [:usepackage {:opt v} (name k)])))
        ""
        standard-packages)
       [:document (tex coll)]))

(defn view [& args]
  (compile-and-view
   test-path
   (demo-string args)))

(view
 (tex->

(comment
  

  (def dirac-delta
    (let [p1 (tex :delta "(x)"
                  :eq 0
                  :sp [:text "if" [:dol "x" :neq 0]])
          
          p2 (tex [:int {:from ["-" :infty] :to :infty}
                   :delta "(x)dx"]
                  :eq 1)
          
          p3 (tex [:int {:from ["-" :infty] :to :infty}
                   :delta "(x)"
                   "f(x)dx"]
                  :eq "f(0)")]

      (tex
       [:documentclass "article"]
       [:usepackage "amsmath"]
       [:usepackage "amssymb"]
       [:usepackage {:opt ["left = 20mm" "right = 20mm"] } "geometry"]
       [:document
        [:huge
         ["The Dirac delta function" [:dol :delta "(x)"] "satisfies"
          [:math
           [:left :curly]
           [:array "c" p1 ",":next p2 "." ]
           [:right :none]]
          "And for any function" [:dol "f(x),"] "we have the equality"
          [:math p3 "."]]]])))

  
  test-path
  ;; => 
  (compile-and-view
   "test/texdata/examples/out/test.tex"
   dirac-delta )

  


  )
