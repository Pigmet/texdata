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
   :mathtools[]
   :inputenc ["itf8"]})

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

(let [s
      (tex [:documentclass "article"]
           [:usepackage "amsmath"]
           [:newtheorem {:in "section"} "theorem" "Theorem"]
           [:newtheorem {:following "theorem"} "lemma" "Lemma"]
           [:document
            (tex->
             (tex
              [:section "intro"]
              "Here is my theorem"
              [:begin "theorem" [:math "E=1"]]
              [:begin "lemma" [:math "m=1"]]
              [:begin "lemma" [:math "c=1"]]
              [:section "new topic"]
              [:begin "theorem" [:math "x"]])
             :huge)])]
  (compile-and-view test-path s))


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
