(ns texdata.example.general
  (:require [texdata.core :refer [tex tex-> tex->> compile-and-view]]
            [texdata.convert :refer [pdf->image]]
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

(defn- view-string
  [&{:keys [packages other size class body]
     :or {packages standard-packages
          size :normalsize
          class "article"}}]
  (tex [:documentclass class]
       (tex (reduce-kv
             (fn [acc k v]
               (tex acc [:usepackage {:opt v} (name k) ]))
             ""
             packages))
       (tex other)
       [:document [size body]]))


(compile-and-view
 test-path
 (view-string  :size :huge
               :body
               (tex [:math :int :sub 1 :super 2 "f"])))



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
      ["The Dirac delta function" [:dol :delta "(x)"] "satisfies"
       [:math
        [:left :curly]
        [:array "c" p1 ",":next p2 "." ]
        [:right :none]]
       "And for any function" [:dol "f(x),"] "we have the equality"
       [:math p3 "."]]])))

(comment

  (compile-and-view
   "test/texdata/examples/out/test.tex"
   dirac-delta)

  (pdf->image
   "test/texdata/examples/out/test.pdf"
   "test/texdata/examples/out/test.png")

  )

