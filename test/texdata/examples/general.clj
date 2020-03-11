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

(def dirac-delta
  (tex
   [:documentclass "article"]
   [:usepackage "amsmath"]
   [:usepackage "amssymb"]
   [:usepackage {:opt ["left = 20mm" "right = 20mm"] } "geometry"]
   [:document
    [:huge
     ["The Dirac delta function" [:dol :delta "(x)"] "satisfies:"
      [:enumerate
       [:item
        [:math :delta "(x)" :eq 0 :sp
         [:text "for all" [:dol "x" :neq 0]]]]
       [:item
        [:math :int :sub ["-" :infty] :super :infty
         :delta "(x)dx"
         :eq 1]]]
      "From these properties, it follows that for all function"
      [:dol "f,"] 
      [:math
       :int :sub ["-" :infty] :super :infty
       :delta "(x)" "f(x)" "dx"
       :eq
       "f(0)."]]]]))


(comment

  (compile-and-view
   "test/texdata/examples/out/test.tex"
   dirac-delta)

  (compile-and-view
   test-path
   (view-string
    :size :huge
    :body (tex [:math :lim :sub ["x" :to 0] "f(x)" :eq 1])))

  (pdf->image
   "test/texdata/examples/out/test.pdf"
   "test/texdata/examples/out/test.png")

  )

