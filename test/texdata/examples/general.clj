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
   [:tabular "|c|c|c|"
    :hline
    "item" :amp "price" :amp "calory"
    :next :hline :hline]
   [[:caption "table demo"]]
   :Huge
   :center
   [:table "h"]))

(def t6
  (tex->
   [[:left :angle] ["a" :pow "n"] [:right :angle]
    :eq
    [:int "d" :alpha  [:alpha :pow "n"] "f(\\alpha \\mid x_1, \\cdots , x_n)" ]]
   :math
   :Huge))

(def t7
  (tex->
   ["L(\\alpha, \\beta)"
    :eq
    [:cases 0  :amp :alpha :eq :beta :next
     1 :amp [:text "otherwise"]]]
   :math
   :Huge))

(def a-fn "f(x_1, \\cdots, x_n)")

(def t8
  (tex->
   [[:frac :partial [:partial :beta]]
    [:int "d\\alpha" :sp "g(\\alpha) L(\\alpha, \\beta)" a-fn ]
    :eq
    0]
   :math
   :huge))


(def t9
  (tex->
   ["L_{ij}" :eq [:pmatrix 0 :amp 1 :next
                  1 :amp 0 ]]
   :math
   :huge))

(example :align*)

(view
 (tex->
  [:align* "p(R\\mid X)" :amp :eq "p" [:sum {:on "V"} "p(D_0 \\mid V)"]
   :next
   :amp :eq "p"[:int {:from 0 :to "V_b"} "dV" :sp "W(V-S)" ]]
  :huge))


(view
 (tex->
  [:align "f(x)" :amp :eq "g(x)"
   :next
   :amp :eq 0]))


