(ns texdata.example.general
  (:require [texdata.core :refer :all]
            [texdata.compile :refer [compile-and-view]]
            [clojure.string :refer [join]]))

(def packages
  ["amsmath" "xcolor" "amssymb" "multirow" "enumerate"])

(def test-path
  "/Users/naka/Documents/work/tex/misc/demo/test.tex" )

(defn demo [s &{:keys [font] :or {font :huge}}]
  (let [s* (tex [:documentclass]
                (join "\n" (map #(tex [:package %]) packages))
                (tex [:package "geometry" "left = 20mm" "right = 20mm"])
                [:document [font s]])]
    (compile-and-view test-path s*)))

(defn prob [& args]
  (tex [:text "Pr"] [:paren :square (tex args)]))



