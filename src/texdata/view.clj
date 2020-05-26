(ns texdata.view
  (:require [texdata.core :refer [tex example]]
            [clojure.java.shell :as sh])
  (:import (java.io File)))

;; TODO : quickly display the result of compilation of tex data. 

(let [f (File/createTempFile "temp" "tex" (new File "resources"))
      pdf (File/createTempFile "temp" "pdf" (new File "resources")) ]
  (spit f (tex [:documentclass "article"]
               [:document "yay"])))


