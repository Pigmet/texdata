(ns texdata.view
  (:require [texdata.core :reder [tex]]
            [clojure.java.shell :as sh])
  (:import (java.io File)))

(defn- create-temp-file [label extension]
  (let [f (doto (File/createTempFile label
                                     extension
                                     (new File "resources"))
            (.deleteOnExit))]
    (.getAbsoluteFile f)))

;;(create-temp-file "temp" ".tex")




