(ns texdata.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [join trim]]
            [texdata.compile :refer [compile-and-view]])
  (:refer-clojure :exclude [repeat]))

;;;;;;;;;;;;;;;;
;; helper fns ;;
;;;;;;;;;;;;;;;;

(defn- belong? [coll x] ((set coll) x))

;;;;;;;;;;;;;;;;;;;;;;
;; specs ;;
;;;;;;;;;;;;;;;;;;;;;;

