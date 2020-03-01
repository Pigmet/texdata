(ns texdata.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [join trim]]
            [texdata.compile :refer [compile-and-view]]))

;;;;;;;;;;;;;;;;
;; helper fns ;;
;;;;;;;;;;;;;;;;

(defn- belong? [coll x] ((set coll) x))

;;;;;;;;;;;;;;;;;;;;;;
;; specs ;;
;;;;;;;;;;;;;;;;;;;;;;

(def ^:private command-repository (atom{}))

(defn- register-impl [the-atom coll]
  (doseq [[k v] (partition 2 coll)]
    (swap! the-atom assoc k v))
  @the-atom)

(defn- register-command [& kvs]
  (register-impl command-repository kvs))

(def ^:private decorator-reposity (atom {}))

(defn- register-decorator [& kvs]
  (register-impl decorator-reposity kvs))




(s/def ::tex-spec
  (s/or
   :literal (some-fn string? number?)))

