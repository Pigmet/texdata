(ns texdata.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [join trim]]))

;; TODO: enable other compile options, produce dvi files
;; MEMO: is defcmd worth the trouble?

;;;;;;;;;;;;;;;;
;; helper fns ;;
;;;;;;;;;;;;;;;;

(defn- belong? [coll x] ((set coll) x))

(defn- env-string [s body]
  (format "\\begin{%s}%s \\end{%s}" s body s))

;; data for impl

(def ^:private  parens-table
  {:round ["(" ")"]
   :curly ["\\{" "\\}"]
   :square ["[" "]"]
   :angle ["<" ">"]
   :none ["." "."]})

;; specs

(def command-types #{:environment :normal :single})

(def commands (atom {}))

(defn- register-cmd!
  ([type k] (register-cmd! type k nil))
  ([type k ex]
   (swap! commands assoc k {:type type :example ex})))

(defn example
  "Returns example data for this command."
  [k]
  (get-in @commands [k :example ] ))

(s/def ::tex-spec
  (s/or :literal (some-fn string? number?)
        :single keyword?
        :normal #(->> % first (get @commands) :type (= :normal) )
        :environment #(->> % first (get @commands) :type (= :environment) )))

(defn- tex-data-type
  "Returns one of the tex-spec keys if x conforms to it, or nil."
  [x]
  (when (s/valid? ::tex-spec x)
    (key (s/conform ::tex-spec x))))

;; example

;; main fn

(defmulti data->string tex-data-type)

(defmethod data->string :literal [x] (str x))

(defmethod data->string :single [x] (str "\\" (name x)))

(defmulti environment-command first)

(defmulti normal-command first)

(defmethod data->string :normal [data] (normal-command data))

(defmethod data->string :environment [data] (environment-command data))

;; impl

(s/def ::documentclass-spec
  (s/&  (s/cat :cmd keyword?
               :class string?
               :opt (s/? (s/cat :k #{:opt} :v (s/coll-of string?))))
        (s/conformer (fn [{cl :class {v :v} :opt}]
                       {:class cl :opt v}))))

(s/conform ::documentclass-spec
           [:documentclass "a" :opt [1 2 3] 9])

(defmethod normal-command :documentclass
  [data]
  (let [{cl :class opt :opt} (s/conform ::documentclass-spec data)]
    (format "\\documentclass[%s]{%s}" (join "," opt) cl  )))

(register-cmd! :normal
               :documentclass
               [[:documentclass "article"]
                [:documentclass "article" :opt ["letterpaper" "12pt"]]])

(data->string [:documentclass "article" :opt ["letterpaper" "12pt"]])

(example :documentclass)




