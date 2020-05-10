(ns texdata.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [join trim]]
            [expound.alpha :as expound]))

;; example for each command and better error message via expound. 

;; TODO: register commands

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

(defn register-cmd!
  ([k type] (register-cmd! k type nil))
  ([k type ex]
   (swap! commands assoc k {:type type :example ex})))

(defn add-example! [command-key & examples]
  (swap! commands update-in [command-key :example] concat examples))

(defn example
  "Returns example data for this command."
  [k]
  (get-in @commands [k :example] ))

;; spec

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
;; main fn

(defmulti data->string tex-data-type)

(defmethod data->string :literal [x] (str x))

(defmethod data->string :single [x] (str "\\" (name x)))

(defmulti environment-command first)

(defmethod environment-command :default [[k & args]]
  (format "\\begin{%s} %s \\end{%s}"
          (name k)
          (join " " (map data->string args))
          (name k)))

(defmulti normal-command first)

(defmethod data->string :normal [data]
  (normal-command data))

(defmethod data->string :environment [data]
  (environment-command data))

;; impl

(defn- check-data [spec data]
  (if (s/valid? spec data )
    data
    (throw (Exception.
            (format "can't convert data:\n\n %s\n\n%s" data
                    (expound/expound-str spec data))))))

;; defcmd 

(s/def ::defcmd-spec
  (s/cat
   :tag keyword?
   :type command-types
   :definition (s/? (s/cat :args vector? :body (s/+ any?)))))

(s/conform ::defcmd-spec '(:documentclass :normal [a b] x))
;; => {:tag :documentclass,
;;     :type :normal,
;;     :definition {:args [a b], :body [x]}}

(defmacro defcmd
  "Registers a new command.
  Example:

  (defcmd :documentclass :normal [data] ...)

  when using default implementation:

  (defcmd :equation :environment)."
  [& args]
  {:pre [(s/valid? ::defcmd-spec args)]}
  (let [data (s/conform ::defcmd-spec args)

        {tag :tag cmd-type :type
         {args :args body :body :as impl} :definition} data

        impl-fn        {:normal `normal-command
                        :environment `environment-command}

        register-part `(register-cmd! ~tag ~cmd-type)

        imple-part `(defmethod ~tag ~cmd-type ~args ~@body)]
    (if impl
      `(do ~register-part ~imple-part)
      `(do ~register-part))))

;; impl

(s/def ::documentclass-spec
  (s/&  (s/cat :cmd keyword?
               :class string?
               :opt (s/? (s/cat :k #{:opt} :v (s/coll-of string?))))
        (s/conformer (fn [{cl :class {v :v} :opt}]
                       {:class cl :opt v}))))

(defcmd :documentclass :normal 
[data]
{:pre [(check-data ::documentclass-spec data)]}
(let [{cl :class opt :opt} (s/conform ::documentclass-spec data)]
  (format "\\documentclass[%s]{%s}" (join "," opt) cl)))

;; when using the default implementation, it suffices just to register command.
(register-cmd! :document :environment)
