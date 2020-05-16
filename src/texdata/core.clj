(ns texdata.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [join trim]]
            [expound.alpha :as expound]
            [texdata.compile :refer [compile-tex open-file]]))

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

(defmethod data->string nil [data]
  (throw (Exception.
          (format "data->string does not know how to handle %s" data))))

(defn tex[& args] (join " " (map data->string args)))

(defmethod data->string :literal [x] (str x))

(defmethod data->string :single [x] (str "\\" (name x)))

(defmulti environment-command first)

(defmethod environment-command :default [[k & args]]
  (format "\\begin{%s} %s \\end{%s}"
          (name k)
          (join " " (map data->string args))
          (name k)))

(defmulti normal-command first)

(defmethod normal-command :default [[tag & args]]
  (format "\\%s{%s}" (name tag) (join " " (map data->string args))))

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

;; TODO: handle single command. 

(defmacro defcmd
  "args => cmd-key cmd-type (args body)?

  Registers a new command.
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

        imple-part `(defmethod ~(impl-fn cmd-type) ~tag ~args ~@body)]
    (if impl
      `(do ~register-part ~imple-part)
      `(do ~register-part))))

(defn- def-environment-default [k]
  (eval `(defcmd ~k :environment)))

(defn- def-nornaml-default [k]
  (eval `(defcmd ~k :normal)))

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

(->> [:document :environment :equation :equation* :align :align*
      :Huge :huge :small :normalsize]
     (map #(def-environment-default %))
     doall)

;; basic math

(defcmd :lower :normal
  [[_ x]]
  (format "_{%s}" (tex x)))

(defcmd :upper :normal
  [[_ x]]
  (format "^{%s}" (tex x)))

(defcmd :math :normal
  [[_ & args]]
  (format "\\[ %s \\]" (apply tex args)))

(defcmd :frac :normal
  [[_ x y]]
  (format "\\frac{%s}{%s}" (tex x) (tex y)))

(s/def ::int-spec
  (s/& (s/cat :tag keyword?
              :lower (s/? (s/cat :k #{:lower} :v any?))
              :upper (s/? (s/cat :k #{:upper} :v any?))
              :args (s/* any?))
       (s/conformer (fn [{{lower :v} :lower {upper :v} :upper args :args}]
                      {:lower lower :upper upper :args args}))))

(defcmd :int :normal
  [data]
  {:pre[(check-data ::int-spec data)]}
  (let [{:keys [lower upper args]} (s/conform ::int-spec data)]
    (cond-> "\\int"
      lower (str (tex [:lower lower]))
      upper (str (tex [:upper upper]))
      (seq args) (str " " (apply tex args)))))

(add-example! :int
              [:int "f(x)dx"]
              [:int :lower 1 :upper 2 "f(x)dx"])

(comment

  (do
    (def s
      (tex [:documentclass "article"]
           [:document
            [:Huge
             [:equation
              [:int :lower 1 :upper [:frac 2 3]
               (tex "f" [:lower 0] "(x)")
               [:upper [:frac 4 3]] "dx"]]]]))

    (spit  "resources/temp.tex" s)
    (compile-tex "resources/temp.tex" )
    (open-file "resources/temp.pdf"))

  )
