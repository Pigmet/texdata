(ns texdata.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [join trim]]
            [expound.alpha :as expound]
            [texdata.compile :refer [compile-tex open-file]]))

(def temp-file "/Users/naka/Documents/work/clojure/lib/texdata/resources/temp.tex")

(def temp-pdf-file
  "/Users/naka/Documents/work/clojure/lib/texdata/resources/temp.pdf")

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

(defn add-example!
  "examples -> coll of tex data

  Registers examples for this command."
  [command-key & examples]
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


(defn tex->> [exp & more]
  (reduce
   (fn [acc x]
     (if (coll? x) (tex (concat x [acc])) (tex [x acc])))
   (tex exp)
   more))

(defn tex-> [exp & more]
  (reduce
   (fn [acc x]
     (if (coll? x) (tex (concat [acc] x)) (tex [x acc])))
   (tex exp)
   more))

(defmethod data->string :literal [x] (str x))

(defmulti environment-command first)

(defmethod environment-command :default [[k & args]]
  (format "\\begin{%s} %s \\end{%s}"
          (name k)
          (join " " (map data->string args))
          (name k)))

(defmulti normal-command first)

(defmethod normal-command :default [[tag & args]]
  (format "\\%s{%s}" (name tag) (join " " (map data->string args))))

(defmulti single-command identity)

(defmethod single-command :default [k] (str "\\" (name k)))

(defmethod data->string :normal [data]
  (normal-command data))

(defmethod data->string :environment [data]
  (environment-command data))

(defmethod data->string :single [data]
  (single-command data))

(defmethod single-command :next [_] "\\\\")

(defn def-single-command-map
  "Takes map of single-command-key to string.
  Registers them as single commands."
  [m]
  (dorun
   (map (fn [[k v]]
          (eval `(defmethod single-command ~k [data#] ~v)))
        m)))

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

(defn- def-environment-default [& ks]
  (dorun (map (fn [k]  (eval `(defcmd ~k :environment))) ks))) 

(defn- def-nornaml-default [& ks]
  (dorun (map (fn [k]  (eval `(defcmd ~k :normal))) ks))) 

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

(def-environment-default :document :environment
  :equation :equation* :align :align*
  :Huge :huge :small :normalsize)

(def-nornaml-default :usepackage)

;; color and text formatting

(defcmd :color :normal [[_ col & args]]
  (format "\\textcolor{%s}{%s}" col (apply tex args)))

(add-example! :color  [:color "red" "This text is red."])

(def-nornaml-default :textbf :textit :underline :emph)

(def-environment-default :flushleft :flushright :center)

;; blank spaces

(def-nornaml-default :hspace :vspace)

;; lists

(->> [:itemize :enumerate]
     (map (fn [k]
            (def-environment-default k)
            (add-example! k
                          [k
                           :item "item 1"
                           :item "item 2"])))
     dorun)

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

(def-single-command-map
  {:eq "="})

(defcmd :binom :normal [[_ x y]]
  (format "\\binom{%s}{%s}" (tex x) (tex y)))

(add-example! :binom [:binom 4 3])

;; math operators

(def math-operators [:cos :displaystyle :cos :csc :displaystyle :csc :exp :displaystyle :exp :ker :displaystyle :ker :limsup :displaystyle :limsup :min :displaystyle :min :sinh :displaystyle :sinh :arcsin :displaystyle :arcsin :cosh :displaystyle :cosh :deg :displaystyle :deg :gcd :displaystyle :gcd :lg :displaystyle :lg :ln :displaystyle :ln :Pr :displaystyle :Pr :sup :arctan :displaystyle :arctan :cot :det :displaystyle :det :hom :displaystyle :hom :lim :log :displaystyle :log :sec :tan :displaystyle :tan :arg :displaystyle :arg :coth :displaystyle :coth :dim :displaystyle :dim :liminf :displaystyle :liminf :max :displaystyle :max :sin :displaystyle :sin :tanh])

(def-single-command-map {:sp "\\,"})

;; math fonts

(->> [:mathcal :mathfrak :mathbb :mathnormal :mathrm :mathit :mathbf
      :mathsf :mathtt]
     (map #(def-nornaml-default %))
     dorun)

;; sections chapters

(->> [:title :author :date :section]
     (map #(def-nornaml-default %))
     dorun)

(def chapter-single-commands
  [:today :maketitle])

;; table

(s/def ::table-spec (s/cat :cmd keyword?
                           :pos string?
                           :body (s/* any?)))

        (defcmd :table :environment [data]
          {:pre [(s/valid? ::table-spec data)]}
          (let [{:keys [cmd pos body]} (s/conform ::table-spec data)]
            (env-string (name cmd)
                        (format "[%s]%s" pos  (apply tex body)))))

        (defcmd :tabular :environment [data]
          {:pre [(s/valid? ::table-spec data)]}
          (let [{:keys [cmd pos body]} (s/conform ::table-spec data)]
            (env-string (name cmd)
                        (format "{%s}%s" pos  (apply tex body)))))

        (defcmd :array :environment [data]
          {:pre [(s/valid? ::table-spec data)]}
          (let [{:keys [cmd pos body]} (s/conform ::table-spec data)]
            (env-string (name cmd)
                        (format "{%s}%s" pos  (apply tex body)))))

        (def-single-command-map {:next "\\\\"
                                 :amp "&"})

        (add-example! :table [:table "htb"
                              [:tabular "cc"
                               1 :amp 2 :next
                               3 :amp 4]])

        (add-example! :tabular [:table "htb"
                                [:tabular "cc"
                                 1 :amp 2 :next
                                 3 :amp 4]])

        (add-example! :array [:array "l" 1 :next 2])

        ;; left and right

        (def-single-command-map {:left-curly "\\{"
                                 :right-curly "\\}"
                                 :left-box "["
                                 :right-box "]"})

        ;; matrix

        (def matrix-tags [:matrix :pmatrix :vmatrix :Vmatrix])

        (apply def-environment-default matrix-tags)

        (dorun (map #(add-example! % [%
                                      "a" [:lower 0] :amp  "a" [:lower 1]
                                      :next
                                      "a" [:lower 2] :amp "a" [:lower 3]])
                    matrix-tags))

        ;; demo 

        (defn- demo [& args]
          (let [f "resources/temp.tex"
                s (tex [:documentclass "article"]
                       [:usepackage "amsmath"]
                       [:usepackage "amssymb"]
                       [:usepackage "color"]
                       [:document
                        [:Huge (apply tex args)]])]
            (spit f s)
            (compile-tex f)
            (open-file "resources/temp.pdf")))




