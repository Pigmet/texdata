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

(def ^:private commands (atom {}))

(defn- register-command [id type]
  (swap! commands assoc id type))

(defn get-all-commands [] @commands)

(defn get-command [id] (get @commands id))

(defn- argument-command-id?
  "Returns logical true iff k is a keyword for
  a command that takes argument(s)."
  [k]
  (let [cmd-type (get-command k)]
    (#{:environment :normal} cmd-type)))

(s/def ::tex-spec
  (s/or
   :string string?
   :number number?
   :independent keyword?
   :command (s/cat :cmd argument-command-id? :args (s/* any?))
   :decorated (s/cat :v ::tex-spec :opt  map?)
   :chunk coll?))

(defn- tex-data-type [x] (key (s/conform ::tex-spec x)))

(declare independent-command
         handle-command
         handle-chunk
         handle-decorated
         tex)

(defmulti data->string tex-data-type)

(defmethod data->string :default [x]
  (case (tex-data-type x)
    (:string :number) (str x)
    :decorated (handle-decorated x)
    :independent (independent-command x)
    :command (handle-command x)
    :chunk (eval (cons `tex x))
    (throw (ex-info "invalid argument in data->string"
                    {:input x}))))

(def ^:private no-whitespace-things
  #{"," "." ";" ":"})

(defn tex [& args] (reduce
                    (fn [acc s] (if (no-whitespace-things s)
                                  (str acc s)
                                  (str acc " " s)))
                    (map data->string args)))

;;;;;;;;;;;;;;;;;;;;
;; tex threading  ;;
;;;;;;;;;;;;;;;;;;;;

(s/def ::tex-arrow-spec
  (s/*  (s/or
         :cmd keyword?
         :cmd-vec (fn [x] (and (coll? x) (keyword? (first x))))
         :vec coll?)))

(defn tex-apply
  "Similar to apply.
  Example:
  (tex-apply :package [\"amsmath\" \"xcolor\"])"
  [& args]
  ( tex (concat (butlast args) (last args) )))

(defn tex-> [exp & args]
  (let [data (s/conform ::tex-arrow-spec args)]
    (reduce (fn [acc [k v]]
              (case k
                :cmd (tex [v acc])
                :cmd-vec (tex (concat [(first v) [acc] (next v)]))
                :vec (tex (concat [acc] v))))
            (tex exp)
            data)))

(defn tex->> [exp & args]
  (let [data (s/conform ::tex-arrow-spec args)]
    (reduce
     (fn [acc [k v]]
       (case k
         :cmd (tex [v acc])
         :cmd-vec (tex (concat v [acc]))
         :vec (tex (concat v [acc]))))
     (tex exp)
     data)))

(defn repeat
  "Inserts x n times."
  [n x]
  (apply tex (clojure.core/repeat n x)))

;;;;;;;;;;
;; impl ;;
;;;;;;;;;;

(defmulti independent-command identity)

(defmethod independent-command :default [k]
  (str " \\" (name k) " "))

(defmulti environemt-command first)

(defmethod environemt-command :default [[cmd & args]]
  (format "\\begin{%s} %s \\end{%s}"
          (name cmd)
          (join (map data->string args))
          (name cmd)))

(defmulti normal-command first)

(defmethod normal-command :default [[cmd & args]]
  (format "\\%s{%s}"
          (name cmd)
          (join
           " "
           (map data->string args))))

(defmulti normal-command first)

(defn- handle-command [[cmd & _ :as data]]
  (case (get-command cmd)
    :environment (environemt-command data)
    :normal (normal-command data)))

(s/def ::defcmd-spec
  (s/cat :id keyword?
         :type keyword?
         :body (s/* any?)))

(defmacro defcmd
  "A macro for implementation of concrete commands.
  If body is nil, just registers the command.

  Example:
  
  when giving a specific implementation...
  (defcmd :equation :environment [data] (...))

  when just registering a command...
  (defcmd :text :normal)"
  [id type & body]
  (let [imple-f (case type
                  :environment `environemt-command
                  :normal `normal-command
                  :independent `independent-command)]
    (if (seq body)
      `(do (register-command ~id ~type)
           (defmethod ~imple-f ~id ~@body))
      `(register-command ~id ~type))))

(defmulti example identity)

(defmethod example :default [_] nil)

;; TODO :Let example handle more information such as comments. 

(defmacro defexample [k data]
  `(defmethod example ~k [~'_] ~data ))

;;;;;;;;;;;;;;;;;;;
;; error message ;;
;;;;;;;;;;;;;;;;;;;

(defn- tex-assert [cmd-key pred m]
  (or pred (throw (ex-info
                   (format "invalid input for %s" cmd-key)
                   m))))

(defmacro check-arg
  "Evaluates test and throws exception when the result is logical falsse.
  cmd-key is a keyword for a specific tex command, for which the test
  checks the validity of input as its argument(s)."
  ([cmd-key test input] `(check-arg ~cmd-key ~test ~input {}))
  ([cmd-key test input m]
   (let [expl (example cmd-key)
         m* (cond-> {:input input}
              expl  (assoc
                     (if (set? expl) :expected-examples :expected-example)
                     expl)
              (seq m) (merge m))]
     `(tex-assert ~cmd-key ~test ~m*))))

;;;;;;;;;;;;;;
;; commands ;;
;;;;;;;;;;;;;;

;; preamble

(s/def ::documentclass-spec
  (s/cat :cmd keyword?
         :class any?
         :opts (s/* any?)))

(defcmd :documentclass :normal
  [data]
  {:pre[(check-arg :documentclass (s/valid? ::documentclass-spec data) data)]}
  (let [{cl :class cmd :cmd opts :opts}
        (s/conform ::documentclass-spec data)
        cmd (name cmd)]
    (format "\\%s[%s]{%s}" cmd
            (join "," (map tex opts))
            (tex cl))))

(defexample :documentclass [:documentclass "article" "12pt"])

(s/def ::package-spec (s/cat
                       :cmd keyword?
                       :package any?
                       :opts (s/* any?)))

(defcmd :package :normal [data]
{:pre[(check-arg :package (s/valid? ::package-spec data) data)]}
(let [{:keys [cmd package opts]} (s/conform ::package-spec data)]
  (cond-> "\\usepackage"
    (seq opts) (str (format "[%s]" (join "," opts)))
    true (str (format "{%s}" (name package))))))

(defexample :package [:package :geometry "landscape" "margin=2in"])

(defcmd :document :environment)

(defcmd :title :normal)

(defcmd :author :normal [[_ & args]]
(format "\\author{%s}" (join " \\and " (map tex args))))

(defcmd :date :normal)

;;;;;;;;;;;;;;;;;;;;;
;; text formatting ;;
;;;;;;;;;;;;;;;;;;;;;

(defcmd :huge :environment)

(defcmd :Huge :environment)

(defcmd :underline :normal)

(defcmd :bold :normal [[_ & args]]
(format "\\textbf{%s}" (join " " (map tex args))))

(defcmd :italic :normal [[_ & args]]
(format "\\textit{%s}" (join " " (map tex args))))

(defcmd :color :normal
[[_ cl & args :as data]]
{:pre [(check-arg :color (string? cl) data
                  {:comment "The first argument specifies color."})]}
(format "\\textcolor{%s}{%s}" cl (tex args)))

(defexample :color [:color "red" 1])

;;;;;;;;;;;;;
;; general ;;
;;;;;;;;;;;;;

(defcmd :math :normal [[_ & args]]
(format "\\[ %s \\]" (join " " (map tex args))))

(defcmd :frac :normal
[[_ x y]] (apply format "\\frac{%s}{%s}" (map tex [x y])))

(defexample :frac [:frac 1 2])

(defcmd :dollar :normal [[_ & args]]
(format "$%s$" (join " " (map tex args))))

(defexample :dollar [:dollar [:int "f"] :eq [:frac 1 2]])

(defcmd :pow :normal [[_ n x]]
(format "%s^{%s}" (tex x) (tex n)))

(defexample :pow [:pow 2 "x"])

(defcmd :sub :normal [[_ n x]]
( format "%s_{%s}" (tex x) (tex n)))

(defexample :sub [:sub 2 "a"])

(s/def ::int-opts-spec
(s/cat :from (s/? any?) :to (s/? any?)))

(s/def ::int-spec
(s/cat :cmd keyword?
       :arg any?
       :opts ::int-opts-spec))

(defn- int-opts-string [{:keys [from to]}]
(cond-> ""
  from (str (format "_{%s}" (tex from)))
  to (str (format "^{%s}" (tex to)))))

(defcmd :int :normal [data]
  {:pre [(check-arg :int (s/valid? ::int-spec data) data)]}
  (let [{:keys [cmd arg opts]} (s/conform ::int-spec data)]
    (cond-> "\\int"
      (seq opts) (str  (int-opts-string opts))
      true (str (tex " " arg)))))

(defexample :int
  #{[:int "f"]
    [:int "f" 1]
    [:int "f" 1 2]})

(defcmd :sqrt :normal
  [[_ & args]]
  (format "\\sqrt{%s}" (tex args)))

(defexample :sqrt [:sqrt 2 :pi])

(defcmd :sqrt-n :normal
  [[_ n & args]]
  (format "\\sqrt[%s]{%s}" (tex n) (tex args)))

(defexample :sqrt-n [:sqrt-n 4 "x"])

;;;;;;;;;;;;;
;; itemize ;;
;;;;;;;;;;;;;

(s/def ::itemize-spec
  (s/cat :cmd keyword?
         :args (s/* (s/or :coll coll? :item any?))))

(defexample :itemize
  #{[:itemize "one" "two"]
    [:itemize ["first item" "one"] ["second item" "two"]]})

(defcmd :itemize :normal [data]
  {:pre[(check-arg :itemize (s/valid? ::itemize-spec data) data)]}
  (let [{:keys [cmd args]} (s/conform ::itemize-spec data)
        cmd (name cmd)
        body (map (fn [[k v]]
                    (case k
                      :coll
                      (apply format "\\item{%s}%s" (map tex v)) 
                      :item
                      (format "\\item %s" (tex v))))
                  args)
        body (join " " (map tex body))]
    (format "\\begin{%s}%s\\end{%s}" cmd body cmd)))

(s/def ::description-spec
  (s/cat :cmd keyword?
         :args(s/& (s/+ any?) #(-> % count even?))))

(defcmd :description :environment [data]
  {:pre[(check-arg :description (s/valid? ::description-spec data) data )]}
  (let [{:keys [cmd args]} (s/conform ::description-spec data)
        cmd (name cmd)
        body (->> args
                  (partition 2)
                  (map (fn [[k v]] (format "\\item[%s]%s" (tex k) (tex v)) ))
                  (join "\n"))]
    (format "\\begin{%s}%s\\end{%s}" cmd body cmd)))

(defexample :description
  [:description
   "first item" 1
   "second item" 2])

;;;;;;;;;;;;;;;;
;; basic math ;;
;;;;;;;;;;;;;;;;

(defcmd :equation :environment)

(defcmd :equation* :environment)

(s/def ::array-spec
  (s/cat :cmd #{:array}
         :pos any?
         :args (s/* any?)))

(defexample :array [:array "cc" 1 :amp 2 :next 3 :amp 4 ])

(defcmd :array :environment [data]
  {:pre [(check-arg :array (s/valid? ::array-spec data) data)]}
  (let [{:keys [cmd pos args]} (s/conform ::array-spec data)]
    (format "\\begin{%s}{%s} %s \\end{%s}"
            (name cmd)
            pos
            (tex args)
            (name cmd))))

(defcmd :align :environment )

(defcmd :align* :environment)

(defexample :align [:align "x" :amp :eq 1 :next "y" :amp :eq 2])

(defexample :align* [:align* "x" :amp :eq 1 :next "y" :amp :eq 2])

(defn- paren-helper [s]
  (let [res (trim s)]
    (if( #{"{" "}"} res) (str "\\" res) res)))

(defcmd :left :normal
  [[_ v]]
  (format "\\left%s" (paren-helper (tex v))))

(defcmd :right :normal
  [[_ v]]
  (format "\\right%s" (paren-helper (tex v))))

(defexample :left [[:left "["] 1 [:right "}"]])

(defexample :right [[:left "["] 1 [:right "}"]])

(s/def ::paren-spec
  (s/cat :cmd keyword?
         :id #{:round :square :curly :angle}
         :args (s/* any?)))

(s/conform ::paren-spec [:paren :curly 1 2])

(defcmd :paren :normal
  [data]
  {:pre [(check-arg :paren (s/valid? ::paren-spec data) data)]}
  (let [{:keys [id args]} (s/conform ::paren-spec data)
        [l r] (case id
                :round ["(" ")"]
                :curly ["{" "}"]
                :angle ["<" ">"]
                :square ["[" "]"])]
    (tex [:left l] (tex args) [:right r])))

(defexample :paren
  #{[:paren :angle "X"]
    [:paren :round "X"]
    [:paren :curly "X"]
    [:paren :square "X"] })

;;;;;;;;;;;;;
;; matrix  ;;
;;;;;;;;;;;;;

(defcmd :matrix :environment)

(defexample :matrix [:matrix 1 :amp 2 :next 3 :amp 4])

(defcmd :pmatrix :environment)

(defcmd :vmatrix :environment)

(defcmd :Vmatrix :environment)


;;;;;;;;;;;;
;; table  ;;
;;;;;;;;;;;;

(defcmd  :tabular :environment[[cmd pos & args]]
  (format "\\begin{%s}{%s} %s \\end{%s}"
          (name cmd) (tex pos) (tex args) (name cmd)))

(defcmd  :table :environment [[cmd pos v]]
  (format "\\begin{%s}[%s] %s \\end{%s}"
          (name cmd)
          (tex pos)
          (tex v)
          (name cmd)))

;;;;;;;;;;;;
;; other  ;;
;;;;;;;;;;;;

(defcmd :eq :independent [_] "=")

(defcmd :minus :independent [_] "-")

(defcmd :amp :independent [_] "&")

(defcmd :quote :normal)

(defcmd :text :normal)

(defcmd :mathbb :normal)

(defcmd :boldsymbol :normal)

(defcmd :next :independent [_] "\\\\")

(s/def ::overbrace-spec
  (s/cat :cmd keyword?
         :arg any?
         :opt (s/? any?)))

(defcmd :overbrace :normal [data]
  {:pre [(check-arg :overbrace (s/valid? ::overbrace-spec data) data)]}
  (let [{:keys [cmd arg opt]} (s/conform ::overbrace-spec data)]
    (cond-> (format "\\%s{%s}" (name cmd) (tex arg))
      opt (str (format "^{%s}" (tex opt))))))

(defcmd :underbrace :normal [data]
  {:pre [(check-arg :underbrace (s/valid? ::overbrace-spec data) data)]}
  (let [{:keys [cmd arg opt]} (s/conform ::overbrace-spec data)]
    (cond-> (format "\\%s{%s}" (name cmd) (tex arg))
      opt (str (format "_{%s}" (tex opt))))))

(defexample :overbrace [:overbrace "f" "explanation"])

(defexample :underbrace [:underbrace "f" "explanation"])

(defcmd :min :normal [[cmd v]]
  (format "\\%s_{%s}"  (name cmd)(tex v)))

(defcmd :max :normal [[cmd v]]
  (format "\\%s_{%s}"  (name cmd)(tex v)))

(defcmd :sup :normal [[cmd v]]
  (format "\\%s_{%s}"  (name cmd)(tex v)))

(defcmd :inf :normal [[cmd v]]
  (format "\\%s_{%s}"  (name cmd)(tex v)))

(defcmd :abs :normal [[_ & args]]
  (format "\\mid %s \\mid" ( tex args)))

(defn- lim-type-impl [[cmd to & args]]
  (format "\\%s_{%s}%s " (name cmd) (tex to) (tex args)))

(defcmd :lim :normal [data] (lim-type-impl data))

(defexample :lim  [:math [:lim ["x" :to 0] "f(x)"] ])

(defcmd :limsup :normal [data] (lim-type-impl data))

(defcmd :varlimsup :normal [data] (lim-type-impl data))

(defcmd :liminf :normal [data] (lim-type-impl data))

(defcmd :varliminf :normal [data] (lim-type-impl data))

;;;;;;;;;;;
;; sigma ;;
;;;;;;;;;;;

(defcmd :sum :normal [[cmd from to & args]]
  (format "\\%s_{%s}^{%s}%s" (name cmd) (tex from) (tex to) (tex args)))

(defcmd :prod :normal [[cmd from to & args]]
  (format "\\%s_{%s}^{%s}%s" (name cmd) (tex from) (tex to) (tex args)))
