(ns texdata.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [join trim]]
            [texdata.compile
             :refer [compile-and-view* update-compile* compile-tex* ]]))

(defn compile-and-view
  "Takes path to a TeX file and string.
  Writes the string in the file and TeX compiles it via the command 'pdflatex',
  opens the resulting PDF if the compilation is successful."
  [path s]
  (compile-and-view* path s))

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

(defn- register-command-impl [& kvs]
  (register-impl command-repository kvs))

(def ^:private command-types
  {:args #{:environment :normal}
   :no-args #{:independent}})

(defn register-command
  ([id type] (register-command id type (name id)))
  ([id type s] (register-command-impl id {:type type :s s})))

(defn- get-command [id] (get @command-repository id))

(def ^:private decorator-reposity (atom {}))

(defn- register-decorator [& kvs]
  (register-impl decorator-reposity kvs))

(defn- get-decorator [id] (get @decorator-reposity id))

(s/def ::decorate-spec
  (s/+ (s/cat :k get-decorator :v any?)))

(defn- argument-command?
  "Returns logical true iff id signies a tex command
  that takes arguments, e.g., :frac."
  [id]
  (->> id get-command :type (belong? (command-types :args))))

(s/def ::command-spec
  (s/cat :cmd argument-command?
         :opt (s/? map?)
         :args (s/* any?)))

(s/def ::tex-spec
  (s/or
   :nil nil?
   :literal (some-fn string? number?)
   :independent keyword?
   :decorated (s/cat :v any? :decorate ::decorate-spec)
   :command ::command-spec
   :chunk coll?))

(defn- conform-tex [data] (s/conform ::tex-spec data))

(defn- tex-data-type [x]
  (key(s/conform ::tex-spec x)))

(defmulti data->string {:private true} tex-data-type)

;; tex does little more than calling data->string, which is
;; a multi-method for converting various data to TeX string.
;; Its dispatch is done based on the type of data
;; specified by the spec ::tex-spec.

(defn tex [& args] (join " " (map data->string args)))

(defn- env-cmd-s [cmd & more]
  (format "\\begin{%s} %s \\end{%s}" cmd (tex more) cmd))

;;;;;;;;;;;;;;;;;;;;
;; tex threading  ;;
;;;;;;;;;;;;;;;;;;;;

(defn tex->> [exp & more]
  (reduce
   (fn [acc x]
     (if (coll? x) (tex (concat x [acc])) ( tex [x acc])))
   (tex exp)
   more))

(defn tex-> [exp & more]
  (reduce
   (fn [acc x]
     (if (coll? x) (tex (concat [acc] x)) (tex [x acc])))
   (tex exp)
   more))

;; data->string 

(defmethod data->string :nil [_] "")

(defmethod data->string :literal[x] (str x))

(defmethod data->string :chunk[x] (apply tex x))

(defn- tex-sub [data v]
  (format "%s_{%s}" (tex data) (tex v)))

(defn tex-pow [data v]
  (format "%s^{%s}" (tex data) (tex v)))

(defn- decorate-tex-impl [data decorators m]
  (reduce-kv
   (fn [acc k v]
     (let [f (get decorators k)]
       (f acc v)))
   (tex data)
   m))

(defn- decorate-tex [data m]
  (decorate-tex-impl data @decorator-reposity m))

(register-decorator :super tex-pow :sub tex-sub)

(defmethod data->string :decorated [[v & more]]
  (let [m (apply hash-map more)]
    (decorate-tex-impl v @decorator-reposity m)))

(defmethod data->string :independent [x]
  (if-let [s (-> x get-command :s)]
    s
    (str "\\" (name x))))

(defn- conform-command [data]
  (s/conform ::command-spec data))

(defmulti env-command {:private true} first)

(defmulti normal-command {:private true} first)

(defmethod data->string :command [data]
  (case (-> data first get-command :type)
    :normal (normal-command data)
    :environment (env-command data)))

(defmethod env-command :default [data]
  (let[{:keys [cmd opt args]} (conform-command data)
       cmd (if-let [s (-> cmd get-command :s)] s (name cmd))]
    (-> (format "\\begin{%s}" cmd)
        (decorate-tex-impl @decorator-reposity opt)
        (str (format " %s \\end{%s}" (tex args) cmd)))))

(defmethod normal-command :default [data]
  (let[{:keys [cmd opt args]} (conform-command data)
       cmd (if-let [s (-> cmd get-command :s)] s (name cmd))]
    (-> (format "\\%s" cmd)
        (decorate-tex-impl @decorator-reposity opt)
        (str (format "{%s}" (tex args)))))) 

(s/def ::defcmd-spec
  (s/cat :id keyword?
         :type #{:environment :normal :independent}
         :body (s/+ any?)))

(defmacro defcmd
  "Defines a new tex command."
  [id type & body]
  {:pre [(s/valid? ::defcmd-spec (list* id type body))]}
  (let [{:keys [id type body]} (s/conform ::defcmd-spec (list* id type body))
        fn-table {:environment `env-command
                  :normal `normal-command}
        default? (-> body first (= :default))]
    (cond
      default? `(register-command ~id ~type)
      (= type :independent) `(register-command ~id ~type ~(first body))
      :else `(do
               (register-command ~id ~type)
               (defmethod ~(get fn-table type) ~id ~@body)))))

(defn- defcmd-coll-default
  "Defines multiple commands via :default. Coll is a collection of command keys
  (e.g. :sup) type is either :normal :environment."
  [type coll]
  (doseq [id coll]
    (eval `(defcmd ~id ~type :default))))

(def ^:private  example-repository (atom{}))

(defn register-example [& kvs]
  (doseq [[k v] (partition 2 kvs)]
    (swap! example-repository assoc k v))
  @example-repository)

(defn example [id] (get @example-repository id))

;; commands implementation

(defn- pre-check-tex
  "Throws ex-info if pred is logical false,
  in which case an example is attached to the exception if
  one is available."
  [pred id data]
  (let [ex (example id)]
    (or pred
        (throw
         (ex-info (format "invalid input for %s" id)
                  (cond-> {:input data}
                    ex (assoc :expected-example ex)))))))


;; basic math 

(def ^:private int-decorators
  {:from tex-sub
   :on tex-sub
   :to tex-pow})

(s/def ::int-spec
  (s/cat :cmd keyword?
         :opt (s/? (s/map-of #{:from :on :to} any?))
         :args (s/* any?)))

(defcmd :int :normal [data]
  {:pre[(pre-check-tex
         (s/valid? ::int-spec data)
         :int
         data)]}
  (let[{:keys [opt args]} (s/conform ::int-spec data)]
    (cond-> "\\int"
      (seq opt)(decorate-tex-impl int-decorators opt)
      (seq args)(str (format " %s" (tex args))))))

(register-example :int [:int {:from 0 :to 1} "f(x)" "dx"])

(defcmd :sum :normal [data]
  {:pre[(pre-check-tex
         (s/valid? ::int-spec data)
         :int
         data)]}
  (let[{:keys [opt args]} (s/conform ::int-spec data)]
    (cond-> "\\sum"
      (seq opt)(decorate-tex opt)
      (seq args)(str (format " %s" (tex args))))))

(register-example :sum
                  [:sum {:from ["n" :eq 1] :to 10} "a_{n}"])

(defcmd :prod :normal [data]
  {:pre[(pre-check-tex
         (s/valid? ::int-spec data)
         :int
         data)]}
  (let[{:keys [opt args]} (s/conform ::int-spec data)]
    (cond-> "\\prod"
      (seq opt)(decorate-tex opt)
      (seq args)(str (format " %s" (tex args))))))

(register-example
 :prod
 [:prod {:from ["n" :eq 1] :to 10} "a_{n}"])

(defcmd :math :normal [[_ & args]]
  (format "\\[ %s \\]" (tex args)))

(defcmd :frac :normal [[_ x y]]
  (format "\\frac{%s}{%s}" (tex x) (tex y)))

(defcmd :equation :environment :default)

(defcmd :equation* :environment :default)

(defcmd :align :environment :default)

(defcmd :align* :environment :default)

(register-example
 :align
 [:align "f(x)" :amp :eq "g(x)"
  :next
  :amp :eq 0]

 :align*
 [:align* "f(x)" :amp :eq "g(x)"
  :next
  :amp :eq 0])

(defcmd :amp :independent "&")

(defcmd :eq :independent "=")

(defcmd :next :independent "\\\\")

(defcmd :array :environment
  [[_ pos & args]]
  (format "\\begin{array}{%s} %s \\end{array}"
          (tex pos)
          (tex args)))

(register-example :array
                  [:array "cc" 1 :amp 2 :next 3 :amp 4])

(def ^:private parens-table
  {:round ["(" ")"]
   :square ["[" "]"]
   :curly ["\\{" "\\}"]
   :angle ["<" ">"]
   :none ["." "."]})

(register-example
 :left
 (into #{} (for [k (keys parens-table)] [:left k]))
 :right
 (into #{} (for [k (keys parens-table)] [:right k]))
 )

(defcmd :left :normal [[_ v :as data]]
  {:pre[(pre-check-tex
         (belong? (keys parens-table) v)
         :left
         data)]}
  (str "\\left"
       (-> v parens-table first)))

(defcmd :right :normal [[_ v :as data]]
  {:pre[(pre-check-tex
         (belong? (keys parens-table) v)
         :right
         data)]}
  (str "\\right"
       (-> v parens-table second)))

(defcmd :paren :normal [[_ k & more :as data]]
  {:pre[(pre-check-tex 
         (belong? (keys parens-table) k)
         :paren
         data
         )]}
  (let [[l r] (get parens-table k)]
    (format "%s %s %s" l (tex more) r)))

(register-example
 :paren
 #{[:paren :curly 1]
   [:paren :angle 1]
   [:paren :round 1]
   [:paren :square 1]})

(defcmd :cases :environment :default)

;; preamble

(s/def ::documentclass-spec
  (s/cat :cmd keyword?
         :opt (s/? (s/map-of #{:opt} (s/coll-of any?)))
         :args any?))

(defcmd :documentclass :normal [data]
  {:pre[(pre-check-tex
         (s/valid? ::documentclass-spec data)
         :documentclass
         data)]}
  (let[{cmd :cmd {opt :opt} :opt args :args} (conform-command data)]
    (cond-> "\\documentclass"
      (seq opt) (str (format "[%s]"(join "," opt)))
      true (str (format "{%s}" (first args))))))

(register-example
 :documentclass
 #{[:documentclass "article"]
   [:documentclass {:opt "12pt"} "article"]})

(defcmd :usepackage :normal [data]
  {:pre[(pre-check-tex
         (s/valid? ::documentclass-spec data)
         :usepckage
         data)]}
  (let[{cmd :cmd {opt :opt} :opt args :args} (conform-command data)]
    (cond-> "\\usepackage"
      (seq opt)      (str (format "[%s]"(join "," opt)))
      true (str (format "{%s}" (first args))))))

(register-example :usepackage
                  #{[:usepackage "xcolor"]})

(defcmd :document :environment :default)

(defcmd :author :normal [[_ & args]]
  (join " \\and " (map tex args) ))

(defcmd-coll-default :normal
  [:part :chapter :section :subsection :subsubsection :date])

(s/def ::newtheorem-spec
  (s/cat :cmd keyword?
         :opt (s/? (s/map-of #{:in :following} string?))
         :tag string?
         :name string?))

;; FIXME: the :following option does not seem to work. why?

(defn- newtheorem-impl [data]
  {:pre [(pre-check-tex
          (s/valid? ::newtheorem-spec data)
          (first data)
          data)]}
  (let [{{in :in following :following} :opt cmd :cmd tag :tag n :name}
        (s/conform ::newtheorem-spec data)
        head (format "\\%s{%s}" (name cmd) tag)]
    (cond
      in (str head (format "{%s}[%s]" n in))
      following (str head (format "[%s]{%s}" following n))
      :else (str head (format "{%s}" n)))))

(defcmd :newtheorem :normal [data] (newtheorem-impl data))

(defcmd :newtheorem* :normal [data] (newtheorem-impl data) )

(register-example :newtheorem
                  #{[:newtheorem "theorem" "Theorem"]
                    [:newtheorem {:in "section"} "theorem" "Theorem" ]
                    [:newtheorem {:following "theorem"} "lemma" "Lemma"]
                    })

(register-example :newtheorem*
                  #{[:newtheorem* "theorem" "Theorem"]
                    [:newtheorem* {:in "section"} "theorem" "Theorem" ]
                    [:newtheorem* {:following "theorem"} "lemma" "Lemma"]
                    })

(s/def ::begin-spec
  (s/cat :cmd keyword?
         :opt (s/? (s/map-of #{:name} string?))
         :tag string?
         :args (s/+ any?)))

(defcmd :begin :normal [data]
  {:pre[(pre-check-tex
         (s/valid? ::begin-spec data)
         :begin
         data)]}
  (let [{{n :name} :opt tag :tag args :args} (s/conform ::begin-spec data)]
    (cond-> (format "\\begin{%s}" tag)
      n (str (format "[%s]" n))
      true (str (format "%s\\end{%s}" (tex args) tag)))))

(register-example
 :begin
 [:begin {:name "Newton"} "theorem" [:math "F=ma"]])

(register-example
 :newtheorem
 [:newtheorem "theorem" "theorem"])

;; formatting text

(defcmd-coll-default :environment [:huge :Huge :small :normalsize])

(defcmd :color :normal [[_ c & more]]
  (format "\\color{%s}{%s}" (tex c) (tex more)))

(register-example :color [:color "red" 1])

(defcmd-coll-default
  :normal
  [:underline :textbf :emph :textit
   :textrm :textmd :textsf
   :sup :sub :wildetilde
   :underbrace :widehat :overrightarrow :overline :overbrace
   :mathbb :mathcal :mathbf])

;; table

(s/def ::table-spec
  (s/cat :cmd keyword?
         :opt (s/? (s/map-of #{:pos} string?))
         :body (s/+ any?)))

(defn- table-impl [data pos-type]
  {:pre[(pre-check-tex
         (s/valid? ::table-spec data)
         (first data)
         data)]}
  (let [{{pos :pos} :opt cmd :cmd body :body}
        (s/conform ::table-spec data)
        pos-fn (if (= pos-type :square)
                 (fn [s] (format "[%s]" s ))
                 (fn [s] (format "{%s}" s)))]
    (cond-> (format "\\begin{%s}" (name cmd))
      pos (str (pos-fn pos))
      body (str (tex body) (format "\\end{%s}" (name cmd))))))

(defcmd :table :environment[data]
  (table-impl data :square))

(defcmd :tabular :environment [data]
  (table-impl data :curly))

(defcmd-coll-default :environment
  [:center :flushleft :flushright])

(defcmd :caption :normal :default)

;;matrix

(defcmd-coll-default :environment
  [:matrix :pmatrix :vmatrix :bmatrix :Vmatrix])

;; accent

(defcmd-coll-default :normal [:b :t :u :H :d :r :c :v])

(defn- accent-impl-code [id s]
  (let [args (gensym "args")]
    `(defcmd ~id :normal [[~'_ & ~args]]
       (format "\\%s{%s}" ~s (tex ~args)))))

(doseq [[id s] [[:a-tilde "~"] [:a-hat "^"] [:a-dash "'"] [:a-dot "."]]]
  (eval (accent-impl-code id s)))

;; accent in math mode

(defcmd-coll-default
  :normal
  [:tilde :grave :hat :vec :check :breve :acute  :dot :var :ddot])

(defcmd :prime :normal [[_ & args]]
  (format "%s\\sp{\\prime}" (tex args)))

;; ref

(defcmd-coll-default :normal [:ref :label :pageref])

;; itemize

(defcmd-coll-default :environment
  [:itemize :enumerate :description])

(s/def ::item-spec (s/cat :cmd keyword?
                          :opt (s/? (s/map-of #{:name} any?))
                          :args (s/* any?)))

(defcmd :item :normal [data]
  {:pre [(pre-check-tex
          (s/valid? ::item-spec data)
          :item
          data)]}
  (let[{{n :name} :opt args :args} (s/conform ::item-spec data)]
    (cond-> (str "\\item")
      n (str (format "{%s}" (tex n) ))
      true (tex args))))

(register-example
 :item  #{[:item "Newton"]
          [:item {:name [:dol "F=ma"]} "Newton"]}

 :itemize   [:itemize
             [:item {:name [:dol "E=mc"]} "Einstein"]
             [:item {:name [:dol "F=ma"]} "Newton"]]

 :enumerate
 [:enumerate
  [:item {:name [:dol "E=mc"]} "Einstein"]
  [:item {:name [:dol "F=ma"]} "Newton"]]

 :description
 [:itemize
  [:item {:name [:dol "E=mc"]} "Einstein"]
  [:item {:name [:dol "F=ma"]} "Newton"]] )


;; other commands

(defcmd :dol :normal [[_ & args]]
  (format "$ %s $" (tex args)))

(defcmd :text :normal :default)

(defcmd :sp :independent "\\;")

(defcmd :sqrt :normal :default)

(defcmd :sqrt-n :normal
  [[_ n & more]]
  (format "\\sqrt[%s]{%s}" (tex n) (tex more)))

(register-example :sqrt-n [:sqrt-n 3 "x"])

(defcmd :minus :normal [[_ & more]]
  (format "- %s" (tex more)))

(def ^:private lim-decorators
  {:as tex-sub})

(defn- lim-type-impl-code [id]
  (let [data (gensym "data")
        cmd (gensym "cmd")
        opt (gensym "opt")
        args (gensym "args")]
    `(defcmd ~id :normal [~data]
       (let [{~cmd :cmd ~opt :opt ~args :args} (conform-command ~data)]
         (cond-> (str "\\" (name ~cmd))
           (seq ~opt) (decorate-tex-impl lim-decorators ~opt)
           true (str " " (tex ~args)))))))

(doseq [id [:limsup :liminf :lim :varlimsup :varliminf]]
  (eval (lim-type-impl-code id)))



