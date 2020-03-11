(ns texdata.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [join trim]]
            [texdata.compile
             :refer [compile-and-view* update-compile* compile-tex* ]]))

(defn compile-and-view
  "Takes path to a TeX file and string.
  Writes the string in the file and TeX compiles it via the command 'pdflatex',
  opens the resulting PDF if the compilation was successful."
  [path s]
  (compile-and-view* path s))

;;;;;;;;;;;;;;;;
;; helper fns ;;
;;;;;;;;;;;;;;;;

(defn- belong? [coll x] ((set coll) x))

(defn- env-string [s body]
  (format "\\begin{%s}%s \\end{%s}" s body s))

;;;;;;;;;;;;;;;;;;;;;;
;; specs ;;
;;;;;;;;;;;;;;;;;;;;;;

(def ^:private command-repository (atom{}))

(defn all-commands [] @command-repository)

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

(defn- argument-command?
  "Returns logical true iff id signies a tex command
  that takes arguments, e.g., :frac."
  [id]
  (->> id get-command :type (belong? (command-types :args))))

(s/def ::command-spec
  (s/cat :cmd argument-command?
         :args (s/* any?)))

(s/def ::decorate-spec
  (s/+ (s/cat :k #{:sub :super} :v any?)))

(s/def ::tex-spec
  (s/or :nil nil?
        :literal (some-fn number? string?)
        :independent keyword?
        :decorate (s/cat :v ::tex-spec
                         :decorate ::decorate-spec
                         :rest (s/* ::tex-spec))
        :command ::command-spec
        :chunk (s/coll-of ::tex-spec)))

(defn- conform-tex-spec [data]
  (s/conform ::tex-spec data))

(defmulti data->string
  {:private true}
  (fn [data] (-> data conform-tex-spec key)))

(defn tex [& args] (join " " (map data->string args)))

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

;; data->string

(defmethod data->string :nil [_] "")

(defmethod data->string :literal [x] (str x))

(defmethod data->string :chunk [x] (apply tex x))

(defn- parse-decorate
  "Decomposes data accroding to its component in the
  :decorate type of ::tex-spec."
  [data]
  (let [{v :v decor :decorate r :rest}
        (val (conform-tex-spec data))]
    {:v (s/unform ::tex-spec v)
     :decorate (apply hash-map(s/unform ::decorate-spec decor))
     :others (s/unform (s/* ::tex-spec) r)}))

(defmethod data->string :decorate [data]
  (let [{:keys [v decorate others]} (parse-decorate data)
        {:keys [sub super]} decorate]
    (cond-> (tex v)
      sub (str (format "_{%s}" (tex sub)))
      super (str (format "^{%s}" (tex super)))
      others (str (tex others)))))

(defmethod data->string :independent [data]
  (if-let [s (-> data get-command :s)]
    s
    (str "\\" (name data))))

;;(register-command :equation :environment)

;; Be careful about the selector -> it should be the command key,
;; hence the first item of vector.

(defmulti handle-env-cmd {:private true} first)

(defn- decorate-env-s [cmd-s body]
  (format "\\begin{%s}%s \\end{%s}" cmd-s body cmd-s))

(defmethod handle-env-cmd :default [[cmd & more]]
  (decorate-env-s (name cmd) (tex more)))

(defmulti handle-normal-cmd {:private true} first)

(defmethod handle-normal-cmd :default
  [[cmd & more]]
  (format "\\%s{%s}" (name cmd) (tex more)))

(defmethod data->string :command [data]
  (if (-> data first get-command :type (= :environment))
    (handle-env-cmd data)
    (handle-normal-cmd data)))

(s/def ::defcmd-spec
  (s/cat :id keyword?
         :type #{:normal :environment :independent}
         :body (s/+ any?)))

(defmacro defcmd
  "Registers a new command.

  Example:

  Define normal command:
  (defcmd :math :normal [...]  )

  Define environment command:
  (defcmd :equation :environment [...])

  Define independent command:
  (defcmd :amp :independent \"&\")
  "
  [id type & body]
  (let [input (list* id type body)
        valid? (s/valid? ::defcmd-spec input)
        imple-fns {:environment `handle-env-cmd
                   :normal `handle-normal-cmd}
        default? (-> body first (= :default))
        ind? (= type :independent)]
    (if-not valid? (throw
                    (ex-info "invalid input for defcmd"
                             {:input input}))
            (cond
              ind? `(register-command ~id ~type ~(first body))
              default? `(register-command ~id ~type)
              :else `(do
                       (register-command ~id ~type)
                       (defmethod ~(imple-fns type) ~id ~@body))))))

(defn- defcmd-coll-default
  "Takes command type and collection of command keys.
  Registers them (the commands) via default implementation."
  [type coll]
  (eval  `(do ~@(map
                 (fn [id] `(defcmd ~id ~type :default))
                 coll))))

(def ^:private example-repository 
  (atom {}))

(defn get-all-examples [] @example-repository)

(defn register-example [& kvs]
  (doseq [[k v] (partition 2 kvs)]
    (swap! example-repository assoc k v))
  @example-repository)

(defn example [k] (get @example-repository k))

;; math expressions

(defcmd :math :normal [[_ & more]] (format "\\[ %s \\]" (tex more)))

(defcmd :dol :normal [[_ & more]] (format "$ %s $" (tex more)))

(defcmd-coll-default :environment
  [:equation :equation* :align :align*])

;; special symbol

(defcmd :amp :independent "&")
;
(defcmd :next :independent "\\\\")

(defcmd :sp :independent "\\,")

;; preamble 

(s/def ::standard-opt-spec
  (s/map-of #{:opt}
            (s/or :one string? :more (s/coll-of string?))))

(s/def ::documentclass-spec
  (s/cat :cmd keyword?
         :opt (s/? ::standard-opt-spec)
         :v string?))

(defn- parse-documentclass-data [data]
  (let[{:keys [cmd opt v]} (s/conform ::documentclass-spec data)
       opt (when opt(s/unform ::standard-opt-spec opt))]
    (merge  {:cmd cmd :v v} opt)))

(defn- documentclass-impl [data]
  {:pre [(s/valid? ::documentclass-spec data)]}
  (let [{:keys [cmd opt v]} (parse-documentclass-data data)]
    (cond-> (str "\\" (name cmd))
      (seq opt) (str (format "[%s]"
                             (if (coll? opt) (join "," opt ) opt)))
      v (str (format "{%s}" (tex v))))))

(defcmd :documentclass :normal
  [data]
  (documentclass-impl data))

(register-example
 :documentclass
 #{[:documentclass "article"]
   [:documentclass {:opt "landscape"} "article"]
   [:documentclass {:opt ["12pt" "landscape"]} "article"]})

(defcmd :usepackage :normal
  [data]
  (documentclass-impl data))

;; TODO : add more examples for :usepackage

(register-example
 #{ [:usepackage "amsmath"]})

;;TODO register other commands

;; array

(s/def ::array-spec
  (s/cat :cmd keyword? :pos string? :args (s/* any?)))

(defn- array-impl [data]
  {:pre[(s/valid? ::array-spec data)]}
  (let [{:keys [cmd pos args]} (s/conform ::array-spec data)]
    (env-string (name cmd) (str (format "{%s}" pos) (tex args)))))

(defcmd :array :environment [data] (array-impl data))

(register-example
 :array
 [:array "cc" 1 :amp 2])

;; left right

(s/def ::left-right-spec
  (s/cat :cmd #{:left :right}
         :type  (-> parens-table keys set)))

(def ^:private  parens-table
  {:round ["(" ")"]
   :curly ["\\{" "\\}"]
   :square ["[" "]"]
   :angle ["<" ">"]
   :none ["." "."]})

(defn- left-rght-impl [[cmd v :as data]]
  {:pre [(s/valid? ::left-right-spec data)]}
  (let [[l r] (get parens-table v)
        s (case cmd
            :left l
            :right r)]
    (format "\\%s%s" (name cmd) s)))

(defcmd :left :normal [data] (left-rght-impl data))

(defcmd :right :normal [data] (left-rght-impl data))

(let [ks (keys parens-table)
      lset (set (for [k ks] [:left k]))
      rset (set (for [k ks] [:right k]))]
  (do
    (register-example
     :left
     lset)
    (register-example
     :right
     rset)))

;; font size

(defcmd-coll-default :environment
  [:huge :Huge :small :normalsize :tiny])


