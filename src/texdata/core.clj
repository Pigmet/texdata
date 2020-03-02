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

(defn- register-command-impl [& kvs]
  (register-impl command-repository kvs))

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

(s/def ::command-spec
  (s/cat :cmd get-command :opt (s/? map?) :args (s/* any?)))

(s/def ::tex-spec
  (s/or
   :literal (some-fn string? number?)
   :independent keyword?
   :decorated (s/cat :v any? :decorate ::decorate-spec)
   :command ::command-spec
   :chunk coll?))

(defn- conform-tex [data] (s/conform ::tex-spec data))

(defn- tex-data-type [x]
  (key(s/conform ::tex-spec x)))

(defmulti data->string {:private true} tex-data-type)

(defn tex [& args] (join " " (map data->string args)))

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

(register-decorator :pow tex-pow :sub tex-sub)

(defmethod data->string :decorated [[v & more]]
  (let [m (apply hash-map more)]
    (decorate-tex-impl v @decorator-reposity m)))

(defmethod data->string :independent [x]
  (if-let [s (-> x get-command :s)] (str "\\" s ) (str "\\" (name x))))

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

(def ^:private  example-repository (atom{}))

(defn register-example [& kvs]
  (doseq [[k v] (partition 2 kvs)]
    (swap! example-repository assoc k v)
    @example-repository))

(defn example [id] (get @example-repository id))

;; commands

(defcmd :int :normal [data]
  (let [{:keys [opt args]} (conform-command data)]
    (-> "\\int"
        (decorate-tex opt)
        (str (format " %s" (tex args))))))

(register-decorator
 :from tex-sub
 :on tex-sub
 :to tex-pow)

(tex [:int {:from 0 :to 1} "f"  1 2])












