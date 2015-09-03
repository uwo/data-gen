(ns data-gen.core
  ;(:refer-clojure :exclude [name])
  (:require [faker.company :as company]
            [faker.name :as names]
            [datomic.api :as d]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.data.generators :as generators]))

(def directory (clojure.java.io/file "resources/schema"))

(def files (remove #(.isDirectory %) (file-seq directory)))

(defn dasherize
  [word]
  (-> word
      (s/replace (re-pattern "([A-Z][a-z]+)")
                 (fn [[match c]]
                   (if c (str "-" (s/lower-case c)) "")))
      (s/replace (re-pattern "([A-Z]+)") "-$1")
      (s/replace (re-pattern "[-_\\s]+") "-")
      (s/replace (re-pattern "^-") "")))

(defn ns-attr
  [entity-key attr]
  (let [ns (s/join "." (map s/lower-case ((juxt namespace
                                                (comp dasherize name)) entity-key)))]
    (keyword ns (name attr))))

;e.g.
#_(ns-attr :ti/FreightBill :service-level)
#_(dasherize (name :ti/FreightBill))

(defn update-keys
  "Takes a function that is given the current key, instead of a key map."
  [m f]
  (reduce-kv (fn [m k v] (assoc m (f k) v)) {} m))

(defn namespace-attrs
  "namespaces all the keys in `m` with `ns`"
  [[ns m]]
  (update-keys m (partial ns-attr ns)))

(defn aggregate-schema
  "Note that this will merge inherited definitions, however those definitions must have
  already been defined in the schema."
  [m [_ entity-key inherited-types _ definition]]
  (let [inherited-definitions (for [t inherited-types
                                    :let [d (t m)]
                                    :when d]
                                [t d])
        defs (into [[entity-key definition]] inherited-definitions)
        definition (into {} (map namespace-attrs) defs)]
    (assoc m entity-key definition)))

(defn process-schema
  [files]
  (let [merged-schema {}
        lookups (for [f files
                      :let [schema (read-string (slurp f))]]
                  (reduce aggregate-schema {} schema))]
    (into {} lookups)))

(def schema-lookup (process-schema files))

(defn update-vals
  [m f]
  (reduce-kv
    (fn [m k v]
      (assoc m k (f v))) {} m))

(defmulti gen-from-schema-type
  (fn dispatch [schema-lookup [type cardinality? & _]]
    (if (identical? cardinality? :many)
      [type :many]
      type)))

(defn fake
  [entity-key schema-lookup]
  (let [schema (entity-key schema-lookup)]
    (update-vals schema (partial gen-from-schema-type schema-lookup))))

(defmethod gen-from-schema-type :default [_ [type] & [args]]
  (identity args))

(defmethod gen-from-schema-type :instant [& _]
  (generators/date))

(defmethod gen-from-schema-type :string [& _]
  (company/bs))

(defmethod gen-from-schema-type :keyword [& _]
  (keyword (names/last-name)))

(defmethod gen-from-schema-type :bigdec [& _]
  (generators/float))

(defmethod gen-from-schema-type :long [& _]
  (generators/long))

(defmethod gen-from-schema-type :double [& _]
  (generators/double))

(defmethod gen-from-schema-type :code [& _]
  (generators/uuid))

(defmethod gen-from-schema-type :enum [_ [type [_ enum-vals]]]
  (rand-nth enum-vals))

(defmethod gen-from-schema-type [:enum :many] [_ [type _ [_ enum-vals]]]
  (vec (repeatedly (rand-int (count enum-vals)) #(rand-nth enum-vals))))

(def entities (atom []))

(defmethod gen-from-schema-type [:keyword :many] [& _]
  (vec (map keyword (repeatedly (rand-int 10) names/last-name))))

(defn gen-ref
  [schema-lookup entity-key]
  (let [tempid (d/tempid :db.part/user)
        entity (assoc (fake entity-key schema-lookup) :db/id tempid)]
    (swap! entities conj entity)
    tempid))

(defmethod gen-from-schema-type :ref [schema-lookup [_ _ [_ entity-key] _]]
  nil
  ;(gen-ref schema-lookup entity-key)
  )

(defmethod gen-from-schema-type [:ref :many] [schema-lookup [_ _ [_ entity-key] _]]
  nil
  ;(into [] (repeatedly
  ;           (inc (rand-int 3))
  ;           #(gen-ref schema-lookup entity-key)))
  )

(defn add-id
  [m]
  (assoc m :db/id (d/tempid :db.part/user)))

(defn remove-nil-keys
  [m]
  (reduce-kv
    (fn [m k v]
      (if (nil? v)
        m
        (assoc m k v)))
    {}
    m))

(defn use-real-states
  [m]
  (let [states [:init :keying :audit :invoice :invoiced]
        transitions {:init [:keying]
                     :keying [:audit :invoice]
                     :audit [:invoice]
                     :invoice [:invoiced]
                     :invoiced [:done]}
        state (rand-nth states)
        next-state (rand-nth (state transitions))]
    (-> m
        (assoc :ti.freight-bill/state state)
        (assoc :ti.freight-bill/next-state next-state))))

(defn filename
  "note naive pluralization"
  [entity-key]
  (str (s/lower-case (dasherize (name entity-key))) "s.edn"))

(defn fakes
  [entity-key n]
  (let [_ (reset! entities [])
        bills (into []
                    (comp
                      (map add-id)
                      (map use-real-states)
                      (map remove-nil-keys))
                    (repeatedly n #(fake entity-key schema-lookup)))
        all-fakes (vec (concat bills @entities))]
    (spit (filename entity-key) (pr-str all-fakes))))

#_(fakes :ti/FreightBill 1)

(defn check
  [entity-key]
  (pprint (read-string (slurp (filename entity-key)))))

#_(check :ti/FreightBill)


