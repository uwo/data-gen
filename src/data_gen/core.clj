(ns data-gen.core
  ;(:refer-clojure :exclude [name])
  (:require [faker.company :as company]
            [faker.name :as names]
            [datomic.api :as d]
            [clojure.string :as s]
            [clojure.data.generators :as generators]))

(def directory (clojure.java.io/file "resources/schema"))

(def files (remove #(.isDirectory %) (file-seq directory)))

(defn aggregate-schema
  [m [_ entity-key _ _ definition]]
  (assoc m entity-key definition))

(defn process-schema
  [files]
  (let [merged-schema {}
        lookups (for [f files
                      :let [schema (read-string (slurp f))]]
                  (reduce aggregate-schema {} schema))]
    (into {} lookups)))

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

(defmethod gen-from-schema-type [:keyword :many] [& _]
  (vec (map keyword (repeatedly (rand-int 10) names/last-name))))

(defmethod gen-from-schema-type :ref [schema-lookup [_ _ [_ entity-key] _]]
  nil
  ;(fake entity-key schema-lookup)
  )

(defmethod gen-from-schema-type [:ref :many] [schema-lookup [_ _ [_ entity-key] _]]
  nil
  ;[(fake entity-key schema-lookup)]
  )

(defn ns-attr
  [entity-key attr]
  (let [ns (s/join "." (map s/lower-case ((juxt namespace name) entity-key)))]
    (keyword ns (name attr))))

;e.g.
#_(ns-attr :ti/Bill :service-level)

(defn update-keys
  "Takes a function that is given the current key, instead of a key map."
  [m f]
  (reduce-kv (fn [m k v] (assoc m (f k) v)) {} m))

(defn ns-entity-keys
  [lookup]
  (reduce-kv
    (fn [m entity-key definition]
      (let [new-definition (update-keys definition (partial ns-attr entity-key))]
        (assoc m entity-key new-definition)))
    {}
    lookup))

(defonce schema-lookup (ns-entity-keys (process-schema files)))

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
        (assoc :ti.bill/state state)
        (assoc :ti.bill/next-state next-state))))

(defn fakes
  [entity-key n]
  (let [bills (into []
                    (comp
                      (map add-id)
                      (map use-real-states)
                      (map remove-nil-keys))
                    (repeatedly n #(fake entity-key entity-lookup)))]
    (spit "bills.edn" (pr-str bills))))



#_(fakes :ti/Bill 5)
