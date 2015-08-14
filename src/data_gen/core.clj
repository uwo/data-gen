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

(defmulti gen-from-schema-val
  (fn dispatch [entity-lookup [type cardinality? & _]] 
    (if (identical? cardinality? :many)
      [type :many]
      type)))

(defn fake-recur
  [entity-key entity-lookup]
  (let [schema (entity-key entity-lookup)]
    (update-vals schema (partial gen-from-schema-val entity-lookup))))

(defmethod gen-from-schema-val :default [_ [type] & [args]]
  (identity args))

(defmethod gen-from-schema-val :instant [& _] 
  (generators/date))

(defmethod gen-from-schema-val :string [& _]
  (company/bs))

(defmethod gen-from-schema-val :keyword [& _]
  (keyword (names/last-name)))

(defmethod gen-from-schema-val :bigdec [& _]
  (generators/float))

(defmethod gen-from-schema-val :long [& _]
  (generators/long))

(defmethod gen-from-schema-val :double [& _]
  (generators/double))

(defmethod gen-from-schema-val :code [& _]
  (generators/uuid))

(defmethod gen-from-schema-val [:keyword :many] [& _]
  (vec (map keyword (repeatedly (rand-int 10) names/last-name))))

(defmethod gen-from-schema-val :ref [entity-lookup [_ _ [_ entity-key] _]]
  (fake-recur entity-key entity-lookup))

(defmethod gen-from-schema-val [:ref :many] [entity-lookup [_ _ [_ entity-key] _]]
  [(fake-recur entity-key entity-lookup)])

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

(defonce entity-lookup (ns-entity-keys (process-schema files)))

(defn add-id
  [m]
  (assoc m :db/id (d/tempid :db.part/user)))

(defn fakes
  [entity-key n]
  (let [bills (into []
                    (map add-id)
                    (repeatedly n #(fake-recur entity-key entity-lookup)))]
    (spit "bills.edn" (pr-str bills))))


#_(fakes :ti/Bill 5)
