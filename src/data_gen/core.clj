(ns data-gen.core
  ;(:refer-clojure :exclude [name])
  (:require [faker.company :as company]
            [faker.name :as names]
            [clojure.data.generators :as generators]))

(def bill-template
  {:state [:keyword "docstring"]
   :next-state [:keyword "docstring"]
   :customer [:ref :one [:ti/ref-type :ti/Customer] "docstring"]
   :carrier [:ref :one [:ti/ref-type :ti/Carrier] "docstring"]
   :pro-number [:string "docstring"]
   :direction [:string "docstring"]
   :terms [:string "docstring"]
   :pickup-date [:instant "docstring"]
   :received-date [:instant "docstring"]
   :freight-bill-date [:instant "docstring"]
   :delivery-date [:instant "docstring"]
   :freight-details [:ref :many [:ti/ref-type :ti/FreightDetail] "docstring"]
   :references [:ref :many [:ti/ref-type :ti/Reference] "docstring"]
   :charge-groups [:ref :many [:ti/ref-type :ti/BillCharges] "docstring"]
   :mode [:string "docstring"]
   :service-level [:string "docstring"]
   :equipment-type [:string "docstring"]
   :bill-flags [:keyword :many "docstring"]
   :system-flags [:keyword :many "docstring"]
   :duplicate-flags [:keyword :many "docstring"]
   :total-miles [:double "docstring"]
   :milage-source [:string "docstring"]
   :mileage-type [:string "docstring"]
   :average-class [:double "docstring"]
   :total-cube [:double "docstring"]
   :carrier-shipper [:ref :one [:ti/ref-type :ti/Location] "docstring" ]
   :carrier-consignee [:ref :one [:ti/ref-type :ti/Location] "docstring"]
   :audited-shipper [:ref :one [:ti/ref-type :ti/Location] "docstring" ]
   :audited-consignee [:ref :one [:ti/ref-type :ti/Location] "docstring"]
   :shipments [:ref :many [:ti/ref-type :ti/Shipment] "docstring"]
   :accessorials [:ref :many [:ti/ref-type :ti/Accessorial] "docstring"]
   :stops [:ref :many [:ti/ref-type :ti/BillStop] "docstring"]})

(defn update-map-ks
  [m f]
  (reduce-kv
    (fn [m k v]
      (assoc m k (f v))) {} m))

(defmulti gen-from-schema-val
  (fn [[type cardinality] & _] 
    (if (identical? cardinality :many)
      [type :many]
      type)))

(defmethod gen-from-schema-val :default [& [args]] 
  (identity args))

(defmethod gen-from-schema-val :instant [& _] 
  (generators/date))

(defmethod gen-from-schema-val :string [& _]
  (company/bs))

(defmethod gen-from-schema-val :keyword [& _]
  (keyword (names/last-name)))

(defmethod gen-from-schema-val [:keyword :many] [& _]
  (vec (map keyword (repeatedly (rand-int 10) names/last-name))))

#_(defmethod gen-from-schema-val :ref [_ _ [_ entity] _]
  nil)

#_(defmethod gen-from-schema-val [:ref :many] [_ _ [_ entity] _]
  nil)

(def fake-bill
  (update-map-ks bill-template gen-from-schema-val))

#_(clojure.pprint/pprint fake-bill)



