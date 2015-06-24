(ns
  ^{:author raptor_MVK}
  video_guide.info)

(declare check-fields-info default-field-value)

(defprotocol EntityInfo
  (sql-fields [_])
  (form-fields [_])
  (columns [_])
  (table [_])
  (default-entity [this]))

(defrecord CommonEntityInfo [name fields-info]
  EntityInfo
  (sql-fields [_]
    (map #(.sql-field %) (filter #(.sql? %) fields-info)))
  (form-fields [_]
    (sort (fn [x y] (< (.field-order x) (.field-order y)))
      (filter #(.field? %) fields-info)))
  (columns [_]
    (sort (fn [x y] (< (.column-order x) (.column-order y)))
      (filter #(.column? %) fields-info)))
  (table [_] name)
  (default-entity [this]
    (let [field-values (map default-field-value (form-fields this))]
      (if (not-empty field-values)
        (reduce into field-values)
        nil))))

(defprotocol FieldInfo
  (field [_])
  (label [_])
  (getter [_])
  (default [_])
  (column [_])
  (sql-field [_])
  (sql-name [_])
  (field-order [_])
  (column-order [_])
  (field? [_])
  (column? [_])
  (sql? [_]))

(defn make-entity-info
  "Given name and fields-info list, returns CommonEntityInfo. If fields-info
  is empty or check-fields-info(fields-info) returns false, then throws
  IllegalArgumentException"
  [name fields-info]
  (if (and (not-empty fields-info) (check-fields-info fields-info))
    (CommonEntityInfo. name fields-info)
    (throw (IllegalArgumentException.))))

(defn- check-fields-info
  "Given fields-info list, checks that all fields (field? returns true) and
  columns (column? returns true) have different field-order and column-order
  respectively"
  [fields-info]
  (let [fields (filter #(.field? %) fields-info)
        columns (filter #(.column? %) fields-info)
        field-order-set (into #{} (map #(.field-order %) fields))
        column-order-set (into #{} (map #(.column-order %) columns))]
    (and (= (count fields) (count field-order-set))
      (= (count columns) (count column-order-set)))))

(defn- default-field-value
  [field-info]
  (hash-map (.sql-name field-info) (.default field-info)))