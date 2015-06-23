(ns
  ^{:author raptor_MVK}
  video_guide.info)

(defprotocol EntityInfo
  (sql-fields [_])
  (form-fields [_])
  (columns [_]))

(defrecord CommonEntityInfo [name fields-info]
  EntityInfo
  (sql-fields [_]
    (filter #(.sql? %) fields-info))
  (form-fields [_]
    (sort (fn [x y] (< (.field-order x) (.field-order y)))
      (filter #(.field? %) fields-info)))
  (columns [_]
    (sort (fn [x y] (< (.column-order x) (.column-order y)))
      (filter #(.column? %) fields-info))))

(defprotocol FieldInfo
  (field-order [_])
  (column-order [_])
  (field? [_])
  (column? [_])
  (sql? [_]))
