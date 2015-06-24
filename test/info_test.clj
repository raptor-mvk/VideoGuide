(ns
  ^{:author raptor_MVK}
  video_guide.info_test
  (:require [clojure.test :refer :all]
            [video_guide.ext :refer :all]
            [video_guide.info :refer [make-entity-info make-domain-info]])
  (:import (video_guide.info FieldInfo CommonEntityInfo CommonDomainInfo)))

(defrecord SQLFieldInfo [name]
  FieldInfo
  (field [_] name)
  (label [_] name)
  (getter [_] true)
  (default [_] 0)
  (column [_] name)
  (sql-field [_] [name :sql])
  (sql-name [_] name)
  (field-order [_] 0)
  (column-order [_] 0)
  (field? [_] false)
  (column? [_] false)
  (sql? [_] true))

(defrecord FormFieldInfo [name field-order]
  FieldInfo
  (field [_] name)
  (label [_] name)
  (getter [_] true)
  (default [_] 0)
  (column [_] name)
  (sql-field [_] [name :form])
  (sql-name [_] name)
  (field-order [_] field-order)
  (column-order [_] 0)
  (field? [_] true)
  (column? [_] false)
  (sql? [_] false))

(defrecord ColumnFieldInfo [name column-order]
  FieldInfo
  (field [_] name)
  (label [_] name)
  (getter [_] true)
  (default [_] 0)
  (column [_] name)
  (sql-field [_] [name :column])
  (sql-name [_] name)
  (field-order [_] 0)
  (column-order [_] column-order)
  (field? [_] false)
  (column? [_] true)
  (sql? [_] false))

(def sql-field1 (SQLFieldInfo. :name))
(def sql-field2 (SQLFieldInfo. :duration))
(def sql-field3 (SQLFieldInfo. :width))
(def form-field1 (FormFieldInfo. :name 1))
(def form-field2 (FormFieldInfo. :duration 2))
(def form-field3 (FormFieldInfo. :width 3))
(def column-field1 (ColumnFieldInfo. :name 1))
(def column-field2 (ColumnFieldInfo. :duration 2))
(def column-field3 (ColumnFieldInfo. :width 3))

(deftest entity-info-sql-fields-test
  (let [entity-test1 (CommonEntityInfo. :test1 [])
        entity-test2 (CommonEntityInfo. :test2 [form-field1 column-field1])
        entity-test3 (CommonEntityInfo. :test3
                       [sql-field1 form-field1 sql-field2 column-field1
                        sql-field3])
        test3-answer (map #(.sql-field %) [sql-field1 sql-field2 sql-field3])]
    (is (= [] (.sql-fields entity-test1))
      "Should return empty vector for empty EntityInfo")
    (is (= [] (.sql-fields entity-test2))
      "Should return empty vector for EntityInfo without sql-typed FieldInfo")
    (is (= test3-answer (.sql-fields entity-test3))
      "Should return vector of sql-typed FieldInfo from EntityInfo")))

(deftest entity-info-form-fields-test
  (let [entity-test1 (CommonEntityInfo. :test1 [])
        entity-test2 (CommonEntityInfo. :test2 [sql-field1 column-field1])
        entity-test3 (CommonEntityInfo. :test3
                       [form-field1 sql-field1 form-field2 column-field1
                        form-field3])
        entity-test4 (CommonEntityInfo. :test4
                       [form-field3 sql-field1 form-field1 column-field1
                        form-field2])
        test-answer [form-field1 form-field2 form-field3]]
    (is (= [] (.form-fields entity-test1))
      "Should return empty vector for empty EntityInfo")
    (is (= [] (.form-fields entity-test2))
      "Should return empty vector for EntityInfo without form-field-typed
      FieldInfo")
    (is (= test-answer (.form-fields entity-test3))
      "Should return sorted by field-order vector of form-field-typed FieldInfo
      from EntityInfo")
    (is (= test-answer (.form-fields entity-test4))
      "Should return sorted by field-order vector of form-field-typed FieldInfo
      from EntityInfo")))

(deftest entity-info-column-fields-test
  (let [entity-test1 (CommonEntityInfo. :test1 [])
        entity-test2 (CommonEntityInfo. :test2 [sql-field1 form-field1])
        entity-test3 (CommonEntityInfo. :test3
                       [form-field1 sql-field1 column-field1 column-field2
                        column-field3])
        entity-test4 (CommonEntityInfo. :test4
                       [column-field3 form-field1 column-field1 sql-field3
                        form-field2 column-field2])
        test-answer [column-field1 column-field2 column-field3]]
    (is (= [] (.columns entity-test1))
      "Should return empty vector for empty EntityInfo")
    (is (= [] (.columns entity-test2))
      "Should return empty vector for EntityInfo without column-typed
      FieldInfo")
    (is (= test-answer (.columns entity-test3))
      "Should return sorted by column-order vector of column-typed FieldInfo
      from EntityInfo")
    (is (= test-answer (.columns entity-test4))
      "Should return sorted by column-order vector of column-typed FieldInfo
      from EntityInfo")))

(deftest entity-info-table-test
  (is (= :test (.table (CommonEntityInfo. :test [])))
    "Should return name of EntityInfo"))

(deftest entity-info-default-entity-test
  (let [entity-test1 (CommonEntityInfo. :test1 [])
        entity-test2 (CommonEntityInfo. :test2 [sql-field1 column-field1])
        entity-test3 (CommonEntityInfo. :test3
                       [form-field1 sql-field1 form-field2 column-field1
                        form-field3])
        test3-answer {:name 0 :duration 0 :width 0}]
    (is (nil? (.default-entity entity-test1))
      "Should return nil for empty EntityInfo")
    (is (nil? (.default-entity entity-test2))
      "Should return nil for EntityInfo without form-field-typed
      FieldInfo")
    (is (= test3-answer (.default-entity entity-test3))
      "Should return map with form-field-typed FieldInfo keys from
      EntityInfo and default values")))

(deftest entity-info-sql-table-info-test
  (let [entity-test1 (CommonEntityInfo. :test1 [])
        entity-test2 (CommonEntityInfo. :test2 [sql-field1 sql-field2])
        entity-test3 (CommonEntityInfo. :test3 [sql-field3 form-field1
                                                 column-field3 sql-field1])]
    (is (= '(:test1) (.sql-table-info entity-test1))
      "Should return table name only for empty field-info")
    (is (= '(:test2 [:name :sql] [:duration :sql])
          (.sql-table-info entity-test2))
      "Should process sql-typed FieldInfo only fields-info correctly")
    (is (= '(:test3 [:width :sql] [:name :sql]) (.sql-table-info entity-test3))
      "Should process mixed fields-info correctly")))

(deftest make-entity-info-test
  (let [fields-info1 [sql-field1 form-field2 column-field3]
        fields-info2 [sql-field1 form-field1]
        fields-info3 [form-field1 column-field3 form-field1 sql-field3
                      form-field2]
        fields-info4 [column-field3 form-field2 sql-field3 column-field1
                      column-field1]]
    (is (thrown? IllegalArgumentException
          (make-entity-info "test1" []))
      "Should throw IllegalArgumentException, when fields-info is empty")
    (is (= (CommonEntityInfo. "test2" fields-info1)
          (make-entity-info "test2" fields-info1))
      "Should create CommonEntityInfo, when fields-info is correct")
    (is (thrown? IllegalArgumentException
          (make-entity-info "test3" fields-info2))
      "Should throw IllegalArgumentException, when fields-info contains
      different form-field-typed of sql-typed FieldInfo with same sql-name")
    (is (thrown? IllegalArgumentException
          (make-entity-info "test4" [fields-info3]))
      "Should throw IllegalArgumentException, when fields-info contains
      different form-field-typed FieldInfo with same field-order")
    (is (thrown? IllegalArgumentException
          (make-entity-info "test5" [fields-info4]))
      "Should throw IllegalArgumentException, when fields-info contains
      different column-typed FieldInfo with same column-order")))

(deftest make-domain-info-test
  (let [entity-info1 (CommonEntityInfo. :table1 [])
        entity-info2 (CommonEntityInfo. :table2 [])]
    (is (thrown? IllegalArgumentException
          (make-domain-info []))
      "Should throw IllegalArgumentException, when entities-info is empty")
    (is (= (CommonDomainInfo. [entity-info1 entity-info2])
          (make-domain-info [entity-info1 entity-info2]))
      "Should create CommonDomainInfo, when entities-info is correct")
    (is (thrown? IllegalArgumentException
          (make-domain-info [entity-info1 entity-info1]))
      "Should throw IllegalArgumentException, when entities-info contains
      different EntityInfo with same table")))

(run-tests 'video_guide.info_test)
