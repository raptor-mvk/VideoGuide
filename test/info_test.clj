(ns
  ^{:author raptor_MVK}
  video_guide.info_test
  (:require [clojure.test :refer :all]
            [video_guide.ext :refer :all]
            [video_guide.info])
  (:import (video_guide.info FieldInfo CommonEntityInfo)))

(defrecord SQLFieldInfo [id]
  FieldInfo
  (field-order [_] 0)
  (column-order [_] 0)
  (field? [_] false)
  (column? [_] false)
  (sql? [_] true))

(defrecord FormFieldInfo [id field-order]
  FieldInfo
  (field-order [_] field-order)
  (column-order [_] 0)
  (field? [_] true)
  (column? [_] false)
  (sql? [_] false))

(defrecord ColumnFieldInfo [id column-order]
  FieldInfo
  (field-order [_] 0)
  (column-order [_] column-order)
  (field? [_] false)
  (column? [_] true)
  (sql? [_] false))

(def sql-field1 (SQLFieldInfo. 1))
(def sql-field2 (SQLFieldInfo. 2))
(def sql-field3 (SQLFieldInfo. 3))
(def form-field1 (FormFieldInfo. 4 1))
(def form-field2 (FormFieldInfo. 5 2))
(def form-field3 (FormFieldInfo. 6 3))
(def column-field1 (ColumnFieldInfo. 7 1))
(def column-field2 (ColumnFieldInfo. 8 2))
(def column-field3 (ColumnFieldInfo. 9 3))

(deftest entity-info-sql-fields-test
  (let [entity-test1 (CommonEntityInfo. "test1" [])
        entity-test2 (CommonEntityInfo. "test2" [form-field1 column-field1])
        entity-test3 (CommonEntityInfo. "test3"
                       [sql-field1 form-field1 sql-field2 column-field1
                        sql-field3])
        test3-answer [sql-field1 sql-field2 sql-field3]]
    (is (= [] (.sql-fields entity-test1))
      "Should return empty vector for empty EntityInfo")
    (is (= [] (.sql-fields entity-test2))
      "Should return empty vector for EntityInfo without sql-typed FieldInfo")
    (is (= test3-answer (.sql-fields entity-test3))
      "Should return vector of sql-typed FieldInfo from EntityInfo")))

(deftest entity-info-form-fields-test
  (let [entity-test1 (CommonEntityInfo. "test1" [])
        entity-test2 (CommonEntityInfo. "test2" [sql-field1 column-field1])
        entity-test3 (CommonEntityInfo. "test3"
                       [form-field1 sql-field1 form-field2 column-field1
                        form-field3])
        entity-test4 (CommonEntityInfo. "test4"
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
  (let [entity-test1 (CommonEntityInfo. "test1" [])
        entity-test2 (CommonEntityInfo. "test2" [sql-field1 form-field1])
        entity-test3 (CommonEntityInfo. "test3"
                       [form-field1 sql-field1 column-field1 column-field2
                        column-field3])
        entity-test4 (CommonEntityInfo. "test4"
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

(run-tests 'video_guide.info_test)
