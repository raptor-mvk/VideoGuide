(ns
  ^{:author raptor_MVK}
  video_guide.db.main_test
  (:require [clojure.test :refer :all]
            [clojure.java.jdbc :refer :all]
            [video_guide.ext :refer :all]
            [video_guide.db.main :refer :all]
            [video_guide.info :refer :all]
            [clojure.java.io :refer :all])
  (:import (video_guide.info CommonEntityInfo CommonDomainInfo)))

(declare get-db-connection prepare-db clean-up-files)

(defrecord SQLTextFieldInfo [name]
  FieldInfo
  (field [_] name)
  (label [_] name)
  (getter [_] true)
  (default [_] 0)
  (column [_] name)
  (sql-field [_] [name :text])
  (sql-name [_] name)
  (field-order [_] 0)
  (column-order [_] 0)
  (field? [_] false)
  (column? [_] false)
  (sql? [_] true))

(defrecord SQLNumFieldInfo [name]
  FieldInfo
  (field [_] name)
  (label [_] name)
  (getter [_] true)
  (default [_] 0)
  (column [_] name)
  (sql-field [_] [name :int])
  (sql-name [_] name)
  (field-order [_] 0)
  (column-order [_] 0)
  (field? [_] false)
  (column? [_] false)
  (sql? [_] true))

(deftest get-db-metadata-test
  (try
    (let [test1-db-connection (get-db-connection "test1.db")
          test2-db-connection (get-db-connection "test2.db")
          test3-db-connection (get-db-connection "test3.db")]
      (prepare-db test2-db-connection
        ["create table film (name text, duration int);"])
      (prepare-db test3-db-connection
        ["create table book (name text, pages int, author int);"]
        ["create table author (name text, surname text);"])
      (is (= '() (get-db-metadata test1-db-connection))
        "Should return empty list for empty database")
      (is (= '((:film [:name :text] [:duration :int]))
            (get-db-metadata test2-db-connection))
        "Should process one-table database correctly")
      (is (= '((:book [:name :text] [:pages :int] [:author :int])
                (:author [:name :text] [:surname :text]))
            (get-db-metadata test3-db-connection))
        "Should process two-table database correctly"))
    (finally (clean-up-files "test1.db" "test2.db" "test3.db"))))

(deftest check-db-test
  (let [domain-test1 (CommonDomainInfo. [])
        domain-test2 (CommonDomainInfo. [(CommonEntityInfo.
                                           :film [(SQLTextFieldInfo. :name)])])
        domain-test3 (CommonDomainInfo. [(CommonEntityInfo.
                                           :film [(SQLTextFieldInfo. :name)])
                                         (CommonEntityInfo.
                                           :disc [(SQLNumFieldInfo. :size)])])
        domain-test4 (CommonDomainInfo.
                       [(CommonEntityInfo.
                          :film [(SQLTextFieldInfo. :name)
                                 (SQLNumFieldInfo. :duration)])])
        db-metadata1 '((:film [:name :text]))
        db-metadata2 '((:film [:name :text]) (:disc [:size :int]))
        db-metadata3 '((:film [:name :text] [:duration :int]))
        db-metadata4 '((:film [:duration :int] [:name :text]))
        db-metadata5 '((:disc [:size :int]) (:film [:name :text]))]
    (is (check-db domain-test1 '())
      "Should return true for empty domain-info and empty database metadata")
    (is (not (check-db domain-test1 db-metadata1))
      "Should return false for empty domain-info and non-empty database
      metadata")
    (is (check-db domain-test2 '())
      "Should return true for non-empty domain-info and empty database
      metadata")
    (is (check-db domain-test2 db-metadata1)
      "Should return true for corresponding domain-info and database metadata")
    (is (not (check-db domain-test2 db-metadata2))
      "Should return false for database metadata, containing extra tables")
    (is (check-db domain-test3 db-metadata1)
      "Should return true for domain-info, containing extra tables")
    (is (check-db domain-test4 db-metadata1)
      "Should return true for domain-info, containing extra columns")
    (is (not (check-db domain-test2 db-metadata3))
      "Should return false for database metadata, containing extra columns")
    (is (not (check-db domain-test3 db-metadata5))
      "Should return false for domain-info, containing tables in wrong order")))

(defn- get-db-connection
  "Given filename, returns database connection"
  [filename]
  {:classname "org.sqlite.JDBC",
   :subprotocol "sqlite",
   :subname filename})

(defn- prepare-db
  "Given database connection and queries list, executes queries consequently"
  [db & queries]
  (runmap #(execute! db %) queries))

(defn- clean-up-files
  "Given filenames list, deletes files from the list"
  [& filenames]
  (runmap delete-file filenames))

(run-tests 'video_guide.db.main_test)

