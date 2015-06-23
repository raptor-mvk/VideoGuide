(ns
  ^{:author raptor_MVK}
  video_guide.db.main_test
  (:require [clojure.test :refer :all]
            [clojure.java.jdbc :refer :all]
            [video_guide.ext :refer :all]
            [video_guide.db.main :refer :all]
            [clojure.java.io :refer :all]))

(declare get-db-connection prepare-db clean-up-files)

(deftest normalize-db-table-meta-test
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

