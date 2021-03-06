(ns
  ^{:author raptor_MVK}
  video_guide.db.main
  (:require [clojure.java.jdbc :refer :all]
            [video_guide.ext :refer :all])
  (:import com.jolbox.bonecp.BoneCPDataSource))

(declare check-table parse-tables query-tables dbcpool)

(defn check-db
  "Given domain-info and database metadata, return true, if database
  corresponds to domain-info or could be updated to it"
  [domain-info db-metadata]
  (let [db-info (.sql-db-info domain-info)]
    (if (>= (count db-info) (count db-metadata))
      (reduce #(and %1 %2) true (map check-table db-info db-metadata))
      false)))

(defn- check-table
  "Given entity-info and table metadata, returns true, if table corresponds
   to entity-info or could be updated to it"
  [entity-info table-metadata]
  (if (>= (count entity-info) (count table-metadata))
    (reduce #(and %1 %2) true (map #(= %1 %2) entity-info table-metadata))
    false))

(defn get-db-metadata
  "Given database connection, returns database metadata"
  [db]
  (map #(parse-tables (:sql %)) (query-tables db)))

(defn- parse-tables
  "Given 'create table' query, returns parsed table metadata"
  [s]
  (let [words (clojure.string/split s #" |\(|\)|,")
        sql-meta (domap keyword (drop 2 (filter #(> (.length %) 0) words)))]
    (conj (map vec (partition 2 (rest sql-meta))) (first sql-meta))))

(defn- query-tables
  "Given database connection, returns list of 'create table' queries"
  [db]
  (query db "select sql from sqlite_master order by rowid;"))

(defn dbcpool
  "Given database filename, creates and returns connection pool data source"
  [filename]
  (let [cpds (doto (BoneCPDataSource.)
               (.setJdbcUrl (str "jdbc:sqlite:" filename))
               (.setIdleConnectionTestPeriodInMinutes 30)
               (.setConnectionTestStatement "select 1"))]
    {:datasource cpds}))
