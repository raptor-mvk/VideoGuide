(ns video_guide.core
  (:gen-class)
  (:require [video_guide.db.main :refer :all]))

(defn -main
  [& args]
  (println ("create table film (name text)")))
