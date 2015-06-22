(ns
  ^{:author raptor_MVK}
  video_guide.ext)

(defn runmap
  "Shortcut for (dorun (map...))"
  [& args]
  (dorun (apply map args)))

(defn domap
  "Shortcut for (doall (map...))"
  [& args]
  (doall (apply map args)))

