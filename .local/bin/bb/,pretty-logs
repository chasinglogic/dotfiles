#!/usr/bin/env bb
; vi: ft=clojure

(require '[cheshire.core :as json]
         '[clojure.string :as str])

;; --- ANSI Color Codes ---
(def colors
  {:reset   "\u001b[0m"
   :red     "\u001b[31m"
   :yellow  "\u001b[33m"
   :cyan    "\u001b[36m"
   :bold    "\u001b[1m"
   :underline "\u001b[4m"})

(def color-for-level
  (memoize
   (fn [level]
     (case (str/upper-case (name level))
       ("INFO") (:cyan colors)
       ("WARN" "WARNING") (:yellow colors)
       ("ERR" "ERROR") (:red colors)
       (:reset colors)))))

(def timestamp-keys [:time :timestamp :ts])
(def level-keys [:level :levelname :level_name])
(def message-keys [:message :msg])

(defn cache-forever [f]
  (let [mem (atom {})
        cache-key (-> f meta :name)]
    (fn [& args]
      (or (when-let [cached (get @mem cache-key)]
            cached)
          (let [ret (apply f args)]
            (swap! mem assoc cache-key ret)
            ret)))))

(defn- discover-key [possible-keys data]
  (when-let [found-key (some #(when (apply % [data]) %) possible-keys)]
    found-key))

(defn timestamp-key* [data]
  (discover-key timestamp-keys data))

(defn level-key* [data]
  (discover-key level-keys data))

(defn message-key* [data]
  (discover-key message-keys data))

(def timestamp-key (cache-forever timestamp-key*))
(def level-key (cache-forever level-key*))
(def message-key (cache-forever message-key*))
(def exclude-list (set (filter some? (concat timestamp-keys level-keys message-keys))))

(declare print-fields)

(defn- print-nested [prefix key value nesting-level]
  (println (str prefix key " = {"))
  (print-fields value (inc nesting-level))
  (println (str prefix "}")))

(defn- print-fields [data nesting-level]
  (let [prefix (apply str (repeat nesting-level "\t"))]
    (doseq [[key val] data]
      (when-not (contains? exclude-list key)
        (cond
          (map? val) (print-nested prefix key val nesting-level)
          (vector? val) (print-nested prefix key (into {} (map-indexed vector val)) nesting-level)
          :else (println (str prefix key " = " val)))))))

(defn pretty-print [line]
  (let [data (json/parse-string line true)
        level (get data (level-key data) :INFO)
        timestamp (get data (timestamp-key data) "")
        msg (get data (message-key data) "")]

    (println (format "%s%s%s %s %s"
                     (color-for-level level)
                     (str/upper-case (name level))
                     (:reset colors)
                     timestamp
                     msg))
    (print-fields data 1)))

(defn main []
  (with-open [reader (java.io.BufferedReader. *in*)]
    (loop [line (.readLine reader)]
      (when line
        (try
          (pretty-print line)
          (catch Exception _e
            (println "NON-JSON LINE:" line)))
        (recur (.readLine reader))))))

(main)
