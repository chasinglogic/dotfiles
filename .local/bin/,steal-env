#!/usr/bin/env bb
; vi: ft=clojure

(require '[babashka.process :refer [sh]]
         '[clojure.string :as str])

(defn- dump-vars [args]
  (->
   (sh (concat ["kubectl exec -it"] args ["--" "env"]))
   :out
   str/split-lines))

(def banned-predicates #{"KUBERNETES_" "HOME" "TERM" "PATH" "HOSTNAME"})

(defn- is-dumpable-var [env-var]
  (not (some #(str/starts-with? env-var %) banned-predicates)))

(defn- main [args]
  (doseq [env-var (dump-vars args)]
    (when (is-dumpable-var env-var)
      (println "export" env-var))))

(main *command-line-args*)
