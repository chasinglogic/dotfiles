#!/usr/bin/env bb
; vi: ft=clojure

(require '[babashka.cli :as cli])

(defn get-decoder [encoding]
  (case encoding
    :base64 (java.util.Base64/getDecoder)
    (throw (Exception. (format "%s is not a valid encoding" encoding)))))

(defn decode [encoding data]
  (let [decoder (get-decoder encoding)
        result-bytes (.decode decoder data)]
    (String. result-bytes)))

(def cli-spec
  {:help {:alias :h
          :coerce :boolean}
   :encoding {:ref "<encoding>"
              :coerce :keyword
              :desc "The encoding of the provided strings"
              :default-desc "base64"
              :default :base64}})

(defn- main [args]
  (let [{:keys [opts args]} (cli/parse-args args {:spec cli-spec})]
    (if (or (:help opts) (empty? args))
      (println (cli/format-opts {:spec cli-spec, :header "Usage: decode [opts] encoded_string [encoded_string...]\n\nOptions:"}))
      (doseq [arg args]
        (println (decode (:encoding opts) arg))))))

; (decode :base64 "YW1xcHM6Ly9hZG1pbjpQWTZRUElFTUdHRzNQTkhJSFNLMkpOR1IyNUB6aWEtcmFiYml0bXEuc3RhZ2luZy16aWEtcGxhdGZvcm0uc3ZjLmNsdXN0ZXIubG9jYWw6NTY3MQ==")
(main *command-line-args*)
