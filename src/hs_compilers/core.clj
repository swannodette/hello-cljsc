(ns hs-compilers.core
  (:require
    [clojure.pprint :as pp]
    [clojure.java.io :as io]
    [clojure.tools.reader :as reader]
    [clojure.tools.reader.reader-types :as readers]
    [cljs.analyzer :as ana]
    [cljs.compiler :as c])
  (:import [java.io StringReader]))

;; ==============================================================================
;; Reading

(defn string-reader [s]
  (clojure.lang.LineNumberingPushbackReader.
    (java.io.StringReader. s)))

(defn forms-seq [stream]
  (let [rdr (readers/indexing-push-back-reader stream 1)
        forms-seq* (fn forms-seq* []
                      (lazy-seq
                        (if-let [form (reader/read rdr nil nil)]
                          (cons form (forms-seq*)))))]
    (forms-seq*)))

(comment
  (first (forms-seq (string-reader "(+ x y)")))

  (first (forms-seq (string-reader "(fn [x y]\n(+ a b))")))

  (meta (first (forms-seq (string-reader "(fn [x y]\n(+ a b))"))))

  (meta (second (first (forms-seq (string-reader "(fn [x y]\n(+ a b))")))))

  (meta (first (rest (rest (first (forms-seq (string-reader "(fn [x y]\n(+ a b))")))))))
  )

