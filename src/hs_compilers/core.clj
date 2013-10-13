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
  (forms-seq (string-reader "(+ 1 2)"))
  
  (first (forms-seq (string-reader "(+ 1 2)")))

  (first (forms-seq (string-reader "(fn [x y]\n(+ x y))")))

  (meta (first (forms-seq (string-reader "(fn [x y]\n(+ x y))"))))

  (meta (second (first (forms-seq (string-reader "(fn [x y]\n(+ x y))")))))

  (meta (first (rest (rest (first (forms-seq (string-reader "(fn [x y]\n(+ x y))")))))))
  )

;; =============================================================================
;; Analyzing

(def user-env '{:ns {:name cljs.user} :locals {}})

(defn read1 [str]
  (first (forms-seq (string-reader str))))

(comment
  (read1 "(if x true false)")

  (first (read1 "(if x true false)"))

  (let [form (read1 "(if x true false)")]
    (pp/pprint (ana/analyze user-env form)))
  )

;; =============================================================================
;; Compiling

(comment
  (let [form (read1 "(if x true false)")]
    (c/emit (ana/analyze user-env form)))

  (let [form (read1 "(fn [a b] (+ a b))")]
    (c/emit (ana/analyze user-env form)))
  )

;; =============================================================================
;; Expressions

(comment
  (let [form (read1 "(if (let [x true] x) true false)")]
    (c/emit (ana/analyze user-env form)))
  )

;; =============================================================================
;; Macros

(comment
  (read1 "(and true (diverge))")

  (let [form (read1 "(and true (diverge))")]
    (pp/pprint (ana/macroexpand-1 user-env form)))

  (let [form (read1 "(+ 1 2 3 4 5 6)")]
    (c/emit (ana/analyze user-env form)))

  (let [form (read1 "(apply + [1 2 3 4 5 6])")]
    (c/emit (ana/analyze user-env form)))

  (let [form (read1 "(+ 1 (bit-shift-left 16 1))")]
    (c/emit (ana/analyze user-env form)))
  )

;; =============================================================================
;; Type Inference

(comment
  (let [form (read1 "(let [x true] true)")]
    (c/infer-tag (ana/analyze user-env form)))

  ;; a bug!
  (let [form (read1 "(and true false)")]
    (c/infer-tag (ana/analyze user-env form)))
  )

;; =============================================================================
;; Using analysis

(comment
  (let [form (read1 "(def x :foo)")]
    (ana/analyze user-env form))

  (pp/pprint @ana/namespaces)

  (let [form (read1 "(defn foo [a b] (+ a b))")]
    (ana/analyze user-env form))

  (pp/pprint @ana/namespaces)

  (let [form (read1 "(defn foo ([a] a) ([a b] (+ a b)))")]
    (c/emit (ana/analyze user-env form)))

  (let [form (read1 "(defn bar [] (foo 1))")]
    (c/emit (ana/analyze user-env form)))
  
  (let [form (read1 "(defn bar [] (foo 1))")]
    (binding [ana/*cljs-static-fns* true]
      (c/emit (ana/analyze user-env form))))
  )
