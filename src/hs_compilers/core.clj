(ns hs-compilers.core
  (:require
    [clojure.pprint :as pp]
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

  (type (first (forms-seq (string-reader "(fn [x y]\n(+ x y))"))))

  (meta (first (forms-seq (string-reader "(fn [x y]\n(+ x y))"))))

  (-> (string-reader "(fn [x y]\n(+ x y))")
    forms-seq
    first
    second
    meta)

  (-> (forms-seq (string-reader "(fn [x y]\n(+ x y))"))
    first
    rest
    rest
    first
    meta)
  )

;; =============================================================================
;; Analyzing

(def user-env '{:ns {:name cljs.user} :locals {}})

(defn read1 [str]
  (first (forms-seq (string-reader str))))

(comment
  (read1 "[1 2 3]")

  (type (read1 "[1 2 3]"))

  (let [form (read1 "[1 2 3]")]
    (pp/pprint (ana/analyze user-env form)))

  (let [form (read1 "(foo 1)")]
    (pp/pprint (ana/analyze user-env form)))

  (read1 "(if x true false)")

  (first (read1 "(if x true false)"))

  (let [form (read1 "(if x true false)")]
    (ana/parse (first form) user-env form nil))

  ;; copy and pasted from compiler.clj
  (defmethod parse 'if
    [op env [_ test then else :as form] name]
    (when (< (count form) 3)
      (throw (error env "Too few arguments to if")))
    (let [test-expr (disallowing-recur (analyze (assoc env :context :expr) test))
          then-expr (analyze env then)
          else-expr (analyze env else)]
      {:env env
       :op :if
       :form form
       :test test-expr
       :then then-expr
       :else else-expr
       :unchecked @*unchecked-if*
       :children [test-expr then-expr else-expr]}))
  
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

  (let [form (read1 "(let [arr (array)] (aset arr 0 100))")]
    (c/emit (ana/analyze user-env form)))
  )

;; =============================================================================
;; Type Inference

(comment
  (let [form (read1 "(let [x true] true)")]
    (c/infer-tag (ana/analyze user-env form)))

  ;; a bug! anyone want to help fix it?
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
