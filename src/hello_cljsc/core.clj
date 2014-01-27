;; The ClojureScript analyzer and compiler can be easily explored from Clojure.
;; This tutorial was designed with Light Table in mind. Evaluate the forms in
;; this file one by one by placing your cursor after each expression and pressing
;; Command-ENTER. Start with evaluating this namespace expression.

(ns hello-cljsc.core
  (:require
    [clojure.pprint :as pp]
    [clojure.tools.reader :as reader]
    [clojure.tools.reader.reader-types :as readers]
    [cljs.analyzer :as ana]
    [cljs.compiler :as c]
    [cljs.closure :as cc]
    [cljs.env :as env])
  (:import [java.io StringReader]))

;; ==============================================================================
;; Utilities

;; First, we define a series of utility helper functions which will simplify
;; our interactions with the ClojureScript analyzer and compiler.

;; A simple helper to emit ClojureScript compiled to JavaScript
;; as a string.
(defn emit-str [ast]
  (with-out-str (c/emit ast)))

;; A simple helper which allows us to read ClojureScript source from a string
;; instead of having to bother with files.
(defn string-reader [s]
  (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. s)))

;; A simple helper that takes a stream and returns a lazy sequences of
;; read forms.
(defn forms-seq [stream]
  (let [rdr (readers/indexing-push-back-reader stream 1)
        forms-seq* (fn forms-seq* []
                      (lazy-seq
                        (if-let [form (reader/read rdr nil nil)]
                          (cons form (forms-seq*)))))]
    (forms-seq*)))

;; ==============================================================================
;; Reading

;; What other languages call "parsing", Clojure and ClojureScript (like Lisps
;; before them) call "reading". Reading a string will result in Clojure
;; data structures. These data structures just happen to represent source code!

;; Getting a seq of s-expressions.
(forms-seq (string-reader "(+ 1 2)"))

;; Evaluate the following expressions.

;; form-seq will return a seq containing two forms.
(forms-seq (string-reader "(+ 1 2) (+ 3 4)"))

;; The first form is (+ 1 2)
(first (forms-seq (string-reader "(+ 1 2) (+ 3 4 )")))

;; The first form is a list.
(first (forms-seq (string-reader "(fn [x y]\n(+ x y))")))

;; The first form in (fn [x y] (+ x y)) is a symbol
(ffirst (forms-seq (string-reader "(fn [x y]\n(+ x y))")))

;; The second form in (fn [x y] (+ x y)) is a vector
(second (first (forms-seq (string-reader "(fn [x y]\n(+ x y))"))))

;; The reader will annotate the data structure, via metadata, with source line
;; and column information. The presence of this information enables accurate
;; source mapping.

;; On what line and column did we read (fn [x y] (+ x y)) ?
(meta (first (forms-seq (string-reader "(fn [x y]\n(+ x y))"))))

;; On what line and column did we read [x y] ?
(-> (string-reader "(fn [x y]\n(+ x y))")
  forms-seq
  first
  second
  meta)

;; On what line and column did we read (+ x y) ?
(-> (forms-seq (string-reader "(fn [x y]\n(+ x y))"))
  first
  rest
  rest
  first
  meta)

;; =============================================================================
;; Analyzing

;; Lisp forms, while adequate for many kinds of user-level syntax manipulation,
;; aren't quite rich enough for actually running programs. Thus, we'll want to
;; generate an Abstract Syntax Tree (AST) from the forms we have read.

;; First we need to setup a basic analyzer environment.
(def user-env '{:ns {:name cljs.user} :locals {}})

;; A helper to just read the first s-expression
(defn read1 [str]
  (first (forms-seq (string-reader str))))

(read1 "[1 2 3]")

;; cljs.analyzer/analyze takes an analyzer environment and a form. It will
;; return a ClojureScript AST node. ClojureScript AST nodes are represented as
;; simple maps. For the following part open the console in a tab so it's easier
;; to view the pretty printed output.

;; This will pretty print a :vector AST node. Click the highlighted number in
;; the lower right corner to bring up the console to see the output.
(let [form (read1 "[1 2 3]")]
  (pp/pprint (ana/analyze user-env form)))

;; This will pretty print an :invoke AST node.
(let [form (read1 "(foo 1)")]
  (pp/pprint (ana/analyze user-env form)))

;; Before moving any further let's review the steps to go from a string to a
;; ClojureScript AST node.

;; First we read a string, converting text into forms.
(read1 "(if x true false)")

;; The very first element in the form (if x true false) is a symbol
(first (read1 "(if x true false)"))

;; In Lisp source code, the first element of an s-expression (form) like
;; (foo 1 2) is extremely important. The first element determines whether it
;; is a special form as in the case of (if x true false), a macro as in the
;; case of (and true false), or a function call as in the case of
;; (first '(1 2 3)).

;; Special forms are actually handled by the compiler. Macros allows users
;; to extend the language without needing to be a Lisp compiler hacker.
;; Macros will desugar into special forms.

;; When the ClojureScript compiler encounters an s-expression that
;; starts with a special form, it calls the cljs.analyer/parse multimethod.
(let [form (read1 "(if x true false)")]
  (pp/pprint (ana/parse (first form) user-env form nil)))

;; The following is copied and pasted from analyzer.clj
;;
;; (defmethod parse 'if
;;   [op env [_ test then else :as form] name]
;;   (when (< (count form) 3)
;;     (throw (error env "Too few arguments to if")))
;;   (let [test-expr (disallowing-recur (analyze (assoc env :context :expr) test))
;;         then-expr (analyze env then)
;;         else-expr (analyze env else)]
;;     {:env env
;;      :op :if
;;      :form form
;;      :test test-expr
;;      :then then-expr
;;      :else else-expr
;;      :unchecked @*unchecked-if*
;;      :children [test-expr then-expr else-expr]}))

;; cljs.analyzer/analyze delegates to cljs.analyzer/parse
(let [form (read1 "(if x true false)")]
  (pp/pprint (ana/analyze user-env form)))

;; =============================================================================
;; Compiling

;; Once we have an AST node, compilation is relatively straightforward.

;; To compile an AST node to JavaScript we just call cljs.compiler/emit
;; with an AST node as the argument. Click on the output box to expand it.
(let [form (read1 "(if x true false)")]
  (with-out-str (c/emit (ana/analyze user-env form))))

;; Pretty simple! try different things!
(let [form (read1 "(fn [a b] (+ a b))")]
  (with-out-str (c/emit (ana/analyze user-env form))))

;; =============================================================================
;; Using analysis

;; The ClojureScript compiler generally discards most of the AST once it has
;; emitted JavaScript. However for optimizations, error checking, and supporting
;; external tools, the ClojureScript compiler needs to preserve information about
;; top level definitions encountered in a namespace. This is accomplished by
;; writing into an atom that represents the compilation environment.

;; We define a compilation environment to store analyzer information.
(def cenv (atom {}))

;; If we want to record analyzer information we need to wrap our analyze
;; calls with cljs.env/with-compiler-env
(let [form (read1 "(def x :foo)")]
  (env/with-compiler-env cenv
    (ana/analyze user-env form)))

;; Now if we look at the contents of cenv we'll see that we have a single def for x.
@cenv

;; Let's analyze a top level function.
(let [form (read1 "(defn foo [a b] (+ a b))")]
  (env/with-compiler-env cenv
    (ana/analyze user-env form)))

;; Let's look at just the information for cljs.user/foo. You will see that the
;; analyzer saves quite a bit more information for functions. This is useful
;; for optimizations!
(get-in @cenv [::ana/namespaces 'cljs.user :defs 'foo])

;; Let's redefine foo, this time with two arities.
(let [form (read1 "(defn foo ([a] a) ([a b] (+ a b)))")]
  (env/with-compiler-env cenv
    (c/emit (ana/analyze user-env form))))

;; When you evaluate this, notice that the generated JavaScript is suboptimal.
;; First, it invokes cljs.user/foo through JavaScript's Function call method, which
;; will be slower on most engines. Also, by going through call, we will need to
;; examine the arguments objects to determine which arity to invoke, another
;; performance hit. This seems a bit silly given that we saw above that the analyzer
;; records enough information to optimize this case.
(let [form (read1 "(defn bar [] (foo 1))")]
  (emit-str
    (env/with-compiler-env cenv
      (ana/analyze user-env form))))

;; And, in fact, ClojureScript does optimize this case under the :advanced
;; compilation mode. You can get the same behavior by dynamically binding
;; cljs.analyzer/*cljs-static-fns* to true.
(let [form (read1 "(defn bar [] (foo 1))")]
  (binding [ana/*cljs-static-fns* true]
    (emit-str
      (env/with-compiler-env cenv
        (ana/analyze user-env form)))))

;; =============================================================================
;; That's it!

;; You now have a basic understanding of the role of the ClojureScript analyzer
;; and the compiler. Most of language "features" you find in ClojureScript are
;; actually provided via macros. In fact if you look at cljs/core.clj you'll see
;; there's as much code in the macro file as there is in the analyzer file or
;; the compiler file!

;; Everything after this point is just icing on the cake!

;; =============================================================================
;; Macros

;; Macros allow us to eliminate a considerable amount of complexity from the
;; ClojureScript analyzer and compiler. They also allow us to implement
;; simple optimizations without relying on special support from the compiler.

;; As it turns out, and is just a macro over let + if!
(let [form (read1 "(and true (diverge))")]
  (ana/macroexpand-1 user-env form))

;; Arithmetic operations in ClojureScript are functions, not operators. However,
;; if we did not optimize simple cases, ClojureScript performance would suffer
;; as the core data structures rely on the presence of fast arithmetic. Notice
;; the generated JavaScript doesn't involve any function calls. This is because
;; addition is implemented as both a ClojureScript function and an optimizing
;; inlining macro!
(let [form (read1 "(+ 1 2 3 4 5 6)")]
  (emit-str (ana/analyze user-env form)))

;; However, if the addition function appears in some other location than the
;; first element of a form, we'll use the ClojureScript addition function instead.
(let [form (read1 "(apply + [1 2 3 4 5 6])")]
  (emit-str (ana/analyze user-env form)))

;; Fast bit operations are also critical for ClojureScript data structure
;; performance. Again, we generate pretty much what you expect.
(let [form (read1 "(+ 1 (bit-shift-left 16 1))")]
  (emit-str (ana/analyze user-env form)))

;; Array operations are also critical for ClojureScript data structure
;; performance and are inlined when possible.
(let [form (read1 "(let [arr (array)] (aset arr 0 100))")]
  (emit-str (ana/analyze user-env form)))

;; =============================================================================
;; Type Inference

;; The ClojureScript compiler has some simple type inference to aid with both
;; performance and rudimentary type checking.

(let [form (read1 "(let [x true] true)")]
  (ana/infer-tag user-env (ana/analyze user-env form)))

;; Thus, in some cases we can statically infer that we don't need truth tests.
(let [form (read1 "(if (and true false) true false)")]
  (emit-str (ana/analyze user-env form)))

;; However, consider that in ClojureScript 0 is not false-y so we must include an
;; extra function call to check. In most cases this not a significant
;; performance hit but for tight loops this may be problematic. You might also
;; notice that there's an extraneous function call here. This is to preserve
;; expression semantics. We rely on Google Closure to optimize cases like this, as
;; you'll see below.
(let [form (read1 "(if (and 0 false) true false)")]
  (emit-str (ana/analyze user-env form)))

;; =============================================================================
;; Google Closure

;; Google Closure applies many more optimizations beyond what the ClojureScript
;; compiler applies. Google Closure will remove superfluous functions, inline
;; functions, fold constants, remove dead code, and minify the final source.
;; Evaluate the following.
(let [form (read1 "(let [x (cond true (+ 1 2) :else (+ 3 4))] x)")]
  (emit-str (ana/analyze user-env form)))

;; The same expression with simple optimizations is surprisingly terse.
(let [form (read1 "(let [x (cond true (+ 1 2) :else (+ 3 4))] x)")]
  (cc/optimize {:optimizations :simple}
    (emit-str (ana/analyze user-env form))))
