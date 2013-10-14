# hs-compilers

Repo for the Hacker School talk tonight.

## Usage

Install [Leiningen](http://leiningen.org). In the project directory
run `lein repl` at a terminal which will start up a REPL. Then type
the following to switch into the correct namespace:

```
(require 'hs-compilers.core)
(in-ns 'hs-compilers.core)
```

You should now be able to the copy and paste the snippets of code in
`src/hs_compilers/core.clj` into the REPL.
