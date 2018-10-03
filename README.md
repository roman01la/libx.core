# libx.core
Enhanced Clojure's standard library

Try it in REPL:
```sh
clj -Sdeps '{:deps {github-roman01la/libx {:git/url "https://github.com/roman01la/libx.core" :sha "c46fcb64fda074bcf4fa9116ccbe17efd073e1c8"}}}'
```

## Features

### Lightweight `doseq`
Expands into `loop` for simple form `(doseq [x coll] ...)`
- [ ] Unroll into a sequence of expressions for constant `coll` values

### Lightweight `for`
Expands into `loop` returning `lazy-seq` for simple form `(for [x coll] ...)`
- [ ] Unroll into a sequence of expressions for constant `coll` values

### Collection transforms fusion in `->>` macro
- [x] 2+ adjacent `map`
- [x] 2+ adjacent `mapv`
- [x] 2+ adjacent `filter`
- [x] 2+ adjacent `filterv`
- [x] adjacent `map` and `filter`, `mapv` and `filterv`
- [ ] arbitrary mixed `map/v` and `filter/v`

### `if-keys` & `when-keys`
When all symbols in destructuring form are evaluated to `true`, evaluates body with those symbols bound to corresponding values in a map.
  
```clojure
(when-keys [{:keys [a b]} {:a 1 :b 2}]
  (println a b))
```

### `let` with JavaScript object destructuring
```clojure
(require '[goog.object :as obj])

(let [{:goog.object/keys [x y]} #js {:x 1 :y 2}
      {::obj/keys [z]} #js {:z 3}] ;; <- with namespace aliased namespaced keyword
  ...)
```
