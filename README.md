# libx.core
Enhanced Clojure's standard library

Try it in REPL:
```sh
clj -Sdeps '{:deps {github-roman01la/libx {:git/url "https://github.com/roman01la/libx.core" :sha "c46fcb64fda074bcf4fa9116ccbe17efd073e1c8"}}}'
```

## Features

### Lightweight `doseq`
Expands into `loop` for simple form `(doseq [x coll] ...)`

### Lightweight `for`
Expands into `loop` returning `lazy-seq` for simple form `(for [x coll] ...)`

### Collection transforms fusion in `->>` macro
- [x] 2+ adjacent `map`
- [x] 2+ adjacent `mapv`
- [x] 2+ adjacent `filter`
- [x] 2+ adjacent `filterv`
- [x] adjacent `map` and `filter`
