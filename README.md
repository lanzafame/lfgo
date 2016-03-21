# LFGo: Lisp-Flavoured Go

(C) 2016 Kim, Taegyoon

LFGo is a programming language that transcompiles to Go. It uses Lisp-like syntax.

## Usage ##
```
#!sh

$ racket l++.rkt -h
l++.rkt [ <option> ... ] [<filenames>] ...
 where <option> is one of
  -c, --compile : Compile only; do not run
  -v, --verbose : Display verbose messages
  -o <file> : Place the output into <file>
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
```

## Syntax and semantics ##

See [the source code](https://bitbucket.org/ktg/lfgo/src) for details.

### Comments ###
`;` `#!` end-of-line comment

`#|` nestable block comment `|#`

`#;` S-expression comment

See [Reading Comments](http://docs.racket-lang.org/reference/reader.html?q=%23%7C&q=comment#%28part._parse-comment%29).

### Macros ###
Macros are supported via [Racket's macro system](http://docs.racket-lang.org/guide/macros.html) [`define-syntax`](http://docs.racket-lang.org/reference/define.html?q=define-syntax#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._define-syntax%29%29), [`define-syntax-rule`](http://docs.racket-lang.org/search/index.html?q=define-syntax-rule&q=define-syntax-rule&q=set-add%21&q=define-syntax&q=set&q=append&q=list-append&q=for&q=define-syntax) and [`defmacro`](http://docs.racket-lang.org/compatibility/defmacro.html).

## Examples ##
### Hello, World! ###
```
(import "fmt")
(main
  (fmt.Println "Hello, World!"))
```

Run with

```
#!sh

$ racket lfgo.rkt ex/hello.lfgo
Hello, World!
```

### Other examples ###

Other examples are in the [`ex` directory](https://bitbucket.org/ktg/lfgo/src).

### [99 Bottles of Beer](http://en.wikipedia.org/wiki/99_Bottles_of_Beer) ###
```
(import "fmt")
(main
  (for (def i 99) (>= i 1) (-- i)
    (fmt.Println i "bottles of beer on the wall," i "bottles of beer.")
    (fmt.Println "Take one down and pass it around," (- i 1) "bottle of beer on the wall.")))
```

## License ##

   Copyright 2016 Kim, Taegyoon

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

   [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
