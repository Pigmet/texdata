# texdata

A Clojure library designed to produce Tex documents. 

## Usage

This library offers a few functions that convert Clojure data to string recognizable by TeX. tex is such a function and the below is a simple examples:

```clojure
(tex 1)
;; => "1"

(tex "a")
;; => "a"

(tex :sin)
;; => "\\sin"

(tex [:frac 1 2])
;; => "\\frac{1}{2}"

(tex [:equation [:frac 1 2]])
;; => "\\begin{equation} \\frac{1}{2} \\end{equation}"
```
When it is given string or number, it justs returns the string representation of the argument. When it is given a keyword, it converts it a TeX symbol.

When it is given a vector beginning with a keyword that specifies a Tex command (like :frac in the last example), it returns a string corresponding to that TeX command. More precisely, data like

[keyword arg1 arg2 ...]

 are treated in a similar way to the S-expression in Lisp:

(function arg1 arg2 ...)


## License

Copyright © 2020 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
