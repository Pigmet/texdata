# texdata

A Clojure library designed to produce Tex documents. 

## Usage

### quick start
Add this to the dependencies:

```clojure
[texdata "0.1.3-SNAPSHOT"]
```
For testing purposes, create test.tex file somewhere, let's say in the test directory of the project.

```clojure
(ns texdata-demo.core
  (:require [texdata.core :refer [tex compile-and-view]]))

(def test-path "test/texdata_demo/test.tex")

(compile-and-view
 test-path
 (tex [:documentclass "article"]
      [:document "hello world"]))
```	

### simple examples

This library offers a few functions that convert Clojure data to string recognizable by TeX. tex is such a function and the below are simple examples:

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
### How tex treats data

#### string and number

When it is given string or number, it just returns the string representation of the argument. In other words, tex is nothing else but clojure.core/str in this case.

### keyword 

tex treats a standalone keyword signifying a TeX symbol. Most keywords are converted to the corresponding TeX symbol when passed to tex, tex-> or tex->>.

```clojure
  (tex :sin)
  ;; => "\\sin"

  (tex :log)
  ;; => "\\log"
```

As you see above, tex converts a keyword k to string by just adding the escape character to the result of (name k).

There are some exceptions to this rule:

```clojure
  (tex :amp)
  ;; => "&"
  (tex :next)
  ;; => "\\\\"
```

### command 

When tex is given a vector beginning with a keyword that specifies a Tex command (like :frac in the last example), it returns a string corresponding to that TeX command. More precisely, a vector like

[keyword arg1 arg2 ...]

 is treated in a similar way to the S-expression in Lisp:

(function arg1 arg2 ...)

### subscripts and superscripts 

In TeX, superscripts and subscripts are written using the symbols ^ and _. To get the same result, use :super and :sub keywords respectively:

```clojure
(tex ["a" :super 2 :sub "n"])
  ;; => "a_{n}^{2}" 
```

### mathematical expressions
To put a mathematical expression in inline mode, use :dol like:

```clojure
  (tex [:dol ["x" :super 2] "+" ["y" :super 2] :eq ["z" :super 2]])
  ;; => "$ x^{2} + y^{2} = z^{2} $"  
```

To effect the display mode, use :math for unnumbered expressions and :equation
for numbered expressions:

```clojure
(tex [:math "E" :eq "mc"])
;; => "\\[ E = mc \\]"

(tex [:equation "E" :eq "mc"])
;; => "\\begin{equation} E = mc \\end{equation}"
```

### optional attributes

You can specify optional attributes in some commands. That is done by inserting a map that contains the desired data. :int is one of those commands that can takes such an optional map:

```clojure
(tex [:int "f(x)dx"])
;; => "\\int f(x)dx"

(tex [:int {:from 0 :to 1} "f(x)dx"])
;; => "\\int_{0}^{1} f(x)dx"

(tex [:int {:on "A"} "f(x)dx" ])
;; => "\\int_{A} f(x)dx"
```

### threading functions

tex-> and tex->> are functions somewhat similar to clojure.core/-> and
clojure.core/->> respectively. They are handy when composing commands. For example, 

```clojure
(tex-> "x" :equation :huge)
```

is same as

```clojure
(tex [:huge [:equation "x"]])
```

producing the result:

```clojure
"\\begin{huge} \\begin{equation} x \\end{equation} \\end{huge}"
```

tex->, like clojure.core/->, inserts the previous result at the second place of the next form and repeats the process. 

tex->> works likewise, but the previous result is inserted at the last of the next form, like clojure.core/->>. This is necessary when working with commands like :color, which takes a string specifying the color of the subsequent part as the first argument: 

```clojure
(tex [:color "red" [:equation "x = 1"]])
```
To obtain the same result with tex->>, we write:

```clojure
(tex->> "x = 1" :equation [:color "red"])
;; => "\\color{red}{\\begin{equation} x = 1 \\end{equation}}"
```

### querying examples

example is a function to obtain expected input examples for each command.

```clojure
(example :int)
;; => [:int {:from 0, :to 1} "f(x)" "dx"]
```
### defining new commands

There may be times when you want to add a new command. defcmd is a macro designed for that purpose. Its grammar is as follows:

```clojure
(defcmd command-keyword command-type &body)
```

Here, command-keyword is a keyword signifying the command to be defined. command-type is one of 

* :environment
* :normal
* :independent 

:environment commands are such ones as :equation, namely, they are commands sandwiched between \begin{...} and \end{...}. :normal commands are other commands that takes arguments (e.g., :frac, :color). :independent commands are specified by standalone keywords that are converted to TeX symbols. 

defcmd provides default implementation for :environment and :normal commands. Let's suppose you want to register a new environment command :hoge with the desired result:

```clojure
(tex [:hoge "hello"])
;; => "\\begin{hoge} hello \\end{hoge}"
```

Then, just evaluating 

```clojure
(defcmd :hoge :environment :default)
```

will do.

Other times, you may need to have more specific structure. Let's suppose that your new command :my-color takes a string of color as the first argument, followed by any number of arguments:

```clojure
(tex [:my-color "red" "hello"])
;; => "\\mycolor{red}{hello}"
```

Registering :my-color requires giving necessary details in defcmd:

```clojure
(defcmd :my-color :normal [[_ c & args]]
  (format "\\mycolor{%s}{%s}" c (tex args)))
```
When not using the :default keyword as explained above, the @body part of the defcmd is like that of defn. Let me note that its parameter is supposed to be a vector (not variable argument list ), whose first item is the command keyword (:my-color in the above example).

## full example

Here is a code that produces a complete TeX document:

```clojure

  (def dirac-delta
    (let [p1 (tex :delta "(x)"
                  :eq 0
                  :sp [:text "if" [:dol "x" :neq 0]])
          
          p2 (tex [:int {:from ["-" :infty] :to :infty}
                   :delta "(x)dx"]
                  :eq 1)
          
          p3 (tex [:int {:from ["-" :infty] :to :infty}
                   :delta "(x)"
                   "f(x)dx"]
                  :eq 0)]

      (tex
       [:documentclass "article"]
       [:usepackage "amsmath"]
       [:usepackage "amssymb"]
       [:usepackage {:opt ["left = 20mm" "right = 20mm"] } "geometry"]
       [:document
        [:huge
         ["The Dirac delta function" [:dol :delta "(x)"] "satisfies"
          [:math
           [:left :curly]
           [:array "c" p1 ",":next p2 "." ]
           [:right :none]]
          "And for any function" [:dol "f(x),"] "we have the equality"
          [:math p3 "."]]]])))

```

compile-and-view is useful when you want to see the result quickly, It takes
path to a TeX file and a string, writes the string therein and TeX compiles the file and opens the resulting PDF in the system's default viewer. Let's create a file:

test/texdata/examples/out/test.tex

To see the result of the above example, evaluate:

```clojure
  (compile-and-view "test/texdata/examples/out/test.tex" dirac-delta )
```

|[dirac](test/texdata/examples/out/test.png "test")

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
