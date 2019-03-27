# Ficus Intro

Ficus is supposed to be as simple as possible (but not simpler) functional language with all (or most of) modern features that should make it suitable for intensive processing of numeric arrays or hierarhical tree-like data structures. In other words, for AI.

## Essential Ficus Features

So, what are the essential features that are planned for Ficus 1.0?
  - C-style syntax. There are several popular kinds of programming language syntactic styles: C, Python, ML, LISP, Pascal, ... However, C style is becoming the dominant one nowadays, so let's follow the trend. Also, we follow another trend of getting rid of ';' separators to make the code look a bit more like Python.
  - automatic memory management; the current implementation of Ficus uses reference counting with various optimizations to reduce unncessary increments/decrements  and also to avoid atomic operations whenever possible. Comparing to a full-scale Garbage Collector, Reference Counting let us reduce the runtime overhead and make the apps less memory-hungry and more cache-friendly. It's also makes it much more easy to extend Ficus with C (see further).
  - type safety and strict compile-time type checking with partial type inference. Because of this, Ficus is both type-safe, very efficient and not that verbose. No runtime "type boxing" is used, e.g. float takes 4 bytes and the tuple (uint8, uint8, uint8) takes 3 bytes. It also enables various compile optimizations that are quite difficult to implement in the case of dynamic languages, such as Python or Javascript.
  - memory-safe. Ficus does not only manages memory buffers automatically, it also checks index ranges when accessing arrays, which makes various memory access problems much easier to catch and fix. Special optimizations are applied to eliminate unnecessary range checks when arrays are accessed sequentially (in this case the checks are moved outside of the loop).
  - rich set of primitive data types: 8-, 16-, 32-, 64-bit signed and unsigned integers, 32- and 64-bit floating-point values (16-bit floating-point type will eventually be added as well), bool, char.
  - tuples and records. At compile-time records are converted to tuples. They are allocated on stack and there is no any extra overhead (i.e. no tags, tables of virtual methods etc.)
  - native support for multi-dimensional dense arrays. Many modern languages implement 2D arrays as arrays of arrays. And 4D arrays, which are essential data structures in Deep Learning, would need arrays of arrays of arrays of arrays, which is very inefficient. Of course, it's possible to represent multi-dimensional arrays using 1D arrays and on each access transform N-D index into the "raw" 1D index, but it's both inconvenient and error-prone. Instead, Ficus offers full support for multi-dimensional arrays, including convenient access to individual elements, as well as sub-arrays, just like in Python+numpy:

        // create 1920x1080 image containing 3-tuples and initialize it with 0's
        val myimg = array((1080, 1920), (0u8, 0u8, 0u8))
        val roi = rect_t { x = 10, y = 20, width = 100, height = 100 }
        // invert the ROI
        myimg[roi.y:roi.y + roi.height, roi.x:roi.x+roi.width] ^= (255u8, 255u8, 255u8)

  - basic support for the functional programming:
    * immutable values by default (val a = 5)
    * first-class functions (i.e. nested functions, anonymous functions can be freely passed and returned by functions; closures are formed when necessary)
    * recursion is well-supported, tail recursion is converted into a loop
    * there are key functional data structures: immutable single-connected lists, algebraic data types (a.k.a. variants)
    * functional-style pattern matching can be used to analyze, decompose and process lists, tuples, records, variants etc
  - exception-based error handling. Exceptions are very easy to define (a single line of code), and the pattern matching is used to handle exceptions.
  - array and list comprehensions. Those are special constructs, similar to Python list comprehensions, that let user to apply various higher-level functions (map, filter, reduce) to arrays and lists (and in the later versions to the user-defined containers as well, such as dictionaries), i.e. implement data processing algorithms in functional style.
  - good support for imperative programming. Unlike some fancy functional languages, such as Haskell, Ficus is rather a multi-paradigm language with the functional programming as preferable but not the only available way to write code. There are variables (`var`, a mutable counterpart of `val`), references (immutable pointers to mutable values), `while()` and `for()` loops etc. Arrays are also mutable data structures, so they are efficient (however, neither dimensionality, shape nor element type of an array can be changed at runtime).
  - immutable text strings and good Unicode support.
  - generic programming. Generic data stuctures and algorithms (a.k.a. templates) can be defined to avoid code duplication. By the way, Ficus does not offer a macro facility, but one can preprocess Ficus source code using `cpp` preprocessor (as Ficus syntax does not use `#` symbol and the overal lexical structure is more or less compatible with C)
  - function and operator overloading. Together with the generic programming facilities it helps to create a reusable code that automatically adapts to the processed data type.
  - module support. We mostly follow Python in this part, i.e. there are `import modname1, modname2 as nickname2, ...` and `from modname1 import f1, f2; from modname2 import *` constructs. However, unlike Python, Ficus does not support dynamic import of the modules. Import directives are processed by the compiler, and the final program or library is formed out of a collection of modules. If a certain module contains some initialization code, it is executed after all the module dependencies (i.e. other modules) have been initialized. Compile-time handling of "import" directives let us to completely eliminate any overhead of calling some function from another module; it's as fast as if the called and the caller functions were in the same module. Inline function expansion across modules is also possible.
  - object-oriented programming. It's very useful concept that helps to create good, reusable frameworks and build apps out of those frameworks. The classical OOP paradigm is based on 3 principles:
    * incapsulation
    * inheritance
    * polymorphism

    Without any doubt, the first and the last one are very useful, however the voices are split about inheritance. In pursuit of minimalism, simplicity of implementation and the decent performance, Ficus eliminates inheritance and leaves just incapsulation and polymorphism. More exactly, it introduces classes (and objects, their instances) and interfaces (a.k.a. protocols). An interface may define some types and function prototypes. A class may then implement zero or more interfaces, as well as add some members and extra methods that do not belong to any interface. An interface can actually inherit from another interface, so inheritance is party supported as well, but only at interface level, not the implementation level. Such approach automatically makes all classes "final", and all the method calls can be automatically inlined (unless we pass an interface to some function instead of a particular class). Classes, which do not implement any interfaces, do not need any virtual tables, they are light-weight.
  - last, but definitely not least feature is very smooth interaction with C/C++. First of all, Ficus compiler generates C code instead of machine code. By doing that we seriously reduce the effort of implementing efficient and portable Ficus compiler. We also let user to implement some of the functions right in C:

        ccode "#include <math.h>"
        nothrow pure inline fun sin(x: float): float = ccode "return sinf(x);"

    That is, just like C compilers let users to put inline assembly code, we let users to put inline C code :) It makes implementation of some standard library functions, as well as adding bindings for 3rd-party C/C++ libraries a very simple task. The generated by Ficus compiler C sources can be combined with other C/C++ modules to form the final app or component.

**[TODO] Cover separate compilation, REPL and the upcoming standard library.**

## Ficus vs. Other Languages

Let's overview the currently available solutions for AI apps. For example, consider *Ocaml*. It's rather elegant, very robust language that is used for some very critical projects. It demonstrates very good performance on hierarchical data processing, thanks to its static typing, efficient compiler and very efficient memory manager. We want something similar, but a bit more modern-looking and more convenional. *Kotlin* and *Swift* can be used as examples of such modern functional or semi-functional languages that use more conventional syntax and modern technologies. So, we want Ficus to be similar to them as well. Then the obvious question arises - why not just take one of those languages? The answer is that the languages provide many of the desured features, but not all of them, and in general they are quite similar to each other (here is a very nice comparison: http://nilhcem.com/swift-is-like-kotlin/) and mostly suited for implementation of normal/enterprise apps, whereas Ficus' primary focus is on data crunching, top performance and compatibility with C/C++; basically, Ficus project has quite an ambitious goal to replace C/C++ and Python for AI apps, where the 2 languages dominate currently.

Here is brief comparison of the languages (where in the Ficus column we put the planned but not yet implemented features :) ):

|   | Ficus  | Kotlin  | Swift  | Ocaml | F#  | C++ | Python |
|---|---|---|---|---|---|--|--|
| syntax  | C-style | C-style | C-style  | ML-style  | ML-style  | C-style | Python-style |
| paradigms | functional, imperative, OOP | funcional, imperative, OOP | functional, imperative, OOP | functional, imperative, OOP | functional, imperative, OOP | imperative, OOP, partly functional | functional, imperative, OOP |
| multi-threading  | +  | +  | +  | –  | + | + | ± (GIL) |
| memory management  | RC  | GC  | RC | GC  | GC  | manual | RC |
| safe | + | + | + | + | + | – | + (but heavily relies on runtime checks) |
| [projected] performance of array processing algorithms | excellent | good | good | good with int's, fair with float's | good | excellent | poor (needs C++ kernels for good speed) |
| interface to C | embedded | easy in native mode, unknown in JVM mode | easy | possible | possible | embedded | possible |
| real multi-dimensional arrays | + (embedded into the language and compiler) | nested 1D arrays | nested 1D arrays | nested 1D arrays | nested 1D arrays | via 3d-party libs | + (via numpy) |
| runtime footprint | tiny | v. big - JVM (or smaller with LLVM - w.i.p) | big, but smaller than JVM | small | .NET (v. big) | normally small; depends on the framework used | relatively small |
| std lib | looong way to go, but easy to connect 3rd-party C/C++ libs | excellent on JVM | excellent on Apple platforms; v. good on others | very good | excellent on MS platforms | v. good + many 3rd-party libs | excellent + many 3rd-party libs |
| numerical lib | looong way to go, but some basic algorithms are embedded into the language or are very easy to implement | + (Koma) | + (w.i.p, Fast AI etc.) | ± | + | OpenCV and such | fantastic! (numpy, scipy + many ML/AI frameworks) |
| implemented in itself? | Yes (for now we are using Ocaml) | Yes | No (C++) | Yes | No (C#) | Yes | No (CPython is implemented in C) |
| REPL (interactive mode) | – (planned in 2.0) | – | + | + | + | – | + |

As you can see, even though Ficus is very far from other mature languages, it's indeed designed to replace C/C++ (together with Python used as "front-end") as a language of choice for data chrunching algorithms. In other words, whereas Kotlin, Swift and F# are modern creatures for enterprise programming world, Ficus is for AI and numeric computing community.
