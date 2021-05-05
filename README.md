 

# The uniform-strings package 
Tries to resolve the confusing breadth of choices to represent and operate on text in Haskell. It contains 
- a principled set of conversions between representations for text, namely 
    - s - Strings,
    - t - Text,
    - b - ByteString,
    - l - LazyByteString, 
    - bu - ByteString containing UTF-8 characters,
    - latin - charcters with latin1 encoding
    - u - URLencoding, and 
conversions functions of type x2y (e.g. s2t for a conversion from String to Text) and methods to circumvent the difficulties.
- a set of functions applicable to character strings, which have the exact same semantics for all the types (usually the name of the function applicable to Strings with an appended \"\'\" to avoid name clashes with existing code.
- a few infix function for string manipulation. 

This is a package in `uniformBase` and other uniform packages will build on it. 


# To Do:
The code originated in 2010 to 2018. It will be revised  and the dependencies greatly reduced when using `text` and `basement`.

# Intention of "uniform" packages
The "uniform" packages are yet another attempt to select a useful subset from the overwhelming variety of the Haskell biotop. It was started in the 2010, grew over the years but was never packaged and put into Hackage; it is comparable to other similar attempts from which it has learned and occasionally copied code. 

The "uniform" approach is different from some others by:
- compatible with 'standard' Haskell, i.e. Haskell 2010 plus extensions as indicated in the modules,
- use the regular Haskell prelude,
- avoid name clashes as far as possible,
- combine logically connected operations in one place and in a form allowing coordinated use, but 
- broken in small, mostly independently building packages.

Choices are:
- strong preference for total functions, achieved often with producing error messages which makes debugging easier than the plain failure of partial functions,
- use monads-tf because TypeFamilies seemed to lead to better documentation.

Issues with this approach: it is limited by the deeps of understanding of Haskell of the authors and his experience. It shows a focus on understanding semantics (and formal ontology) linked to algebra applied to practical problems (Geographic Information Systems). 
It seems that efforts to construct coherent subsets of Haskell are limited by the complexity of the task -- the more comprehensive an environment should be the more complex is it to learn and use. The approach here is what emerged after some 25 years of using Haskell to write application oriented code, mostly to demonstrate theories in spatial information theory. 