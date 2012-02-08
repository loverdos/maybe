MAYBE
=====

Inspired by Lift's Box, Haskell's Maybe and Scala's Option.

We innovate in a few areas, especially regarding the existing Scala APIs:

* `Maybe()` is a polymorphich constructor that Does The Right Thing, no matter the input.
* `get` is only provided for a `Just` value, not in the general `Maybe` API.
* `map` on a `Just` Does The Right Thing, even if the calling function explodes.

Latest release is `"com.ckkloverdos" % "maybe_2.9.1" % "0.3.0"` from Maven Central.
There are also jars for Scala 2.8.2 and 2.8.1.

