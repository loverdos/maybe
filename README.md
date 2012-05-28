MAYBE
=====

Inspired by Lift's Box, Haskell's Maybe and Scala's Option.

We innovate in a few areas, especially regarding the existing Scala APIs:

* `Maybe()` is a polymorphich constructor that Does The Right Thing, no matter the input.
* `get` is only provided for a `Just` value, not in the general `Maybe` API.
* `map` on a `Just` Does The Right Thing, even if the calling function explodes.

Latest release is `"com.ckkloverdos" % "maybe" % "0.5.0"` from Maven Central,
compiled for Scala 2.9.1.

