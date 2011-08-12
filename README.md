### What this is:

It's a slightly cleaned-up version of some example code from [an answer on 
Stack Overflow](http://stackoverflow.com/questions/7030476/haskell-function-with-both-an-arbitrary-number-of-arguments-and-io/7035253#7035253).
It's mostly a silly example of doing some computation with type 
families--specifically, to construct terms in a type-directed manner, rather 
than constructing free-floating types for their own sake as is often used for
examples.

### What it does:

Insofar as it does anything terribly useful, it converts between functions 
using standard Haskell types, e.g. `Int -> String -> IO Double`, and equivalent
functions using C types for the FFI, e.g. `CInt -> CString -> IO CDouble`.

Lots of functionality is missing, though, since it was written as a 
demonstration.

### What else?

You'll need a recent version of GHC to try this. And by recent, I mean 
"released a few days before I wrote this code", heh.
