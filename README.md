Specjure
========

Specjure is a software specification library for the [Clojure](http://clojure.org) programming language.
Specjure shares many similarities with [RSpec](http://rspec.info), but is differnet in ways that make it more suitable for a functional programming language, and Clojure specifically.

Getting
-------

Checkout Specjure using Git:

     git clone git://github.com/larrytheliquid/specjure.git

Then, build the latest jar:
    
    ant jar

Make sure to add the generated `clojure.jar` to your classpath.

Updating
--------

If you already have the Git repository and jar file but the library has updated:

   git pull
   ant clean
   ant jar

Basic Spec Definition
---------------------

To get top-level access to all Specjure functions, use Specjure in your namespace:

    (ns specjure.example (:use specjure))

Now you are ready to specify some behavior using plain English (or your language of choice):

    (spec "specjure.example/greet without arguments greets generally")

If you are spec'ing a specific function, you can optionally use its symbol.

    (spec greet "without arguments greets generally")

Now specify what you mean in Clojure:

    (spec greet "without arguments greets generally"
      (ie = "Greetings!" (greet))

The `ie` macro is similar to `clojure.core/apply`, but the last argument is not a list, and you can pass it macros.
`ie` closes its environment in a function for later verification, so lets do that.

Checking Specs
--------------

Go into the Clojure REPL of your choice and:

    (require 'specjure)

At this point you can load your file, and use:

    (specjure/check)

Or, if your file is in a separate file:

    (specjure/check-file "/path/to/file.clj"

If you give `check-file` a directory, it will check all files ending in `_spec.clj` in that directory and subdirectories.

You should have received:

    java.lang.Exception: Unable to resolve symbol: greet in this context (my-examp.clj:4)
      [Thrown class clojure.lang.Compiler$CompilerException]

So let's define our function (make sure to define it before the spec.)

    (defn greet [])

Now if you check the spec again it will run and fail. The failure currently includes the spec description,
and will include more detailed failure information soon.

Let's get our function to pass:

    (defn greet [] "Greetings!")