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

If you already have the Git repository and jar file but want the latest version:

    git pull
    ant clean
    ant jar

Basic Spec Definition
---------------------

To get top-level access to all Specjure functions, use Specjure in your namespace:

    (ns specjure.example (:use specjure))

Now you are ready to specify some behavior using plain English (or your language of choice):

    (spec "specjure.example/greet without arguments greets generally")

If you are spec'ing a specific function, you can optionally use its var:

    (spec greet "without arguments greets generally")

Now specify what you mean in Clojure:

    (spec greet "without arguments greets generally"
      (ie = "Greetings!" (greet))

The `ie` macro is similar to `clojure.core/apply`, but the last argument is not a list, and you can pass it macros.
`ie` closes its environment in a function for later verification, so let's do that.

Checking Specs
--------------

Go into the Clojure REPL of your choice and:

    (require 'specjure)

At this point you can load your file, and use:

    (specjure/check)

Or, if your file is in a separate file:

    (specjure/check-file "/path/to/file.clj"

If you give `check-file` a directory, it will check all files ending in `_spec.clj` in that directory and subdirectories.

You should have received an error like:

    java.lang.Exception: Unable to resolve symbol: greet in this context (my-examp.clj:4)
      [Thrown class clojure.lang.Compiler$CompilerException]

So let's define our function (make sure to define it before the spec.)

    (defn greet [])

Now if you check the spec again it will run and fail. The failure currently includes the spec description,
and will include more detailed failure information soon.

Let's get our function to pass:

    (defn greet [] "Greetings!")

More Spec'ing
-------------

Now we can add more specifications:

    (spec greet "with a single argument greets the individual"
      (ie = "Greetings Larry!" (greet "Larry"))
      (ie = "Greetings Frank!" (greet "Frank")))

Notice that we can group several Clojure language examples in one natural language specification.

Although Specjure focuses on simplicity, it can also be used for more complex specification.
You can run code before example by passing it to `before` inside of `spec`. All the instances of 
code inside `before` or `after` will run before or after each `ie`. 

For example, you could load some databaes records and clean them up in in `before` and `after`.
You can also store values from before in a thread local paramaters hash. Set items in the hash
using `($assoc! :key val)`, and retrieve them with `($get :key)`. 

`spec` support arbitray nesting, where each nest will inherit parent descriptions, befores, and afters.
You may want to share specs with `(share-spec "specs in this scenario" [my-var])` and use them in
`spec` like this: `(use-spec "specs in a certain scenario" {:data-for "my-var"})`

All of these features are in the [stack example](http://github.com/larrytheliquid/specjure/tree/master/examples/stack_spec.clj).
Look for the comment at the top of this file to see the equivalent RSpec.

Specjure separates the steps of collecting new specs, and checking them. This means you can do metaprogramming things like
mapping over a collection to generate a bunch of specifications. Any code inside of `ie` is not executed until check time
(as it is stored in a function.) The same applies to `before` and `after`.

After specs are collected into functions, a they can be retrieved lazily. Each before, after, and local parameter bindings
(what you access with `$get`) is specific to its `ie`. This was done intentionally to prevent troublesome bugs to track
down when doing stuff with clojure references, and other side-effect code. The api is not decided on for this yet, but
later the user will be able to access the lazy list of specs and run them however they please. A concurrent `pcheck` will
also be available.