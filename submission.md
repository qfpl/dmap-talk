
Session Title: 
  DMap for delightful dynamism

Abstract:

Living in a statically typed world is great, but sometimes a part of your problem really does need to be modelled as something a bit more dynamic.
Maybe something in your domain really should be modelled as a heterogenous collection.
Maybe you just have a bunch of things you want to group together and work with via a common typeclass, but their types are all different.
Maybe you're interfacing with a system that doesn't care about types.
You might just want to feel the wind through your hair like when you were working with dynamically typed languages.

The dependent-map library (and friends) has you covered for all of these things.
It gives you a heterogenous collection with great usability, where you use GADTs as the keys and have the "Functor Functor" pattern wrapped up in the mix for good measure.

This talk will start by looking at GADTs and the "Functor Functor" pattern to make sure everyone is on the same page, before spending some time getting to know the the details of DSum, DMap and friends.
After that we'll spend some time looking at some of the additional things you can do using related libraries from the ecosystem, and then we'll spend the rest of the time looking at examples and applications.

Theme: 
  Practice
Level: 
  Intermediate
Session Type: 
  Talk
Duration: 
  30 mins
Target audience: 
  Haskell programmers
Session prerequisite:
  Some understanding of Haskell
  Knowledge of GADTs, or the willingnes to passively acquire that knowledge over the course of the talk
Learning outcome:
  A better understanding of how using some more-advanced-than-vanilla type system features can lead to be better code when the rubber hits the road.
  An understanding of when and how to use DMaps in your code.
Outline:

- Some motivating examples (3 mins)
  - both showing uses of DMap without explicitly calling out or showing that DMap is involved
  - one involving DMaps as heterogenous maps
  - one involving form validation where parts of the form will involve a heterogenous collection

- An explanation of GADTs (3 mins)
  - what they are, how to create them, how to use them

- An explanation of the Functor Functor idea (3 mins)
  - data Foo f = Foo { bar :: f Int, baz :: f String } 
  - what happens when you use different Functors as f
  - a brief, high-level explanation about the kinds of things you can do with them if you build up enough typeclasses
    - maps, traversals, construction, etc...
  - a shout out to the Functor Functor article by benjamin.pizza
    - https://www.benjamin.pizza/posts/2017-12-15-functor-functors.html

- DSum (2 mins)
  - data DSum tag f = (tag a) :=> (f a)
  - this can be viewed as being similar to a dynamic version of polymorphic variants, but with the Functor Functor pattern mixed in
  - just like a GADT, we can pattern match on these to get access to the type
  - we can write things like `getSummary :: DSum MyTag Identity -> Int`, in which we handle the pattern matching so our users don't have to

- DMap (5 mins)
  - DMap.fromList :: [DSum tag f] -> DMap tag f is like Map.fromList [(k, v)] -> Map k v, except the values depend on the keys and are wrapped in the f type construtor
  - this can be viewed as being similar to a dynamic version of row polymorphism, but with the Functor Functor pattern mixed in
  - cover the type and the basic API
  - show that most of the core bits of the Map API from containers is present
  - show that there is functionality that can do the things from the Functor Functor post
  - show that you can nest keys and what that buys you
  - tie back to the heterogenous map example from the intro

- How you can use classy prisms to generalize the keys (1 mins)
  - this will be explicitly scoped to those who are already familiar with prisms
  - it is a handy trick that is worth knowing about

- How to apply constraints to all of the values in a DMap (2 mins)
  - This is done via the constraints-extras library
  - look at the Has/Has' constraint and the has/has' function for consuming it
  - a brief mention of the aeson-gadt-th and dependent-sum-aeson-orphans libraries for JSON interoperation

- Vessel (5 mins)
  - This is a library that wraps up the Functor Functor idea for Identity / Option First / Map / DMap and makes it possible to nest them
  - It allows us to stack these things and map / traverse / union / intersect them
  - This section will most likely bleed into extended form validation example in the next section
  - The focus will be on the core idea and the fact that it exists and is useful, rather than the details of Vessel

- Application: form validation (2 mins)
  - this is the extended from of the example from the intro
  - use Maybe a -> Validation e (Identity a) to traverse over a DMap k Maybe to get a Validation e (DMap k Identity)
  - repeat with Vessel to show how wild you can go with this

- Application: complex configuration (2 mins)
  - the ggplot2 library in R and its clones are well-loved because they have a nice system for specifying plots
    that comprise of related parts with good defaults that are partly interdependent
  - this part will go through a subset of that which is thorny enough to be impressive and show how
    using DMap and friends can be used to make a complex but highly usable typed API for this kind of thing

- Application: even more dynamism (2 mins, if time remains)
  - using the prim-uniq library to create tags dynamically at runtime
  - this really underlines how dynamic you can make things
  - I have a toy FRP system implemented using this which I will tidy up / mention / link to

