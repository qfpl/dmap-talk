

map patientid patientdata
map investigatorid investigatordata

patientdata = 
  details
  day1
  day2
  day3
  
details =
  initials
  dob
  
day =
  investigator
  date
  weight
  some optional bool
  
this works for demoing the functor functor pattern, what about dmap?
maybe pre-existing conditions, where the conditions have parameters
- what stage/type/date-of-diagnosis cancer? what type of diabetes? 

a programmer specific on might be handy
- maybe interests / experience for YOW
- management stuff and programmer stuff might be different
  - years of experience might be common (classy prisms...)
  - programming might have a selection of languages (years per language?)
  - some aspect of programming that is closer to pre-existing conditions might be better
    - versions of C++ ?

- management
  - team lead / product manager / project manager
  - methodologies
- programming
  - languages
  - technologies
  
something near enough will probably do to get started with


- want a valiation example with both dmap and with vessel
- want a ggplot2 style config example
- want a prim-uniq example, possibly just reading in a bunch of stuff and being able to query it / delete from it
- should probably tidy up the FRP implementation

- vessel for RealWorld would probably be handy


Comments from Cale:
  https://www.reddit.com/r/haskell/comments/a1ofh2/maybe_not_rich_hickey/easta7k/
  https://www.reddit.com/r/haskell/comments/alywku/how_do_you_work_with_variations_of_sum_types/efs1rgu/

use as a heterogenous map
use as an extensible record system
- we don't necessarily have Has constraints

contraints-extras to impose constraints on everything in the dmap
prim-uniq for runtime madness
brief talk about Vessel for cool stuff?

Cool things:
  basic FRP implementation
  GGPlot 2 like system?
  Session types? linearity might be an issue
  Row type simulation? 
    - via classy prisms on the keys? to pick a particular key
    - via `k Int` to allow any key that maps to the right type
  Vessel and possibly Rhyolite?

Vessel for RealWorld would be useful to work stuff out


=====

   programmer and language 
vs team lead
vs manager and methodology 

could be handy for a DSum example

language as a parameter to a generic key vs a key per language

big dmap versus some data type with maps all through which could be empty, along with semigroup / monoid instances etc...
converting to a dmap lets us access all of our functor functor based data uniformly
vessel lets us encode things about multiplicities, so that we don't need to use f for that

- x maybe -> maybe (x identity)
- x maybe -> validation (x (const e)) (x identity)
- what should we do with x proxy?
- can also add some stuff like x widget, where widget ~ (Dynamic t x -> Event t (Endo x))










