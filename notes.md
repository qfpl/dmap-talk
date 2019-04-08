
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
