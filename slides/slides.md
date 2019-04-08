% DMap for delightful dynamism
% Dave Laing

# Introduction

##

-- could do with a running example that goes through to Vessel

# GADTs

##

```haskell
data Foo = Bar Int | Baz String
```

```haskell
> :t Bar
Int -> Foo
```

```haskell
> :t Baz
String -> Foo
```

##

```haskell
data Open
data Closed

data Door a = Door

open :: Door a -> Door Open

knock :: Door Closed -> IO ()

close :: Door a -> Door Closed
```

##

```haskell
data Door = Open | Closed

data Fridge (d :: Door) (ss :: [*]) where
  NewFridge   :: Fridge Closed []
  OpenFridge  :: Fridge d ss -> Fridge Open ss
  CloseFridge :: Fridge d ss -> Fridge Open ss
  AddToFridge :: s -> Fridge Open ss -> Fridge Open (s : ss)
  EmptyFridge :: FridgeOpen ss -> Fridge Open []
```

-- Delay :: Show b => (a -> b) -> a -> Wat b

-- possibly a simple language and evaluator as an example
-- HOAS?

-- something using a witness would be cool, like the typerep example
-- do singletons count?

# Functor functor

## 

-- example type

## 

-- example type with fs

## 

-- run through what different values of f might correspond to

## 

-- talk about what map and traverse end up looking like

## 

-- examples of map / traverse

# DSum

##

-- show the type

##

-- show introduction

##

-- show elimination

##

-- show typeclasses


# DMap

##

-- show the type

##

-- show fromList / toList

##

-- show singelton / insert / update / delete

##

-- show the typeclasses from DSum

##

-- show map and traverse

# DMap and tricks with keys

##

-- show what you can do with classy prisms for keys

##

-- show what you can do with nested keys

# DMap and Constraints

##

-- JSON example

##

-- show class

##

-- show TH to get class

##

-- show the constraints level usage

##

-- show the term level usage

# Vessel

##

-- show the View typeclass

##

-- show the various things which are instances of it

##

-- show Vessel itself

##

-- show a use of Vessel

# Example: Configuration

# Example: Validation

# Example: Dynamic tags

# Conclusion
