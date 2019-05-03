% DMap for delightful dynamism
% Dave Laing

# Introduction

##

-- motivating example for a heterogenous map

##

```haskell
data ExistingConditions =
  ExistingConditions {
    ecDiabetes :: Maybe DiabetesInfo
  , ecCancer :: Map CancerType CancerStage
  , ecEpilepsy :: Any
  } deriving (Eq, Ord, Show)
```

##

-- code for doing validation over that pile of maps

##

-- why you might want something like DMap

##

```haskell
data ExistingConditionKey a where
  ECDiabetes :: ExistingConditionKey DiabetesInfo
  ECCancer :: CancerType -> ExistingConditionKey CancerStage
  ECEpilepsy :: ExistingConditionKey ()
```

```haskell
type ExistingCondition = DMap ExistingConditionKey Identity
```

##

-- code for doing validation with DMap

##

-- why you might want something like Vessel

##

-- data structure for wrapping that up with Vessel

##

-- code for doing validation with Vessel


# GADTs

## Regular types

```haskell
data Foo = 
    Bar Int 
  | Baz String
```

```haskell
> :t Bar
Int -> Foo

> :t Baz
String -> Foo
```

## Phantom types

```haskell
{-# LANGUAGE DataKinds #-}

data Open
data Closed

data Door a = Door

open :: Door a -> Door Open

knock :: Door Closed -> IO ()

close :: Door a -> Door Closed
```

-- TODO

## Existential types

-- TODO

## GADTs for regular types

```haskell
data Foo where
  Bar :: Int -> Foo
  Baz :: String -> Foo
```

## GADTs for phantom types

```haskell
data Door a where
  Open :: Door a -> Door Open
  Close :: Door a -> Door Closed
```

## GADTs for existential types

```haskell
data AST a where
  IntLit :: Int -> AST Int
  BoolLit :: Bool -> AST Bool
  Lam :: (a -> AST b) -> AST (a -> b)
  App :: AST (a -> b) -> AST a -> AST b
```

## GADTs for the win

-- TODO

# Functor functor

## 

-- example type, name and date of birth from opening example

## 

-- example type with fs

## 

-- run through what different values of f might correspond to
  - maybe (or Option Last) for being filled out, in ways that might be combined
  - identity for filled out
  - either e / validation e for checking problems
  - const e for reporting problems
  - proxy for picking out fields of interest
    - this is more interesting after a View has been condensed, so we can get a count
      of how many things have a non-nothing member for the selected field, for instance
  - maybe mention selected count?

## 

-- talk about what map and traverse end up looking like

## 

-- examples of map / traverse
  - probably validation, possibly widgets

## 

-- shoutout to the benjamin.pizza post

# DSum

## From `Data.Dependent.Sum`

```haskell
data DSum tag f = 
  (tag a) :=> (f a)
```

##

```haskell
data MyTag a where
  IntTag  :: MyTag Int
  BoolTag :: MyTag Bool
```

##

```haskell
> let x = IntTag :=> Identity 1
```

##

```haskell
(==>) :: Applicative f => tag a -> a -> DSum tag f
```

##

``` haskell
> let y :: DSum MyTag Identity = BoolTag ==> False
```

##

```haskell
toString :: DSum MyTag Identity -> String
toString (IntTag  :=> Identity i) = show i ++ " :: Int"
toString (BoolTag :=> Identity b) = show b ++ " :: Bool"
```

##


## From `Data.GADT.Compare`

```haskell
class GEq f where
  geq :: f a -> f b -> Maybe (a := b) 
```

```haskell
data a := b where
  Refl :: a := a
```

##

```haskell
instance GEQ MyTag where
  geq IntTag IntTag = Just Refl
  geq BoolTag BoolTag = Just Refl
  geq _ _ = Nothing
```

## From `Data.GADT.Compare`

```haskell
class GEq f => GCompare f where
  gcompare :: f a -> f b -> GOrdering a b
```

```haskell
data GOrdering a b where
  GLT :: GOrdering a b
  GEQ :: GOrdering t t
  GGT :: GOrdering a b
```

##

```haskell
instance GCompare MyTag where
  gcompare IntTag IntTag = GEQ
  gcompare IntTag _ = GLT
  gcompare _ IntTag _ = GGT
  gcompare BoolTag BoolTag = GEQ
```

##

There is also a `GShow` typeclass.

##

The `dependent-sum-template` package can create these instances for us.

##

```haskell
import Data.GADT.Compare.TH
import Data.GADT.Show.TH

deriveGEq ''MyTag
deriveGCompare ''MyTag
deriveGShow ''MyTag
```


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

## 

-- talk about the problem

##

-- talk about the solution


# Example: Validation

## 

-- talk about the problem

##

-- talk about the solution

# Example: Dynamic tags

## 

-- talk about the problem

##

-- talk about the solution

# Conclusion
