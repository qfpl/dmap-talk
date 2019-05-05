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
type ExistingCondition = 
  DMap ExistingConditionKey Identity
```

##

```haskell
type ExistingConditionForm = 
  DMap ExistingConditionKey Maybe
```

##

```haskell
type ExistingConditionErrors = 
  DMap ExistingConditionKey (Const [Error])
```

##

```haskell
newtype Validator x = 
  Validator { 
    runValidator :: Maybe x 
                 -> Validation (Const [Error] x) (Identity x)
  }
```

```haskell
type ExistingConditionValidators = 
  DMap ExistingConditionKey Validator
```

##

TODO something around widgets?

# GADTs

## Regular types

```haskell
data PatientInformation =
    PatientDetails Text Day -- initials and dob
  | PatientId Int           -- id in the system
  deriving (Eq, Ord, Show)
```

```haskell
> :t PatientDetails
Text -> Day -> PatientInformation

> :t PatientId
Int -> PatientInformation
```

## GADT version

```haskell
{-# LANGUAGE GADTs #-}

data PatientInformation where
  PatientDetails :: Text -> Day -> PatientInformation
  PatientId :: Int -> PatientInformation
```

## Phantom types

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

data PatientStatus = 
    NewPatient 
  | InSystem
  deriving (Eq, Ord, Show)
```

```haskell
data PatientInformation (a :: PatientStatus) =
    PatientDetails Text Day -- initials and dob
  | PatientId Int           -- id in the system
  deriving (Eq, Ord, Show)
```

```haskell
newPatient :: Text 
           -> Day 
           -> PatientInformationNewPatient 
                   -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = 
  pure (PatientId 0)
addPatientToSystem (PatientId i) = 
  error "awkward..."
```

## GADT version

```haskell
data PatientInformation (a :: PatientStatus) where
  PatientDetails :: Text -> Day -> PatientInformation 'NewPatient
  PatientId :: Int -> PatientInformation 'InSystem
```

```haskell
addPatientToSystem :: PatientInformation 'NewPatient 
                   -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = 
  pure (PatientId 0)
```

## Existential types

```haskell
{-# LANGUAGE ExistentialQuantification #-}

data PatientInformation = forall a. (Read a, Show a) =>
    PatientDetails Text Day a
  | PatientId Int
```

```haskell
instance Show PatientInformation where
  showsPrec n (PatientId ident) =
    showString "PatientId " .
    showsPrec n ident
  showsPrec n (PatientDetails initials dob other) =
    showString "PatientDetails " .
    showsPrec n initials .
    showString " " .
    showsPrec n dob .
    showString " " .
    showsPrec n other
```

## GADT version

```haskell
{-# LANGUAGE GADTs #-}

data PatientInformation where
  PatientDetails :: (Read a, Show a) 
                 => Text 
                 -> Day 
                 -> a 
                 -> PatientInformation
  PatientId      :: Int 
                 -> PatientInformation
```

## GADTs for the win

```haskell
data AST a where
  IntLit :: Int -> AST Int
  BoolLit :: Bool -> AST Bool
  Lam :: (a -> AST b) -> AST (a -> b)
  Ap :: AST (a -> b) -> AST a -> AST b
```

## Other GADT fun

```haskell
data a := b where
  Refl :: a := a
```

```haskell
transitive :: 
    a := b 
 -> b := c 
 -> a := c
transitive ab bc = 
  case ab of
    Refl ->      -- now we know that a~b
      case bc of
        Refl ->  -- now we know that b~c
          Refl   -- captures that fact that a~c
```


# Functor functor

## 

```haskell
data Details =
  Details {
    dId :: Id
  , dInitials :: Text
  , dDOB :: Day
  , dWeight :: Maybe Weight
  } deriving (Eq, Ord, Show)
```

## 

```haskell
data Details f =
  Details {
    dId :: f Id
  , dInitials :: f Text
  , dDOB :: f Day
  , dWeight :: f (Maybe Weight)
  }
```

## 

```haskell
mapDetails :: 
     (forall x. g x -> h x) 
  -> Details g 
  -> Details h
mapDetails f (Details ident initials dob weight) =
  Details (f ident) (f initials) (f dob) (f weight)
```

```haskell
editFilled :: 
     Details Identity 
  -> Details Maybe
editFilled = 
  mapDetails (Just . runIdentity)
```

## 

```haskell
traverseDetails :: 
     Applicative f 
  => (forall x. g x -> f (h x)) 
  -> Details g 
  -> f (Details h)
traverseDetails f (Details ident initials dob weight) =
  Details <$> f ident <*> f initials <*> f dob <*> f weight
```

```haskell
checkFilled :: Details Maybe 
            -> Maybe (Details Identity)
checkFilled = 
  traverseDetails (fmap Identity)
```

##

```haskell
newtype Fn f g h x = Fn { runFn :: g x -> f (h x) }
```

```haskell
apDetails :: 
     Applicative f 
  => Details (Fn f g h) 
  -> Details g 
  -> f (Details h)
apDetails (Details fnIdent fnInitials fnDOB fnWeight) 
          (Details ident initials dob weight) =
  Details <$>
    runFn fnIdent ident <*>
    runFn fnInitials initials <*>
    runFn fnDOB dob <*>
    runFn fnWeight weight
```

## 

```haskell
newtype Fn f g h x = Fn { runFn :: g x -> f (h x) }
```

```haskell
validateInitials :: 
     Maybe Text
  -> Validation [DetailsError] (Identity Text)
validateInitials Nothing = 
  Failure [NotSet]
validateInitials (Just t)
  | Text.length t < 2 = Failure [InitialsTooShort]
  | otherwise = Success (Identity t)
```

## 

```haskell
newtype Fn f g h x = Fn { runFn :: g x -> f (h x) }
```

```haskell
detailsFn :: 
  Details (Fn (Validation [DetailsError]) Maybe Identity)
detailsFn =
  Details
  (Fn validateIdent)
  (Fn validateInitials)
  (Fn validateDOB)
  (Fn validateWeight)
```

## 

```haskell
newtype Fn f g h x = Fn { runFn :: g x -> f (h x) }
```

```haskell
validateDetails :: 
     Details Maybe 
  -> Validation [DetailsError] (Details Identity)
validateDetails =
  apDetails detailsFn
```

## 

The post "Functor functors" on benjamin.pizza has a great rundown of what you can do with this.

# DSum

## From `Data.Dependent.Sum`

```haskell
data DSum tag f = 
  (tag a) :=> (f a)
```

##

```haskell
data PatientInformationTag a where
  DetailsTag :: PatientInformationTag (Text, Day)
  IdTag      :: PatientInformationTag Int
```

##

```haskell
> let x = IdTag :=> Identity 1234
```

##

```haskell
(==>) :: Applicative f => tag a -> a -> DSum tag f
```

##

```haskell
> let x = IdTag ==> 1234
```

##

```haskell
addPatientToSystem :: 
     DSum PatientInformationTag Maybe
  -> IO (DSum PatientInformationTag Identity)
addPatientToSystem (DetailsTag :=> Nothing) =
  error "information not available"
addPatientToSystem (DetailsTag :=> Just (initials, dob)) =
  pure (IdTag ==> 0) -- some process here...
addPatientToSystem (IdTag :=> Nothing)
  error "information not available"
addPatientToSystem (IdTag :=> Just i) =
  pure (IdTag ==> i)
```

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
instance GEq PatientInformationTag where
  geq DetailsTag DetailsTag = Just Refl
  geq IdTag IdTag = Just Refl
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
instance GCompare PatientInformationTag where
  gcompare DetailsTag DetailsTag = GEQ
  gcompare DetailsTag _ = GLT
  gcompare _ DetailsTag = GGT
  gcompare IdTag IdTag = GEQ
```

##

There is also a `GShow` typeclass.

```haskell
instance GShow PatientInformationTag where
  gshowsPrec _ DetailsTag =
    showString "DetailsTag"
  gshowsPrec _ IdTag =
    showString "IdTag"
```

##

The `dependent-sum-template` package can create these instances for us.

##

```haskell
import Data.GADT.Compare.TH
import Data.GADT.Show.TH

deriveGEq ''PatientInformationTag
deriveGCompare ''PatientInformationTag
deriveGShow ''PatientInformationTag
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
