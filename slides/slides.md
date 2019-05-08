% DMap for delightful dynamism
% Dave Laing

# Introduction

##

Sometimes we have a problem that is best modeled as a heterogenous map.

##

```haskell
data ExistingConditions =
  ExistingConditions {
    ecDiabetes :: Maybe DiabetesInfo
  , ecCancer   :: Map CancerType CancerStage
  , ecEpilepsy :: Bool
  } deriving (Eq, Ord, Show)
```

##

Adding more `Functor`s to things is always an improvement<sup>*</sup>.

##

```haskell
data ExistingConditions f =
  ExistingConditions {
    ecDiabetes :: f (Maybe DiabetesInfo)
  , ecCancer   :: Map CancerType (f CancerStage)
  , ecEpilepsy :: f Bool
  } deriving (Eq, Ord, Show)
```

##

```haskell
checkFilled ::
            ExistingConditions Maybe 
  -> Maybe (ExistingConditions Identity)
checkFilled (ExistingConditions d c e) =
  ExistingConditions       <$> 
    fmap Identity        d <*> 
    fmap (fmap Identity) c <*> 
    fmap Identity        e
```

##

Adding more type system features to things is _also_ always an improvement<sup>**</sup>.

##

```haskell
data ExistingConditionKey a where
  ECDiabetes :: ExistingConditionKey DiabetesInfo
  ECCancer   :: CancerType 
             -> ExistingConditionKey CancerStage
  ECEpilepsy :: ExistingConditionKey ()
```

```haskell
type ExistingCondition = 
  DMap ExistingConditionKey Identity
```

##

`Data.Dependent.Map` 

> - just like `Data.Map`, only dependentier!

##

```haskell
type ExistingConditionForm = 
  DMap ExistingConditionKey Maybe
```

##

```haskell
traverseWithKey :: 
     Applicative t 
  => (forall v. k v -> f v -> t (g v)) 
  -> DMap k f -> t (DMap k g) 
```

##

```haskell
checkFilled ::
            DMap ExistingConditionKey Maybe 
  -> Maybe (DMap ExistingConditionKey Identity)
checkFilled = 
  traverseWithKey (fmap Identity)
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
  PatientDetails :: Text 
                 -> Day 
                 -> PatientInformation
  PatientId      :: Int 
                 -> PatientInformation
```

## Phantom types

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

data PatientStatus = 
    NewPatient 
  | InSystem
  deriving (Eq, Ord, Show)

data PatientInformation (a :: PatientStatus) =
    PatientDetails Text Day -- initials and dob
  | PatientId Int           -- id in the system
  deriving (Eq, Ord, Show)
```

## Phantom types

```haskell
newPatient :: 
     Text 
  -> Day 
  -> PatientInformation 'NewPatient
newPatient = 
  PatientDetails
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation 'NewPatient 
  -> IO (PatientInformation 'InSystem)
addPatientToSystem = _



  
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation 'NewPatient 
  -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = do



  
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation 'NewPatient 
  -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = do
  i <- actuallyAddThePatientDetails initials dob


  
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation 'NewPatient 
  -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = do
  i <- actuallyAddThePatientDetails initials dob
  pure (PatientId i)

  
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation 'NewPatient 
  -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = do
  i <- actuallyAddThePatientDetails initials dob
  pure (PatientId i)
addPatientToSystem (PatientId i) = 
  
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation 'NewPatient 
  -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = do
  i <- actuallyAddThePatientDetails initials dob
  pure (PatientId i)
addPatientToSystem (PatientId i) = 
  pure (PatientId i)
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation 'NewPatient 
  -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = do
  i <- actuallyAddThePatientDetails initials dob
  pure (PatientId i)
addPatientToSystem (PatientId i) = 
  pure (PatientId i) -- awful ...
```

## GADT version

```haskell
data PatientInformation (a :: PatientStatus) where
  PatientDetails :: Text 
                 -> Day 
                 -> PatientInformation 'NewPatient
  PatientId      :: Int 
                 -> PatientInformation 'InSystem
```

## GADT version

```haskell
addPatientToSystem :: 
         PatientInformation 'NewPatient 
  -> IO (PatientInformation 'InSystem)
addPatientToSystem = _
  
```

## GADT version

```haskell
addPatientToSystem :: 
         PatientInformation 'NewPatient 
  -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = 
  
```

## GADT version

```haskell
addPatientToSystem :: 
         PatientInformation 'NewPatient 
  -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = 
  pure (PatientId 0)
```

## GADT version

```haskell
addPatientToSystem :: 
         PatientInformation 'NewPatient 
  -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = 
  pure (PatientId 0) -- ... awesome
```

## Existential types

```haskell
{-# LANGUAGE ExistentialQuantification #-}

data PatientInformation = forall a. (Read a, Show a) =>
    PatientDetails Text Day a
  | PatientId Int
```

## Existential types

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

## GADTs for transporting evidence

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
   
   
   
    
   
```

## GADTs for transporting evidence

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
    Refl ->      -- now we know that       a ~ b
    
    
   
```

## GADTs for transporting evidence

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
    Refl ->      -- now we know that       a ~ b
      case bc of
        Refl ->  -- now we know that       b ~ c
  
```

## GADTs for transporting evidence

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
    Refl ->      -- now we know that       a ~ b
      case bc of
        Refl ->  -- now we know that       b ~ c
          Refl   -- captures evidence that a ~ c
```

## GADTify all of the things

```haskell
data AST a where
  IntLit :: Int -> AST Int
  BoolLit :: Bool -> AST Bool
  Lam :: (a -> AST b) -> AST (a -> b)
  Ap :: AST (a -> b) -> AST a -> AST b
```

## GADTify all of the things

```haskell
collatzStep :: AST (Int -> Int)
collatzStep = Lam $ \x -> IntLit $
  if even x
  then x `div` 2
  else 3 * x + 1

collatz :: AST (Int -> Int)
collatz = Lam $ \x ->
  if x == 1
  then IntLit 1
  else Ap collatz (Ap collatzStep (IntLit x))

testMe :: AST Int
testMe =
  Ap collatz (IntLit 7)
```

# Functor functor

## 

```haskell
data Details =
  Details {
    dId       :: Id
  , dInitials :: Text
  , dDOB      :: Day
  , dWeight   :: Maybe Weight
  } deriving (Eq, Ord, Show)
```

## 

```haskell
data Details f =
  Details {
    dId       :: f Id
  , dInitials :: f Text
  , dDOB      :: f Day
  , dWeight   :: f (Maybe Weight)
  }
```

## 

```haskell
mapDetails :: 
     (forall x. g x -> h x) 
  -> Details    g 
  -> Details           h
mapDetails f (Details ident initials dob weight) =
  Details (f ident) (f initials) (f dob) (f weight)
```

```haskell

 


  
```

## 

```haskell
mapDetails :: 
     (forall x. g x -> h x) 
  -> Details    g 
  -> Details           h
mapDetails f (Details ident initials dob weight) =
  Details (f ident) (f initials) (f dob) (f weight)
```

```haskell
editFilled :: 
     Details Identity 
  -> Details Maybe
editFilled =
  _
```

## 

```haskell
mapDetails :: 
     (forall x. g x -> h x) 
  -> Details    g 
  -> Details           h
mapDetails f (Details ident initials dob weight) =
  Details (f ident) (f initials) (f dob) (f weight)
```

```haskell
editFilled :: 
     Details Identity 
  -> Details Maybe
editFilled = 
  mapDetails _
```

## 

```haskell
mapDetails :: 
     (forall x. g x -> h x) 
  -> Details    g 
  -> Details           h
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
  ->    Details g 
  -> f (Details           h)
traverseDetails f (Details ident initials dob weight) =
  Details <$> f ident <*> f initials <*> f dob <*> f weight
```

```haskell




  
```

## 

```haskell
traverseDetails :: 
     Applicative f 
  => (forall x. g x -> f (h x)) 
  ->    Details g 
  -> f (Details           h)
traverseDetails f (Details ident initials dob weight) =
  Details <$> f ident <*> f initials <*> f dob <*> f weight
```

```haskell
checkFilled ::
            Details Maybe 
  -> Maybe (Details Identity)
checkFilled = 
  _
```

## 

```haskell
traverseDetails :: 
     Applicative f 
  => (forall x. g x -> f (h x)) 
  ->    Details g 
  -> f (Details           h)
traverseDetails f (Details ident initials dob weight) =
  Details <$> f ident <*> f initials <*> f dob <*> f weight
```

```haskell
checkFilled ::
            Details Maybe 
  -> Maybe (Details Identity)
checkFilled = 
  traverseDetails _
```

## 

```haskell
traverseDetails :: 
     Applicative f 
  => (forall x. g x -> f (h x)) 
  ->    Details g 
  -> f (Details           h)
traverseDetails f (Details ident initials dob weight) =
  Details <$> f ident <*> f initials <*> f dob <*> f weight
```

```haskell
checkFilled ::
            Details Maybe 
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
  =>    Details (Fn f g h) 
  ->    Details       g 
  -> f (Details         h)
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
validateInitials :: 
                                Maybe    Text
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
apDetails :: 
     Applicative f 
  => Details (Fn  f                          g     h       ) 
  -> Details g -> f (Details h)

detailsFn :: 
     Details (Fn (Validation [DetailsError]) Maybe Identity)
```

```haskell




  
```

## 

```haskell
apDetails :: 
     Applicative f 
  => Details (Fn  f                          g     h       ) 
  -> Details g -> f (Details h)

detailsFn :: 
     Details (Fn (Validation [DetailsError]) Maybe Identity)
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
addPatientToSystem = _






  
```

##

```haskell
addPatientToSystem :: 
         DSum PatientInformationTag Maybe
  -> IO (DSum PatientInformationTag Identity)
addPatientToSystem (DetailsTag :=> Nothing) =






  
```

##

```haskell
addPatientToSystem :: 
         DSum PatientInformationTag Maybe
  -> IO (DSum PatientInformationTag Identity)
addPatientToSystem (DetailsTag :=> Nothing) =
  error "information not available"





  
```

##

```haskell
addPatientToSystem :: 
         DSum PatientInformationTag Maybe
  -> IO (DSum PatientInformationTag Identity)
addPatientToSystem (DetailsTag :=> Nothing) =
  error "information not available"
addPatientToSystem (DetailsTag :=> Just (initials, dob)) =




  
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

##

```haskell
instance GEq PatientInformationTag where
  -- geq :: f a -> f b -> Maybe (a := b) 





  
```

##

```haskell
instance GEq PatientInformationTag where
  -- geq :: f a -> f b -> Maybe (a := b) 
  geq DetailsTag DetailsTag = 




  
```

##

```haskell
instance GEq PatientInformationTag where
  -- geq :: f a -> f b -> Maybe (a := b) 
  geq DetailsTag DetailsTag = 
    Just Refl



  
```

##

```haskell
instance GEq PatientInformationTag where
  -- geq :: f a -> f b -> Maybe (a := b) 
  geq DetailsTag DetailsTag = 
    Just Refl
  geq IdTag IdTag = 


  
```

##

```haskell
instance GEq PatientInformationTag where
  -- geq :: f a -> f b -> Maybe (a := b) 
  geq DetailsTag DetailsTag = 
    Just Refl
  geq IdTag IdTag = 
    Just Refl

  
```

##

```haskell
instance GEq PatientInformationTag where
  -- geq :: f a -> f b -> Maybe (a := b) 
  geq DetailsTag DetailsTag = 
    Just Refl
  geq IdTag IdTag = 
    Just Refl
  geq _ _ = 
  
```

##

```haskell
instance GEq PatientInformationTag where
  -- geq :: f a -> f b -> Maybe (a := b) 
  geq DetailsTag DetailsTag = 
    Just Refl
  geq IdTag IdTag = 
    Just Refl
  geq _ _ = 
    Nothing
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
  -- gcompare :: f a -> f b -> GOrdering a b







  
```

##

```haskell
instance GCompare PatientInformationTag where
  -- gcompare :: f a -> f b -> GOrdering a b
  gcompare DetailsTag DetailsTag = 






  
```

##

```haskell
instance GCompare PatientInformationTag where
  -- gcompare :: f a -> f b -> GOrdering a b
  gcompare DetailsTag DetailsTag = 
    GEQ





  
```

##

```haskell
instance GCompare PatientInformationTag where
  -- gcompare :: f a -> f b -> GOrdering a b
  gcompare DetailsTag DetailsTag = 
    GEQ
  gcompare DetailsTag _ = 




  
```

##

```haskell
instance GCompare PatientInformationTag where
  -- gcompare :: f a -> f b -> GOrdering a b
  gcompare DetailsTag DetailsTag = 
    GEQ
  gcompare DetailsTag _ = 
    GLT



  
```

##

```haskell
instance GCompare PatientInformationTag where
  -- gcompare :: f a -> f b -> GOrdering a b
  gcompare DetailsTag DetailsTag = 
    GEQ
  gcompare DetailsTag _ = 
    GLT
  gcompare _ DetailsTag = 


  
```

##

```haskell
instance GCompare PatientInformationTag where
  -- gcompare :: f a -> f b -> GOrdering a b
  gcompare DetailsTag DetailsTag = 
    GEQ
  gcompare DetailsTag _ = 
    GLT
  gcompare _ DetailsTag = 
    GGT

  
```

##

```haskell
instance GCompare PatientInformationTag where
  -- gcompare :: f a -> f b -> GOrdering a b
  gcompare DetailsTag DetailsTag = 
    GEQ
  gcompare DetailsTag _ = 
    GLT
  gcompare _ DetailsTag = 
    GGT
  gcompare IdTag IdTag = 
  
```

##

```haskell
instance GCompare PatientInformationTag where
  -- gcompare :: f a -> f b -> GOrdering a b
  gcompare DetailsTag DetailsTag = 
    GEQ
  gcompare DetailsTag _ = 
    GLT
  gcompare _ DetailsTag = 
    GGT
  gcompare IdTag IdTag = 
    GEQ
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

deriveGEq      ''PatientInformationTag
deriveGCompare ''PatientInformationTag
deriveGShow    ''PatientInformationTag
```

##

```haskell
{-# LANGUAGE StandaloneDeriving #-}

deriving instance Show (PatientInformationTag a)
```

# DMap

## From `Data.Dependent.Map`

```haskell
data DMap k f = ...
```

##

```haskell
toList   ::
                 DMap k f  -> [DSum k f]
fromList :: 
  GCompare k => [DSum k f] ->  DMap k f
```

##

```haskell
singleton :: 
  k v -> f v -> DMap k f
```

```haskell
insert :: 
     GCompare k 
  => k v -> f v -> DMap k f -> DMap k f
```

```haskell
delete :: 
     GCompare k 
  => k v -> DMap k f -> DMap k f
```

##

```haskell
adjust :: 
     GCompare k 
  => (f v -> f v) 
  -> k v -> DMap k f -> DMap k f
```

```haskell
update :: 
     GCompare k 
  => (f v -> Maybe (f v)) 
  -> k v -> DMap k f -> DMap k f
```

##

```haskell
union :: 
     GCompare k 
  => DMap k f -> DMap k f -> DMap k f
```

```haskell
unionWithKey :: 
      GCompare k 
  => (k v -> f v -> f v -> f v) 
  -> DMap k f -> DMap k f -> DMap k f
```

##

```haskell
difference :: 
     GCompare k 
  => DMap k f -> DMap k g -> DMap k f
```

```haskell
differenceWithKey :: 
     GCompare k 
  => (forall v. k v -> f v -> g v -> Maybe (f v)) 
  -> DMap k f -> DMap k g -> DMap k f
```

##

```haskell
intersection :: 
     GCompare k 
  => DMap k f -> DMap k f -> DMap k f
```

```haskell
intersectionWithKey :: 
  GCompare k 
  => (forall v. k v -> f v -> g v -> h v) 
  -> DMap k f -> DMap k g -> DMap k h
```

##

```haskell
mapWithKey :: 
  (forall v. k v -> f v -> g v) 
  -> DMap k f -> DMap k g 
```

```haskell
mapMaybeWithKey :: 
     GCompare k 
  => (forall v. k v -> f v -> Maybe (g v)) 
  -> DMap k f -> DMap k g 
```

```haskell
traverseWithKey :: 
     Applicative t 
  => (forall v. k v -> f v -> t (g v)) 
  -> DMap k f -> t (DMap k g) 
```

##

-- show the vessel example but in DMap land, and how to do validation with it

-- talk about having the full keyset compared to having some things missing on occasion
-- proxy for querying probably comes in here

# DMap and tricks with keys

## A bonus GADT

```haskell
data Some (tag :: k -> *) where
  This :: Some tag	 
```

```haskell
keys :: 
      DMap k f 
  -> [Some k] 
```

```haskell

  
```

## A bonus GADT

```haskell
data Some (tag :: k -> *) where
  This :: Some tag	 
```

```haskell
keys :: 
      DMap k f 
  -> [Some k] 
```

```haskell
> DMap.keys myTestMap
  
```

## A bonus GADT

```haskell
data Some (tag :: k -> *) where
  This :: Some tag	 
```

```haskell
keys :: 
      DMap k f 
  -> [Some k] 
```

```haskell
> DMap.keys myTestMap
[This DKInitials, This DKDOB]
```

## Nesting keys

```haskell
data NewPatientKey a where
  NPKInitials :: NewPatientKey Text
  NPKDOB      :: NewPatientKey Day
```

```haskell
data InSystemKey a where
  ISKId :: InSystemKey Int
```

```haskell


  
```

## Nesting keys

```haskell
data NewPatientKey a where
  NPKInitials :: NewPatientKey Text
  NPKDOB      :: NewPatientKey Day
```

```haskell
data InSystemKey a where
  ISKId :: InSystemKey Int
```

```haskell
data DetailsKey a where
  DKNewPatient :: NewPatientKey a -> DetailsKey a
  DKInSystem   :: InSystemKey a   -> DetailsKey a
```

## Nested keys give us unions

```haskell
injectNewPatient :: 
     DMap NewPatientKey f 
  -> DMap DetailsKey f
injectNewPatient = 
  DMap.mapKeysMonotonic DKNewPatient
```

##

```haskell
testDMap :: 
     GCompare k
  => DMap k Proxy
  -> DMap k f
  -> DMap k Proxy
testDMap =
  DMap.intersectionWithKey (\_ p _ -> p)
```

```haskell
matchesDMap :: 
     (EqTag k Proxy, GCompare k)
  => DMap k Proxy
  -> DMap k f
  -> Bool
matchesDMap dmp dmf =
  dmp == testDMap dmp dmf
```

##

```haskell
newPatientTest :: 
  DMap DetailsKey Proxy
newPatientTest =
  injectNewPatient . DMap.fromList $ [
    NPKInitials :=> Proxy
  , NPKDOB :=> Proxy
  ]
```


```haskell
isNewPatient :: 
     DMap DetailsKey f 
  -> Bool
isNewPatient =
  matchesDMap newPatientTest
```

## Classy prisms for keys

```haskell
class HasNewPatient k where
  _NewPatient :: forall x. Prism' (k x) (NewPatientKey x)

  ...
```

## Classy prisms for keys

```haskell
class HasNewPatient k where
  ...

  _Initials :: Prism' (k Text) ()
  _Initials =
    let
      _NPKInitials :: Prism' (NewPatientKey Text) ()
      _NPKInitials = prism (const NPKInitials) $ \x -> 
        case x of
          NPKInitials -> Right ()
    in
      _NewPatient . _NPKInitials
      
  ...
```

## Classy prisms for keys

```haskell
class HasNewPatient k where
  ...

  _DOB :: Prism' (k Day) ()
  _DOB =
    let
      _NPKDOB :: Prism' (NewPatientKey Day) ()
      _NPKDOB = prism (const NPKDOB) $ \x -> 
        case x of
          NPKDOB -> Right ()
    in
      _NewPatient . _NPKDOB
```

## Classy prisms for keys

```haskell
instance HasNewPatient NewPatientKey where
  _NewPatient = id
```

```haskell
instance HasNewPatient DetailsKey where
  _NewPatient = prism DKNewPatient $ \x -> case x of
    DKNewPatient k -> Right k
    _ -> Left x
```

## Classy prisms for keys

```haskell
newPatientTest :: 
     (GCompare k, HasNewPatient k) 
  => DMap k Proxy
newPatientTest =
  DMap.fromList [
    _Initials # () :=> Proxy
  , _DOB      # () :=> Proxy
  ]
```

```haskell
isNewPatient :: 
     (EqTag k Proxy, GCompare k, HasNewPatient k)
  => DMap k f 
  -> Bool
isNewPatient =
  matchesDMap newPatientTest
```

# Making things more dynamic

## From `prim-uniq`

```haskell
newtype Tag m a = ...

instance GEq (Tag m) where ...
instance GCompare (Tag m) where ...
instance GShow (Tag m) where ...
```

```haskell
newTag :: PrimMonad m => m (Tag (PrimState m) a)
```

##

```haskell
data NoteEntryKey a where
  NEKNote :: (Eq a, Ord a, Show a, Read a) 
          => Tag (PrimState IO) a 
          -> NoteEntryKey a
```

```haskell
newtype Notes f = 
  Notes { getNotes :: DMap NoteEntryKey f }
```

##

```haskell
addNote :: 
    (Eq a, Ord a, Show a, Read a) 
  => a 
  ->     Notes Identity 
  -> IO (Notes Identity)
addNote a n = do





  
```

##

```haskell
addNote :: 
    (Eq a, Ord a, Show a, Read a) 
  => a 
  ->     Notes Identity 
  -> IO (Notes Identity)
addNote a n = do
  tag <- newTag




  
```

##

```haskell
addNote :: 
    (Eq a, Ord a, Show a, Read a) 
  => a 
  ->     Notes Identity 
  -> IO (Notes Identity)
addNote a n = do
  tag <- newTag
  let



  
```

##

```haskell
addNote :: 
    (Eq a, Ord a, Show a, Read a) 
  => a 
  ->     Notes Identity 
  -> IO (Notes Identity)
addNote a n = do
  tag <- newTag
  let
    dm  = getNotes n


  
```

##

```haskell
addNote :: 
    (Eq a, Ord a, Show a, Read a) 
  => a 
  ->     Notes Identity 
  -> IO (Notes Identity)
addNote a n = do
  tag <- newTag
  let
    dm  = getNotes n
    dm' = DMap.insert (NEKNote tag) (Identity a) dm

  
```

##

```haskell
addNote :: 
    (Eq a, Ord a, Show a, Read a) 
  => a 
  ->     Notes Identity 
  -> IO (Notes Identity)
addNote a n = do
  tag <- newTag
  let
    dm  = getNotes n
    dm' = DMap.insert (NEKNote tag) (Identity a) dm
    n'  = Notes dm'
  
```

##

```haskell
addNote :: 
    (Eq a, Ord a, Show a, Read a) 
  => a 
  ->     Notes Identity 
  -> IO (Notes Identity)
addNote a n = do
  tag <- newTag
  let
    dm  = getNotes n
    dm' = DMap.insert (NEKNote tag) (Identity a) dm
    n'  = Notes dm'
  pure n'
```

##

```haskell
noteExample :: IO ()
noteExample = do



  
```

##

```haskell
noteExample :: IO ()
noteExample = do
  let ns = Notes DMap.empty


  
```

##

```haskell
noteExample :: IO ()
noteExample = do
  let ns = Notes DMap.empty
  ns'  <- addNote (1 :: Int)      ns

  
```

##

```haskell
noteExample :: IO ()
noteExample = do
  let ns = Notes DMap.empty
  ns'  <- addNote (1 :: Int)      ns
  ns'' <- addNote (False :: Bool) ns'
  
```

##

```haskell
noteExample :: IO ()
noteExample = do
  let ns = Notes DMap.empty
  ns'  <- addNote (1 :: Int)      ns
  ns'' <- addNote (False :: Bool) ns'
  print ns''
```

##

```haskell
> noteExample
Notes {getNotes = fromList [
    NEKNote 0 :=> Identity 1
  , NEKNote 1 :=> Identity False
  ]}
```

# `constraints` and `constraints-extras`

##

```haskell
data Dict :: Constraint -> * where
  Dict :: a => Dict a
```

```haskell
withDict :: Dict a -> (a => r) -> r 
```

##

```haskell
myDictionary :: Dict (Eq X)
myFunction   :: Eq X => Y

result :: Y
result = 
  withDict myDictionary myFunction
```

##

```haskell
newtype a :- b
```

```haskell
Ord a :- Eq a
```

```haskell
(\\) :: a => (b => r) -> (a :- b) -> r 
```

##

```haskell
instF     :: forall p f a. ForallF p f :- p (f a) 
```

```haskell
whichever :: forall c t a r. (ForallF c t) => (c (t a) => r) -> r
```

```haskell
--  ForallF ToJSON k -- For any (a), we have an instance (ToJSON (k a))
```

##

```haskell
class ArgDict f where
  type ConstraintsFor f (c :: k -> Constraint) :: Constraint
  argDict :: ConstraintsFor f c => f a -> Dict (c a)
```

```haskell
deriveArgDict ''PatientInformationTag
```

##

```haskell
type Has (c :: k -> Constraint) f = 
  (ArgDict f, ConstraintsFor f c)
```

```haskell
has :: forall c f a r. (Has c f) => f a -> (c a => r) -> r
```

```haskell
--  Has ToJSON k -- For any (k a), we have an instance (ToJSON a)
```

##

```haskell
type ConstraintsFor' f (c :: k -> Constraint) (g :: k' -> k) =
  ConstraintsFor f (ComposeC c g)
```

```haskell
type Has' (c :: k -> Constraint) f (g :: k' -> k) = 
  (ArgDict f, ConstraintsFor' f c g)
```

```haskell
has' :: forall c g f a r. (Has' c f g) => f a -> (c (g a) => r) -> r
```

```haskell
--  Has' ToJSON k f -- For any (k a), we have an instance (ToJSON (f a))
```

##

-- JSON example
--  Has ToJSON k -- Given a value of type (k a), we can obtain an instance (ToJSON a)
--  Has' ToJSON k f -- Given a value of type (k a), we can obtain an instance (ToJSON (f a))
--  ForallF ToJSON k -- For any (a), we have an instance (ToJSON (k a))

```haskell
instance forall k f.
  ( Has' ToJSON k f -- Given a value of type (k a), we can obtain an instance (ToJSON (f a))
  , ForallF ToJSON k -- For any (a), we have an instance (ToJSON (k a))
  ) => ToJSON (DSum k f) where
  toJSON (DSum (k :: k a) f) = toJSON
    ( whichever @ToJSON @k @a $ toJSON k -- Use the (ForallF ToJSON k) constraint to obtain the (ToJSON (k a)) instance
    , has' @ToJSON @f k $ toJSON f -- Use the (Has' ToJSON k f) constraint to obtain the (ToJSON (f a)) instance
    )
```

# Vessel

##

```haskell
class View (v :: (* -> *) -> *) where
  condenseV :: 
    (Foldable t, FunctorMaybe t, Functor t) 
    => t (v g) -> v (Compose t g)
  disperseV :: 
    (Align t) 
    => v (Compose t g) -> t (v g)
  ...
```

```haskell
condenseV :: 
  Map Id (Details Maybe) -> Details (Compose (Map Id) Maybe)
```

```haskell
disperseV :: 
  Details (Compose (Map Id) Maybe) -> Map Id (Details Maybe)
```

##

```haskell
class View (v :: (* -> *) -> *) where
  ...
  cropV :: 
    (forall a. s a -> i a -> r a) -> v s -> v i -> v r
  nullV :: 
    v i -> Bool
  ...
```

##

```haskell
class View (v :: (* -> *) -> *) where
  ...
  mapV :: 
    (forall a. f a -> g a) -> v f -> v g
  traverseV :: 
    (Applicative m) 
    => (forall a. f a -> m (g a)) -> v f -> m (v g)
  mapMaybeV :: 
    (forall a. f a -> Maybe (g a)) -> v f -> Maybe (v g)
```

##

```haskell
newtype IdentityV (a :: *) (g :: * -> *) = 
  IdentityV { unIdentityV :: g a }
```

```haskell
instance View (IdentityV a) where
  ...
```

##

```haskell
newtype SingleV (a :: *) (g :: * -> *) = 
  SingleV { unSingleV :: g (First (Maybe a)) }
```

```haskell
instance View (SingleV a) where
  ...
```

##

```haskell
newtype MapV k v g = 
  MapV { unMapV :: MonoidalMap k (g v) }
```

```haskell
instance View (MapV k v)  where
  ...
```

##

```haskell
newtype DMapV (k :: * -> *) (v :: * -> *) g = 
  DMapV { unDMapV :: MonoidalDMap k (Compose g v) }
```

```haskell
instance View (DMapV k v)  where
  ...
```

##

```haskell
newtype Vessel (k :: ((* -> *) -> *) -> *) (g :: * -> *) = 
  Vessel { unVessel :: MonoidalDMap k (FlipAp g) }

newtype FlipAp (g :: k) (v :: k -> *) = 
  FlipAp { unFlipAp :: v g }
```

```haskell
instance (Has View k, GCompare k) => View (Vessel k) where
  ...
```

##

TODO show uses

##

```haskell
data DetailsKey a where
  DKInitials :: DetailsKey (IdentityV Text)
  DKDOB      :: DetailsKey (IdentityV Day)
  DKWeight   :: DetailsKey (SingleV Weight)
  DKDisease  :: DetailsKey (DMapV ExistingConditionKey Identity)
```

##

TODO validate, condense and disperse, query

##

```haskell
type ConstraintsForV (f :: (k -> k') -> *) (c :: k' -> Constraint) (g :: k) = 
  ConstraintsFor f (FlipC (ComposeC c) g)
```

```haskell
type HasV c f g = 
  (ArgDict f, ConstraintsForV f c g)
```

```haskell
hasV :: forall c g f v r. (HasV c f g) => f v -> (c (v g) => r) -> r
```

```haskell
--  HasV ToJSON k f -- For any (k v), we have an instance (ToJSON (v f))
```

##

```haskell
instance (GCompare k, ForallF ToJSON k, HasV ToJSON k g) => ToJSON (Vessel k g) where
  ...
instance (GCompare k, FromJSON (Some k), HasV FromJSON k g) => FromJSON (Vessel k g) where
  ...
```

# Applications

##

-- TODO

# Conclusion

##

-- TODO
