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
checkFilled formData = 
  traverseWithKey (fmap Identity) formData
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

data NewPatient 
data InSystem

data PatientInformation a =
    PatientDetails Text Day -- initials and dob
  | PatientId Int           -- id in the system
  deriving (Eq, Ord, Show)
```

## Phantom types

```haskell
newPatient :: 
     Text 
  -> Day 
  -> PatientInformation NewPatient
newPatient = 
  PatientDetails
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation NewPatient 
  -> IO (PatientInformation InSystem)
addPatientToSystem = _



  
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation NewPatient 
  -> IO (PatientInformation InSystem)
addPatientToSystem (PatientDetails initials dob) = do



  
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation NewPatient 
  -> IO (PatientInformation InSystem)
addPatientToSystem (PatientDetails initials dob) = do
  i <- actuallyAddThePatientDetails initials dob


  
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation NewPatient 
  -> IO (PatientInformation InSystem)
addPatientToSystem (PatientDetails initials dob) = do
  i <- actuallyAddThePatientDetails initials dob
  pure (PatientId i)

  
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation NewPatient 
  -> IO (PatientInformation InSystem)
addPatientToSystem (PatientDetails initials dob) = do
  i <- actuallyAddThePatientDetails initials dob
  pure (PatientId i)
addPatientToSystem (PatientId i) = 
  
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation NewPatient 
  -> IO (PatientInformation InSystem)
addPatientToSystem (PatientDetails initials dob) = do
  i <- actuallyAddThePatientDetails initials dob
  pure (PatientId i)
addPatientToSystem (PatientId i) = 
  pure (PatientId i)
```

## Phantom types

```haskell
addPatientToSystem ::
  ->     PatientInformation NewPatient 
  -> IO (PatientInformation InSystem)
addPatientToSystem (PatientDetails initials dob) = do
  i <- actuallyAddThePatientDetails initials dob
  pure (PatientId i)
addPatientToSystem (PatientId i) = 
  pure (PatientId i) -- awful ...
```

## GADT version

```haskell
data PatientInformation a where
  PatientDetails :: Text 
                 -> Day 
                 -> PatientInformation NewPatient
  PatientId      :: Int 
                 -> PatientInformation InSystem
```

## GADT version

```haskell
addPatientToSystem :: 
         PatientInformation NewPatient 
  -> IO (PatientInformation InSystem)
addPatientToSystem = _
  
```

## GADT version

```haskell
addPatientToSystem :: 
         PatientInformation NewPatient 
  -> IO (PatientInformation InSystem)
addPatientToSystem (PatientDetails initials dob) = 
  
```

## GADT version

```haskell
addPatientToSystem :: 
         PatientInformation NewPatient 
  -> IO (PatientInformation InSystem)
addPatientToSystem (PatientDetails initials dob) = 
  pure (PatientId 0)
```

## GADT version

```haskell
addPatientToSystem :: 
         PatientInformation NewPatient 
  -> IO (PatientInformation InSystem)
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
editFilled filledData =
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
editFilled filledData = 
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
editFilled filledData = 
  mapDetails (Just . runIdentity) filledData
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
checkFilled formData = 
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
checkFilled formData = 
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
checkFilled formData = 
  traverseDetails (fmap Identity) formData
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
validateDetails details =
  apDetails detailsFn details
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

##

There are also a few other classes for working with both the key and the value of a `DSum`.

##

```haskell
class GEq tag => EqTag tag f where
  eqTagged :: tag a -> tag a 
           -> f   a -> f   a 
           -> Bool
```

##

There is work underway to remove these classes, and we'll see how later on.

# DMap

## From `Data.Dependent.Map`

```haskell
data DMap k f = ...
```

## From `Data.Dependent.Map`

```haskell
toList   ::
                 DMap k f  -> [DSum k f]
fromList :: 
  GCompare k => [DSum k f] ->  DMap k f
```

## From `Data.Dependent.Map`

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

## From `Data.Dependent.Map`

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

## From `Data.Dependent.Map`

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

## From `Data.Dependent.Map`

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

## From `Data.Dependent.Map`

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

## From `Data.Dependent.Map`

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

```haskell
data DetailsKey a where
  DKInitials :: DetailsKey Text
  DKDOB      :: DetailsKey Day
  DKWeight   :: DetailsKey (Maybe Weight)
  DKDisease  :: DetailsKey (DMap ExistingConditionKey Identity)
```

```haskell
type Details f = DMap DetailsKey f
```

##

```haskell
newDetails :: 
  Details Maybe
newDetails =
  DMap.fromList [
    DKInitials :=> Nothing
  , DKDOB :=> Nothing
  , DKWeight :=> Nothing
  , DKDisease :=> Just DMap.empty
  ]
```

##

```haskell
checkFilled :: 
            Details Maybe 
  -> Maybe (Details Identity)
checkFilled formData =
  DMap.traverseWithKey (\_ -> fmap Identity) formData
```

```haskell
editFilled :: 
     Details Identity 
  -> Details Maybe
editFilled filledData =
  DMap.map (Just . runIdentity) filledData
```

##

```haskell
data DetailsError = 
    NotSet 
  | InitialsTooShort
  deriving (Eq, Ord, Show)
```

```haskell
newtype Validator x = 
  Validator { 
    runValidator :: 
                                    Maybe    x 
      -> Validation [DetailsError] (Identity x) 
  }
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
detailsValidator :: Details Validator
detailsValidator =
  DMap.fromList [
    DKInitials :=> Validator validateInitials
  , DKDOB      :=> Validator validateDOB
  , DKWeight   :=> Validator validateWeight
  , DKDisease  :=> Validator validateDisease
  ]
```

##

```haskell
validateDetails :: 
       Details Maybe 
  -> Validation 
      [DetailsError] 
      (Details Identity)
validateDetails details =
  DMap.traverseWithKey 
    (\_ -> fmap Identity) .
  DMap.intersectionWithKey 
    (\_ v m ->
                 runIdentity <$> runValidator v m)
    detailsValidator $
  details
```

##

```haskell
validateDetails :: 
        Details Maybe 
  -> Validation 
       (Details (Const [DetailsError])) 
       (Details Identity)
validateDetails details =
  DMap.traverseWithKey 
    (\_ -> fmap Identity) .
  DMap.intersectionWithKey 
    (\k v m -> first (DMap.singleton k . Const) $ 
                 runIdentity <$> runValidator v m)
    detailsValidator $
    details
```

## Design decision

Should you use `Maybe` to model optional fields, or use the partiality of the `Map`-like abstraction?

##

With `Maybe` to model optional fields:

```haskell
validateDetails :: 
                                Details Maybe 
  -> Validation [DetailsError] (Details Identity)
validateDetails details =
  DMap.traverseWithKey 
    (\_ -> fmap Identity) .
  DMap.intersectionWithKey 
    (\_ v m -> runIdentity <$> runValidator v m) 
    detailsValidator $
    details
    
  
```

##

With the partiality of the `Map`-like abstraction:

```haskell
validateDetails :: 
                                Details Maybe 
  -> Validation [DetailsError] (Details Identity)
validateDetails details =
  DMap.traverseWithKey 
    (\_ -> fmap Identity) .
  DMap.intersectionWithKey 
    (\_ v m -> runIdentity <$> runValidator v m) 
    detailsValidator $
    DMap.union 
      details 
      (DMap.map (const Nothing) detailsValidator)
```

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
injectNewPatient dm = 
  DMap.mapKeysMonotonic DKNewPatient dm
```

##

```haskell
testDMap :: 
     GCompare k
  => DMap k Proxy
  -> DMap k f
  -> DMap k Proxy
testDMap dmp dmf =
  DMap.intersectionWithKey (\_ p _ -> p) dmp dmf
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
  , NPKDOB      :=> Proxy
  ]
```


```haskell
isNewPatient :: 
     DMap DetailsKey f 
  -> Bool
isNewPatient dmf =
  matchesDMap newPatientTest dmf
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
isNewPatient dmf =
  matchesDMap newPatientTest dmf
```

## 

This provides an alternative way of describing unions without having to mess with the key type.

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
          => UTCTime
          -> Tag (PrimState IO) a 
          -> NoteEntryKey a
```

```haskell
type Notes f = DMap NoteEntryKey f
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
  utc <- getCurrentTime

   
```

##

```haskell
addNote :: 
    (Eq a, Ord a, Show a, Read a) 
  => a 
  ->     Notes Identity 
  -> IO (Notes Identity)
addNote a n = do
  utc <- getCurrentTime
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
  utc <- getCurrentTime
  tag <- newTag
  pure . DMap.insert (NEKNote utc tag) (Identity o) $ n
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
  let ns = DMap.empty


  
```

##

```haskell
noteExample :: IO ()
noteExample = do
  let ns = DMap.empty
  ns'  <- addNote (1 :: Int)      ns

  
```

##

```haskell
noteExample :: IO ()
noteExample = do
  let ns = DMap.empty
  ns'  <- addNote (1 :: Int)      ns
  ns'' <- addNote (False :: Bool) ns'
  
```

##

```haskell
noteExample :: IO ()
noteExample = do
  let ns = DMap.empty
  ns'  <- addNote (1 :: Int)      ns
  ns'' <- addNote (False :: Bool) ns'
  print ns''
```

##

```haskell
> noteExample
fromList [
    NEKNote 2019-05-10 03:27:49.405641731 UTC 0 :=> 
      Identity 1
  , NEKNote 2019-05-10 03:27:49.405647013 UTC 1 :=> 
     Identity False
  ]
```

# `constraints` and `constraints-extras`

##

```haskell
instance Show (DSum k f) where
  showsPrec n ((ka :: k a) :=> (fa :: f a)) = _
  
  
  
```

##

```haskell
instance Show (DSum k f) where
  showsPrec n ((ka :: k a) :=> (fa :: f a)) = _ -- hmm
  
  
  
```

##

Let's do some plumbing!

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
newtype a :- b -- ie Ord X :- Eq X
```

```haskell
(\\) :: 



  
```

##

```haskell
newtype a :- b -- ie Ord X :- Eq X
```

```haskell
(\\) :: 
  a 
  =>

  
```


##

```haskell
newtype a :- b -- ie Ord X :- Eq X
```

```haskell
(\\) :: 
  a 
  => (b => r) 
  ->
  
```

##

```haskell
newtype a :- b -- ie Ord X :- Eq X
```

```haskell
(\\) :: 
  a 
  => (b => r) 
  -> (a :- b) 
  ->
```

##

```haskell
newtype a :- b -- ie Ord X :- Eq X
```

```haskell
(\\) :: 
  a 
  => (b => r) 
  -> (a :- b) 
  -> r 
```

##

```haskell
newtype a :- b -- ie Ord X :- Eq X
```

```haskell
(\\) :: 
  a
  => (b => r) 
  -> (Ord X :- Eq X) 
  -> r
```

##

```haskell
newtype a :- b -- ie Ord X :- Eq X
```

```haskell
(\\) :: 
  Ord X 
  => (b => r) 
  -> (Ord X :- Eq X) 
  -> r
```

##

```haskell
newtype a :- b -- ie Ord X :- Eq X
```

```haskell
(\\) :: 
  Ord X 
  => (Eq X => r) 
  -> (Ord X :- Eq X) 
  -> r
```

##

```haskell
newtype a :- b -- ie Ord X :- Eq X
```

```haskell
(\\) :: 
  Ord X 
  => (Eq X => Y) 
  -> (Ord X :- Eq X) 
  -> Y
```

##

```haskell
-- from `constraints`
instF :: 
  forall c k a. 
  ForallF c k :- c (k a) 
```

```haskell
-- from `constraints-extras`
whichever :: 
  forall c k a r. 
     (ForallF c k) 
  => (c (k a) => r) 
  -> r
```

##

This
```haskell
deriving instance Show (DetailsKey a)
```
gives us a
```haskell
ForallF Show DetailsKey
```

##

```haskell
ForallF Show DetailsKey
```
means that for each 
```haskell
a
```
we have a 
```haskell
Show (DetailsKey a)
```

##

```haskell
instance
         Show (DSum k f) where
  showsPrec n ((ka :: k a) :=> (fa :: f a)) =
    _


   
```

##

```haskell
instance (ForallF Show k) 
      => Show (DSum k f) where
  showsPrec n ((ka :: k a) :=> (fa :: f a)) =
    _


   
```

##

```haskell
instance (ForallF Show k) 
      => Show (DSum k f) where
  showsPrec n ((ka :: k a) :=> (fa :: f a)) =
    whichever @Show @k @a (showsPrec n ka) .
    _

   
```

##

```haskell
instance (ForallF Show k) 
      => Show (DSum k f) where
  showsPrec n ((ka :: k a) :=> (fa :: f a)) =
    whichever @Show @k @a (showsPrec n ka) .
    showString " :=> " .
    _
   
```

##

```haskell
instance (ForallF Show k) 
      => Show (DSum k f) where
  showsPrec n ((ka :: k a) :=> (fa :: f a)) =
    whichever @Show @k @a (showsPrec n ka) .
    showString " :=> " .
    _ -- hmm
   
```

##

```haskell
class ArgDict k where
  type ConstraintsFor k (c :: k1 -> Constraint) :: Constraint
  argDict :: ConstraintsFor k c => k a -> Dict (c a)
```

```haskell
deriveArgDict ''PatientInformationTag
```

##

```haskell
type Has (c :: k1 -> Constraint) k = 
  (ArgDict k, ConstraintsFor k c)
```

```haskell
has :: 
  forall c k a r. 
     (Has c k) 
  => k a 
  -> (c a => r) 
  -> r
```

##

```haskell
Has Show DetailsKey
```
means that for each 
```haskell
DetailsKey a
```
we have a 
```haskell
Show a
```

##

```haskell
type ConstraintsFor' k (c :: k1 -> Constraint) (f :: k2 -> k1) =
  ConstraintsFor k (ComposeC c f)
```

```haskell
type Has' (c :: k1 -> Constraint) k (f :: k2 -> k1) = 
  (ArgDict k, ConstraintsFor' k c f)
```

```haskell
has' :: forall c f k a r. 
     (Has' c k f) 
  => k a 
  -> (c (f a) => r) 
  -> r
```

##

```haskell
Has' Show DetailsKey f
```
means that for each 
```haskell
DetailsKey a
```
we have a 
```haskell
Show (f a)
```

##

```haskell
instance (ForallF Show k               )
      => Show (DSum k f) where
  showsPrec n ((ka :: k a) :=> (fa :: f a)) =
    whichever @Show @k @a (showsPrec n ka) .
    showString " :=> " .
    _
```

##

```haskell
instance (ForallF Show k, Has' Show k f)
      => Show (DSum k f) where
  showsPrec n ((ka :: k a) :=> (fa :: f a)) =
    whichever @Show @k @a (showsPrec n ka) .
    showString " :=> " .
    _
```

##

```haskell
instance (ForallF Show k, Has' Show k f)
      => Show (DSum k f) where
  showsPrec n ((ka :: k a) :=> (fa :: f a)) =
    whichever @Show @k @a (showsPrec n ka) .
    showString " :=> " .
    has' @Show @f ka (showsPrec n fa)
```

##

```haskell
instance (ForallF Show k, Has' Show k f)
      => Show (DSum k f) where
  showsPrec n ((ka :: k a) :=> (fa :: f a)) =
    whichever @Show @k @a (showsPrec n ka) .
    showString " :=> " .
    has' @Show @f ka (showsPrec n fa) -- hah!
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

```haskell
data DetailsKey a where
  DKInitials :: DetailsKey (IdentityV Text)
  DKDOB      :: DetailsKey (IdentityV Day)
  DKWeight   :: DetailsKey (SingleV Weight)
  DKDisease  :: DetailsKey (DMapV ExistingConditionKey Identity)
```

```haskell
type Details f = Vessel DetailsKey f
```

##

```haskell
type ConstraintsForV (k :: (k1 -> k2) -> *) (c :: k2 -> Constraint) (f :: k1) = 
  ConstraintsFor k (FlipC (ComposeC c) f)
```

```haskell
type HasV c k f = 
  (ArgDict k, ConstraintsForV k c f)
```

```haskell
hasV :: 
  forall c f k v r. 
     (HasV c k f) 
  => k v 
  -> (c (v f) => r) 
  -> r
```

##

```haskell
instance (GCompare k, ForallF ToJSON k, HasV ToJSON k g) 
      => ToJSON (Vessel k g) where
  ...
instance (GCompare k, FromJSON (Some k), HasV FromJSON k g) 
      => FromJSON (Vessel k g) where
  ...
```

##

This can be used for cool things.

# Conclusion

##

The ecosystem occupies an interesting point in the design space.

##

"extensible records with all the operations you could ever hope for""

##

```haskell
flexibility :: Power -> Responsibility
```

##

Some of the best practices are still emerging.

##

Obsidian Systems is doing a bunch of work in this space.

##

Questions?
