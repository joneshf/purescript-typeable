module Data.Typeable where

  import Data.Array () -- Needed for semigroup instance.
  import Data.Function
    ( Fn0()
    , Fn1()
    , Fn2()
    , Fn3()
    , Fn4()
    , Fn5()
    , Fn6()
    , Fn7()
    , Fn8()
    , Fn9()
    , Fn10()
    )

  data TypeRep = TypeRep TyCon [TypeRep]

  data TyCon = TyCon
    { tyConModule :: String
    , tyConName   :: String
    }

  instance eqTypeRep :: Eq TypeRep where
    (==) (TypeRep con reps) (TypeRep con' reps') = con == con' && reps == reps'
    (/=) rep                rep'                 = not (rep == rep')

  instance ordTypeRep :: Ord TypeRep where
    compare (TypeRep con reps) (TypeRep con' reps') =
      case compare con con' of
        EQ -> compare reps reps'
        c  -> c

  instance showTypeRep :: Show TypeRep where
    show (TypeRep con reps) = case reps of
      [] -> show con
      [x] | con == arrayTc -> "[" ++ show x ++ "]"
      [a, b] | con == arrTc -> "(" ++ show a ++ " -> " ++ show b ++ ")"

  instance eqTyCon :: Eq TyCon where
    (==) (TyCon {tyConModule = m, tyConName = n}) (TyCon {tyConModule = m', tyConName = n'}) = m == m' && n == n'
    (/=) con con' = not (con == con')

  instance ordTyCon :: Ord TyCon where
    compare (TyCon {tyConModule = m, tyConName = n}) (TyCon {tyConModule = m', tyConName = n'}) =
      case compare m m' of
        EQ -> compare n n'
        c  -> c

  instance showTyCon :: Show TyCon where
    show (TyCon {tyConName = name}) = name

  class Typeable a where
    typeOf :: a -> TypeRep

  class Typeable1 t where
    typeOf1 :: forall a. t a -> TypeRep

  class Typeable2 t where
    typeOf2 :: forall a b. t a b -> TypeRep

  class Typeable3 t where
    typeOf3 :: forall a b c. t a b c -> TypeRep

  class Typeable4 t where
    typeOf4 :: forall a b c d. t a b c d -> TypeRep

  class Typeable5 t where
    typeOf5 :: forall a b c d e. t a b c d e -> TypeRep

  class Typeable6 t where
    typeOf6 :: forall a b c d e f. t a b c d e f -> TypeRep

  class Typeable7 t where
    typeOf7 :: forall a b c d e f g. t a b c d e f g -> TypeRep

  class Typeable8 t where
    typeOf8 :: forall a b c d e f g h. t a b c d e f g h -> TypeRep

  class Typeable9 t where
    typeOf9 :: forall a b c d e f g h i. t a b c d e f g h i -> TypeRep

  class Typeable10 t where
    typeOf10 :: forall a b c d e f g h i j. t a b c d e f g h i j -> TypeRep

  class Typeable11 t where
    typeOf11 :: forall a b c d e f g h i j k. t a b c d e f g h i j k -> TypeRep

  instance typeableFromTypeable1 :: (Typeable1 t, Typeable a) => Typeable (t a) where
    typeOf = typeOfDefault

  instance typeable1FromTypeable2 :: (Typeable2 t, Typeable a) => Typeable1 (t a) where
    typeOf1 = typeOf1Default

  instance typeable2FromTypeable3 :: (Typeable3 t, Typeable a) => Typeable2 (t a) where
    typeOf2 = typeOf2Default

  instance typeable3FromTypeable4 :: (Typeable4 t, Typeable a) => Typeable3 (t a) where
    typeOf3 = typeOf3Default

  instance typeable4FromTypeable5 :: (Typeable5 t, Typeable a) => Typeable4 (t a) where
    typeOf4 = typeOf4Default

  instance typeable5FromTypeable6 :: (Typeable6 t, Typeable a) => Typeable5 (t a) where
    typeOf5 = typeOf5Default

  instance typeable6FromTypeable7 :: (Typeable7 t, Typeable a) => Typeable6 (t a) where
    typeOf6 = typeOf6Default

  instance typeable7FromTypeable8 :: (Typeable8 t, Typeable a) => Typeable7 (t a) where
    typeOf7 = typeOf7Default

  instance typeable8FromTypeable9 :: (Typeable9 t, Typeable a) => Typeable8 (t a) where
    typeOf8 = typeOf8Default

  instance typeable9FromTypeable10 :: (Typeable10 t, Typeable a) => Typeable9 (t a) where
    typeOf9 = typeOf9Default

  instance typeable9FromTypeable11 :: (Typeable11 t, Typeable a) => Typeable10 (t a) where
    typeOf10 = typeOf10Default

  -- Prim types.

  instance typeableBoolean :: Typeable Boolean where
    typeOf _ = mkTyRep "Prim" "Boolean"

  instance typeableNumber :: Typeable Number where
    typeOf _ = mkTyRep "Prim" "Number"

  instance typeableString :: Typeable String where
    typeOf _ = mkTyRep "Prim" "String"

  instance typeable1Array :: Typeable1 [] where
    typeOf1 _ = mkTyRep "Prim" "Array"

  instance typeable2Arr :: Typeable2 (->) where
    typeOf2 _ = TypeRep arrTc []

  -- Prelude types.

  instance typeableUnit :: Typeable Unit where
    typeOf _ = mkTyRep "Prelude" "Unit"

  instance typeableOrdering :: Typeable Ordering where
    typeOf _ = mkTyRep "Prelude" "Ordering"

  -- Data.Function types.

  instance typeable1Fn0 :: Typeable1 Fn0 where
    typeOf1 _ = mkTyRep "Data.Function" "Fn0"

  instance typeable2Fn1 :: Typeable2 Fn1 where
    typeOf2 _ = mkTyRep "Data.Function" "Fn1"

  instance typeable3Fn2 :: Typeable3 Fn2 where
    typeOf3 _ = mkTyRep "Data.Function" "Fn2"

  instance typeable4Fn3 :: Typeable4 Fn3 where
    typeOf4 _ = mkTyRep "Data.Function" "Fn3"

  instance typeable5Fn4 :: Typeable5 Fn4 where
    typeOf5 _ = mkTyRep "Data.Function" "Fn4"

  instance typeable6Fn5 :: Typeable6 Fn5 where
    typeOf6 _ = mkTyRep "Data.Function" "Fn5"

  instance typeable7Fn6 :: Typeable7 Fn6 where
    typeOf7 _ = mkTyRep "Data.Function" "Fn6"

  instance typeable8Fn7 :: Typeable8 Fn7 where
    typeOf8 _ = mkTyRep "Data.Function" "Fn7"

  instance typeable9Fn8 :: Typeable9 Fn8 where
    typeOf9 _ = mkTyRep "Data.Function" "Fn8"

  instance typeable10Fn9 :: Typeable10 Fn9 where
    typeOf10 _ = mkTyRep "Data.Function" "Fn9"

  instance typeable11Fn10 :: Typeable11 Fn10 where
    typeOf11 _ = mkTyRep "Data.Function" "Fn10"

  typeRepTyCon :: TypeRep -> TyCon
  typeRepTyCon (TypeRep con _) = con

  typeRepReps :: TypeRep -> [TypeRep]
  typeRepReps (TypeRep _ reps) = reps

  mkTyRep :: String -> String -> TypeRep
  mkTyRep mod name = TypeRep (TyCon {tyConModule: mod, tyConName: name}) []

  mkTyConApp :: TyCon -> [TypeRep] -> TypeRep
  mkTyConApp = TypeRep

  mkAppTy :: TypeRep -> TypeRep -> TypeRep
  mkAppTy (TypeRep con reps) arg = mkTyConApp con (reps ++ [arg])

  typeOfDefault :: forall t a. (Typeable1 t, Typeable a) => t a -> TypeRep
  typeOfDefault x = typeOf1 x `mkAppTy` typeOf (coerce x)
    where
      coerce :: forall t a. (Typeable1 t, Typeable a) => t a -> a
      coerce = unsafeCoerce

  typeOf1Default :: forall t a b. (Typeable2 t, Typeable a) => t a b -> TypeRep
  typeOf1Default x = typeOf2 x `mkAppTy` typeOf (coerce x)
    where
      coerce :: forall t a b. (Typeable2 t, Typeable a) => t a b -> a
      coerce = unsafeCoerce

  typeOf2Default :: forall t a b c. (Typeable3 t, Typeable a) => t a b c -> TypeRep
  typeOf2Default x = typeOf3 x `mkAppTy` typeOf (coerce x)
    where
      coerce :: forall t a b c. (Typeable3 t, Typeable a) => t a b c -> a
      coerce = unsafeCoerce

  typeOf3Default :: forall t a b c d. (Typeable4 t, Typeable a) => t a b c d -> TypeRep
  typeOf3Default x = typeOf4 x `mkAppTy` typeOf (coerce x)
    where
      coerce :: forall t a b c d. (Typeable4 t, Typeable a) => t a b c d -> a
      coerce = unsafeCoerce

  typeOf4Default :: forall t a b c d e. (Typeable5 t, Typeable a) => t a b c d e -> TypeRep
  typeOf4Default x = typeOf5 x `mkAppTy` typeOf (coerce x)
    where
      coerce :: forall t a b c d e. (Typeable5 t, Typeable a) => t a b c d e -> a
      coerce = unsafeCoerce

  typeOf5Default :: forall t a b c d e f. (Typeable6 t, Typeable a) => t a b c d e f -> TypeRep
  typeOf5Default x = typeOf6 x `mkAppTy` typeOf (coerce x)
    where
      coerce :: forall t a b c d e f. (Typeable6 t, Typeable a) => t a b c d e f -> a
      coerce = unsafeCoerce

  typeOf6Default :: forall t a b c d e f g. (Typeable7 t, Typeable a) => t a b c d e f g -> TypeRep
  typeOf6Default x = typeOf7 x `mkAppTy` typeOf (coerce x)
    where
      coerce :: forall t a b c d e f g. (Typeable7 t, Typeable a) => t a b c d e f g -> a
      coerce = unsafeCoerce

  typeOf7Default :: forall t a b c d e f g h. (Typeable8 t, Typeable a) => t a b c d e f g h -> TypeRep
  typeOf7Default x = typeOf8 x `mkAppTy` typeOf (coerce x)
    where
      coerce :: forall t a b c d e f g h. (Typeable8 t, Typeable a) => t a b c d e f g h -> a
      coerce = unsafeCoerce

  typeOf8Default :: forall t a b c d e f g h i. (Typeable9 t, Typeable a) => t a b c d e f g h i -> TypeRep
  typeOf8Default x = typeOf9 x `mkAppTy` typeOf (coerce x)
    where
      coerce :: forall t a b c d e f g h i. (Typeable9 t, Typeable a) => t a b c d e f g h i -> a
      coerce = unsafeCoerce

  typeOf9Default :: forall t a b c d e f g h i j. (Typeable10 t, Typeable a) => t a b c d e f g h i j -> TypeRep
  typeOf9Default x = typeOf10 x `mkAppTy` typeOf (coerce x)
    where
      coerce :: forall t a b c d e f g h i j. (Typeable10 t, Typeable a) => t a b c d e f g h i j -> a
      coerce = unsafeCoerce

  typeOf10Default :: forall t a b c d e f g h i j k. (Typeable11 t, Typeable a) => t a b c d e f g h i j k -> TypeRep
  typeOf10Default x = typeOf11 x `mkAppTy` typeOf (coerce x)
    where
      coerce :: forall t a b c d e f g h i j k. (Typeable11 t, Typeable a) => t a b c d e f g h i j k -> a
      coerce = unsafeCoerce

  arrayTc :: TyCon
  arrayTc = typeRepTyCon (typeOf [unit])

  arrTc :: TyCon
  arrTc = TyCon {tyConModule: "Prim", tyConName: "Arr"}

  foreign import unsafeCoerce
    "function unsafeCoerce(x) {\
    \ return x;\
    \}" :: forall a b. a -> b
