module Data.Typeable where

  import Type.Proxy (Proxy(..))
  import Data.List (List(..), snoc)
  import Data.Foldable (foldMap)

  import Data.Function.Uncurried
    ( Fn0
    , Fn2
    , Fn3
    , Fn4
    , Fn5
    , Fn6
    , Fn7
    , Fn8
    , Fn9
    , Fn10
    )

  import Prelude
    ( class Eq, (==)
    , class Show, show
    , Unit, class Ord
    , Ordering(..), compare
    , (<>), (&&), (<<<)
    )


  data TypeRep = TypeRep TyCon (List TypeRep)

  data TyCon = TyCon
    { tyConModule :: String
    , tyConName   :: String
    }

  instance eqTypeRep :: Eq TypeRep where
    eq (TypeRep con reps) (TypeRep con' reps') = con == con' && reps == reps'

  instance ordTypeRep :: Ord TypeRep where
    compare (TypeRep con reps) (TypeRep con' reps') =
      case compare con con' of
        EQ -> compare reps reps'
        c  -> c

  instance showTypeRep :: Show TypeRep where
    show (TypeRep con reps) =
      case reps of
        Nil ->
          show con

        Cons x Nil | con == arrayTc ->
          "[" <> show x <> "]"

        Cons a (Cons b Nil) | con == funTc ->
          "(" <> show a <> " -> " <> show b <> ")"

        _ ->
          "(" <> show con <> foldMap ((" " <> _) <<< show) reps <> ")"


  instance eqTyCon :: Eq TyCon where
    eq (TyCon {tyConModule: m, tyConName: n}) (TyCon {tyConModule: m', tyConName: n'}) = m == m' && n == n'

  instance ordTyCon :: Ord TyCon where
    compare (TyCon {tyConModule: m, tyConName: n}) (TyCon {tyConModule: m', tyConName: n'}) =
      case compare m m' of
        EQ -> compare n n'
        c  -> c

  instance showTyCon :: Show TyCon where
    show (TyCon {tyConName: name}) = name

  class Typeable a where
    typeOf :: (Proxy a) -> TypeRep

  class Typeable1 t where
    typeOf1 :: forall a. Proxy (t a) -> TypeRep

  class Typeable2 t where
    typeOf2 :: forall a b. Proxy (t a b) -> TypeRep

  class Typeable3 t where
    typeOf3 :: forall a b c. Proxy (t a b c) -> TypeRep

  class Typeable4 t where
    typeOf4 :: forall a b c d. Proxy (t a b c d) -> TypeRep

  class Typeable5 t where
    typeOf5 :: forall a b c d e. Proxy (t a b c d e) -> TypeRep

  class Typeable6 t where
    typeOf6 :: forall a b c d e f. Proxy (t a b c d e f) -> TypeRep

  class Typeable7 t where
    typeOf7 :: forall a b c d e f g. Proxy (t a b c d e f g) -> TypeRep

  class Typeable8 t where
    typeOf8 :: forall a b c d e f g h. Proxy (t a b c d e f g h) -> TypeRep

  class Typeable9 t where
    typeOf9 :: forall a b c d e f g h i. Proxy (t a b c d e f g h i) -> TypeRep

  class Typeable10 t where
    typeOf10 :: forall a b c d e f g h i j. Proxy (t a b c d e f g h i j) -> TypeRep

  class Typeable11 t where
    typeOf11 :: forall a b c d e f g h i j k. Proxy (t a b c d e f g h i j k) -> TypeRep

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

  instance typeableInt :: Typeable Int where
    typeOf _ = mkTyRep "Prim" "Int"

  instance typeableNumber :: Typeable Number where
    typeOf _ = mkTyRep "Prim" "Number"

  instance typeableString :: Typeable String where
    typeOf _ = mkTyRep "Prim" "String"

  instance typeable1Array :: Typeable1 Array where
    typeOf1 _ = mkTyRep "Prim" "Array"

  instance typeable2Arr :: Typeable2 (->) where
    typeOf2 _ = TypeRep funTc Nil

  -- Prelude types.

  instance typeableUnit :: Typeable Unit where
    typeOf _ = mkTyRep "Prelude" "Unit"

  instance typeableOrdering :: Typeable Ordering where
    typeOf _ = mkTyRep "Prelude" "Ordering"

  -- Data.Function types.

  instance typeable1Fn0 :: Typeable1 Fn0 where
    typeOf1 _ = mkTyRep "Data.Function" "Fn0"

  instance typeable2Fn1 :: Typeable2 (->) where
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

  typeRepReps :: TypeRep -> List TypeRep
  typeRepReps (TypeRep _ reps) = reps

  mkTyRep :: String -> String -> TypeRep
  mkTyRep mod name = TypeRep (TyCon {tyConModule: mod, tyConName: name}) Nil

  mkTyConApp :: TyCon -> List TypeRep -> TypeRep
  mkTyConApp = TypeRep

  mkAppTy :: TypeRep -> TypeRep -> TypeRep
  mkAppTy (TypeRep con reps) arg = mkTyConApp con (snoc reps arg)

  typeOfDefault :: forall t a. (Typeable1 t, Typeable a) => Proxy (t a) -> TypeRep
  typeOfDefault x = typeOf1 (Proxy :: Proxy (t a)) `mkAppTy` typeOf (Proxy :: Proxy a)
  --
  typeOf1Default :: forall t a b. (Typeable2 t, Typeable a) => Proxy (t a b) -> TypeRep
  typeOf1Default x = typeOf2 (Proxy :: Proxy (t a b)) `mkAppTy` typeOf (Proxy :: Proxy a)

  typeOf2Default :: forall t a b c. (Typeable3 t, Typeable a) => Proxy (t a b c) -> TypeRep
  typeOf2Default x = typeOf3 (Proxy :: Proxy (t a b c)) `mkAppTy` typeOf (Proxy :: Proxy a)

  typeOf3Default :: forall t a b c d. (Typeable4 t, Typeable a) => Proxy (t a b c d) -> TypeRep
  typeOf3Default x = typeOf4 (Proxy :: Proxy (t a b c d)) `mkAppTy` typeOf (Proxy :: Proxy a)

  typeOf4Default :: forall t a b c d e. (Typeable5 t, Typeable a) => Proxy (t a b c d e) -> TypeRep
  typeOf4Default x = typeOf5 (Proxy :: Proxy (t a b c d e)) `mkAppTy` typeOf (Proxy :: Proxy a)

  typeOf5Default :: forall t a b c d e f. (Typeable6 t, Typeable a) => Proxy (t a b c d e f) -> TypeRep
  typeOf5Default x = typeOf6 (Proxy :: Proxy (t a b c d e f)) `mkAppTy` typeOf (Proxy :: Proxy a)

  typeOf6Default :: forall t a b c d e f g. (Typeable7 t, Typeable a) => Proxy (t a b c d e f g) -> TypeRep
  typeOf6Default x = typeOf7 (Proxy :: Proxy (t a b c d e f g)) `mkAppTy` typeOf (Proxy :: Proxy a)

  typeOf7Default :: forall t a b c d e f g h. (Typeable8 t, Typeable a) => Proxy (t a b c d e f g h) -> TypeRep
  typeOf7Default x = typeOf8 (Proxy :: Proxy (t a b c d e f g h)) `mkAppTy` typeOf (Proxy :: Proxy a)

  typeOf8Default :: forall t a b c d e f g h i. (Typeable9 t, Typeable a) => Proxy (t a b c d e f g h i) -> TypeRep
  typeOf8Default x = typeOf9 (Proxy :: Proxy (t a b c d e f g h i)) `mkAppTy` typeOf (Proxy :: Proxy a)

  typeOf9Default :: forall t a b c d e f g h i j. (Typeable10 t, Typeable a) => Proxy (t a b c d e f g h i j) -> TypeRep
  typeOf9Default x = typeOf10 (Proxy :: Proxy (t a b c d e f g h i j)) `mkAppTy` typeOf (Proxy :: Proxy a)

  typeOf10Default :: forall t a b c d e f g h i j k. (Typeable11 t, Typeable a) => Proxy (t a b c d e f g h i j k) -> TypeRep
  typeOf10Default x = typeOf11 (Proxy :: Proxy (t a b c d e f g h i j k)) `mkAppTy` typeOf (Proxy :: Proxy a)

  arrayTc :: TyCon
  arrayTc = typeRepTyCon (typeOf (Proxy :: Proxy (Array Unit)))

  funTc :: TyCon
  funTc = TyCon {tyConModule: "Prim", tyConName: "Function"}
