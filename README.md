# Module Documentation

## Module Data.Typeable

### Types

    data TyCon where
      TyCon :: { tyConName :: String, tyConModule :: String } -> TyCon

    data TypeRep where
      TypeRep :: TyCon -> [TypeRep] -> TypeRep


### Type Classes

    class Typeable a where
      typeOf :: a -> TypeRep

    class Typeable1 t where
      typeOf1 :: forall a. t a -> TypeRep

    class Typeable10 t where
      typeOf10 :: forall a b c d e f g h i j. t a b c d e f g h i j -> TypeRep

    class Typeable11 t where
      typeOf11 :: forall a b c d e f g h i j k. t a b c d e f g h i j k -> TypeRep

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


### Type Class Instances

    instance eqTyCon :: Eq TyCon

    instance eqTypeRep :: Eq TypeRep

    instance ordTyCon :: Ord TyCon

    instance ordTypeRep :: Ord TypeRep

    instance showTyCon :: Show TyCon

    instance showTypeRep :: Show TypeRep

    instance typeable10Fn9 :: Typeable10 Fn9

    instance typeable11Fn10 :: Typeable11 Fn10

    instance typeable1Array :: Typeable1 Prim.Array

    instance typeable1Fn0 :: Typeable1 Fn0

    instance typeable1FromTypeable2 :: (Typeable2 t, Typeable a) => Typeable1 (t a)

    instance typeable2Arr :: Typeable2 Prim.Function

    instance typeable2Fn1 :: Typeable2 Fn1

    instance typeable2FromTypeable3 :: (Typeable3 t, Typeable a) => Typeable2 (t a)

    instance typeable3Fn2 :: Typeable3 Fn2

    instance typeable3FromTypeable4 :: (Typeable4 t, Typeable a) => Typeable3 (t a)

    instance typeable4Fn3 :: Typeable4 Fn3

    instance typeable4FromTypeable5 :: (Typeable5 t, Typeable a) => Typeable4 (t a)

    instance typeable5Fn4 :: Typeable5 Fn4

    instance typeable5FromTypeable6 :: (Typeable6 t, Typeable a) => Typeable5 (t a)

    instance typeable6Fn5 :: Typeable6 Fn5

    instance typeable6FromTypeable7 :: (Typeable7 t, Typeable a) => Typeable6 (t a)

    instance typeable7Fn6 :: Typeable7 Fn6

    instance typeable7FromTypeable8 :: (Typeable8 t, Typeable a) => Typeable7 (t a)

    instance typeable8Fn7 :: Typeable8 Fn7

    instance typeable8FromTypeable9 :: (Typeable9 t, Typeable a) => Typeable8 (t a)

    instance typeable9Fn8 :: Typeable9 Fn8

    instance typeable9FromTypeable10 :: (Typeable10 t, Typeable a) => Typeable9 (t a)

    instance typeable9FromTypeable11 :: (Typeable11 t, Typeable a) => Typeable10 (t a)

    instance typeableBoolean :: Typeable Boolean

    instance typeableFromTypeable1 :: (Typeable1 t, Typeable a) => Typeable (t a)

    instance typeableNumber :: Typeable Number

    instance typeableOrdering :: Typeable Ordering

    instance typeableString :: Typeable String

    instance typeableUnit :: Typeable Unit


### Values

    arrTc :: TyCon

    arrayTc :: TyCon

    mkAppTy :: TypeRep -> TypeRep -> TypeRep

    mkTyConApp :: TyCon -> [TypeRep] -> TypeRep

    mkTyRep :: String -> String -> TypeRep

    typeOf10Default :: forall t a b c d e f g h i j k. (Typeable11 t, Typeable a) => t a b c d e f g h i j k -> TypeRep

    typeOf1Default :: forall t a b. (Typeable2 t, Typeable a) => t a b -> TypeRep

    typeOf2Default :: forall t a b c. (Typeable3 t, Typeable a) => t a b c -> TypeRep

    typeOf3Default :: forall t a b c d. (Typeable4 t, Typeable a) => t a b c d -> TypeRep

    typeOf4Default :: forall t a b c d e. (Typeable5 t, Typeable a) => t a b c d e -> TypeRep

    typeOf5Default :: forall t a b c d e f. (Typeable6 t, Typeable a) => t a b c d e f -> TypeRep

    typeOf6Default :: forall t a b c d e f g. (Typeable7 t, Typeable a) => t a b c d e f g -> TypeRep

    typeOf7Default :: forall t a b c d e f g h. (Typeable8 t, Typeable a) => t a b c d e f g h -> TypeRep

    typeOf8Default :: forall t a b c d e f g h i. (Typeable9 t, Typeable a) => t a b c d e f g h i -> TypeRep

    typeOf9Default :: forall t a b c d e f g h i j. (Typeable10 t, Typeable a) => t a b c d e f g h i j -> TypeRep

    typeOfDefault :: forall t a. (Typeable1 t, Typeable a) => t a -> TypeRep

    typeRepReps :: TypeRep -> [TypeRep]

    typeRepTyCon :: TypeRep -> TyCon

    unsafeCoerce :: forall a b. a -> b



