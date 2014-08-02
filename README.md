# Module Documentation

## Module Data.Typeable

### Type Classes

    class Typeable a where
      typeOf :: a -> String

    class Typeable1 t where
      typeOf1 :: forall a. t a -> String

    class Typeable2 t where
      typeOf2 :: forall a b. t a b -> String

    class Typeable3 t where
      typeOf3 :: forall a b c. t a b c -> String

    class Typeable4 t where
      typeOf4 :: forall a b c d. t a b c d -> String

    class Typeable5 t where
      typeOf5 :: forall a b c d e. t a b c d e -> String


### Type Class Instances

    instance typeableArray :: Typeable1 Prim.Array

    instance typeableBoolean :: Typeable Prim.Boolean

    instance typeableEither :: Typeable2 Either

    instance typeableMaybe :: Typeable1 Maybe

    instance typeableNumber :: Typeable Prim.Number

    instance typeableString :: Typeable Prim.String

    instance typeableTuple :: Typeable2 Tuple



