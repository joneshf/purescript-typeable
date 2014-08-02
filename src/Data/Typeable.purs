module Data.Typeable where

  import Data.Either (Either())
  import Data.Maybe (Maybe())
  import Data.Tuple (Tuple())

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

  instance typeableNumber :: Typeable Number where
    typeOf _ = "Number"
  instance typeableBoolean :: Typeable Boolean where
    typeOf _ = "Boolean"
  instance typeableString :: Typeable String where
    typeOf _ = "String"
  instance typeableArray :: Typeable1 [] where
    typeOf1 _ = "Array"
  instance typeableEither :: Typeable2 Either where
    typeOf2 _ = "Either"
  instance typeableMaybe :: Typeable1 Maybe where
    typeOf1 _ = "Maybe"
  instance typeableTuple :: Typeable2 Tuple where
    typeOf2 _ = "Tuple"
