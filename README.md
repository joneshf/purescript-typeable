## Module Data.Typeable

#### `TypeRep`

``` purescript
data TypeRep
  = TypeRep TyCon (List TypeRep)
```

##### Instances
``` purescript
Eq TypeRep
Ord TypeRep
Show TypeRep
```

#### `TyCon`

``` purescript
data TyCon
  = TyCon { tyConModule :: String, tyConName :: String }
```

##### Instances
``` purescript
Eq TyCon
Ord TyCon
Show TyCon
```

#### `Typeable`

``` purescript
class Typeable a where
  typeOf :: (Proxy a) -> TypeRep
```

##### Instances
``` purescript
(Typeable1 t, Typeable a) => Typeable (t a)
Typeable Boolean
Typeable Int
Typeable Number
Typeable String
Typeable Unit
Typeable Ordering
```

#### `Typeable1`

``` purescript
class Typeable1 t where
  typeOf1 :: forall a. Proxy (t a) -> TypeRep
```

##### Instances
``` purescript
(Typeable2 t, Typeable a) => Typeable1 (t a)
Typeable1 Array
Typeable1 Fn0
```

#### `Typeable2`

``` purescript
class Typeable2 t where
  typeOf2 :: forall a b. Proxy (t a b) -> TypeRep
```

##### Instances
``` purescript
(Typeable3 t, Typeable a) => Typeable2 (t a)
Typeable2 Function
Typeable2 Function
```

#### `Typeable3`

``` purescript
class Typeable3 t where
  typeOf3 :: forall a b c. Proxy (t a b c) -> TypeRep
```

##### Instances
``` purescript
(Typeable4 t, Typeable a) => Typeable3 (t a)
Typeable3 Fn2
```

#### `Typeable4`

``` purescript
class Typeable4 t where
  typeOf4 :: forall a b c d. Proxy (t a b c d) -> TypeRep
```

##### Instances
``` purescript
(Typeable5 t, Typeable a) => Typeable4 (t a)
Typeable4 Fn3
```

#### `Typeable5`

``` purescript
class Typeable5 t where
  typeOf5 :: forall a b c d e. Proxy (t a b c d e) -> TypeRep
```

##### Instances
``` purescript
(Typeable6 t, Typeable a) => Typeable5 (t a)
Typeable5 Fn4
```

#### `Typeable6`

``` purescript
class Typeable6 t where
  typeOf6 :: forall a b c d e f. Proxy (t a b c d e f) -> TypeRep
```

##### Instances
``` purescript
(Typeable7 t, Typeable a) => Typeable6 (t a)
Typeable6 Fn5
```

#### `Typeable7`

``` purescript
class Typeable7 t where
  typeOf7 :: forall a b c d e f g. Proxy (t a b c d e f g) -> TypeRep
```

##### Instances
``` purescript
(Typeable8 t, Typeable a) => Typeable7 (t a)
Typeable7 Fn6
```

#### `Typeable8`

``` purescript
class Typeable8 t where
  typeOf8 :: forall a b c d e f g h. Proxy (t a b c d e f g h) -> TypeRep
```

##### Instances
``` purescript
(Typeable9 t, Typeable a) => Typeable8 (t a)
Typeable8 Fn7
```

#### `Typeable9`

``` purescript
class Typeable9 t where
  typeOf9 :: forall a b c d e f g h i. Proxy (t a b c d e f g h i) -> TypeRep
```

##### Instances
``` purescript
(Typeable10 t, Typeable a) => Typeable9 (t a)
Typeable9 Fn8
```

#### `Typeable10`

``` purescript
class Typeable10 t where
  typeOf10 :: forall a b c d e f g h i j. Proxy (t a b c d e f g h i j) -> TypeRep
```

##### Instances
``` purescript
(Typeable11 t, Typeable a) => Typeable10 (t a)
Typeable10 Fn9
```

#### `Typeable11`

``` purescript
class Typeable11 t where
  typeOf11 :: forall a b c d e f g h i j k. Proxy (t a b c d e f g h i j k) -> TypeRep
```

##### Instances
``` purescript
Typeable11 Fn10
```

#### `typeRepTyCon`

``` purescript
typeRepTyCon :: TypeRep -> TyCon
```

#### `typeRepReps`

``` purescript
typeRepReps :: TypeRep -> List TypeRep
```

#### `mkTyRep`

``` purescript
mkTyRep :: String -> String -> TypeRep
```

#### `mkTyConApp`

``` purescript
mkTyConApp :: TyCon -> List TypeRep -> TypeRep
```

#### `mkAppTy`

``` purescript
mkAppTy :: TypeRep -> TypeRep -> TypeRep
```

#### `typeOfDefault`

``` purescript
typeOfDefault :: forall t a. (Typeable1 t, Typeable a) => Proxy (t a) -> TypeRep
```

#### `typeOf1Default`

``` purescript
typeOf1Default :: forall t a b. (Typeable2 t, Typeable a) => Proxy (t a b) -> TypeRep
```

#### `typeOf2Default`

``` purescript
typeOf2Default :: forall t a b c. (Typeable3 t, Typeable a) => Proxy (t a b c) -> TypeRep
```

#### `typeOf3Default`

``` purescript
typeOf3Default :: forall t a b c d. (Typeable4 t, Typeable a) => Proxy (t a b c d) -> TypeRep
```

#### `typeOf4Default`

``` purescript
typeOf4Default :: forall t a b c d e. (Typeable5 t, Typeable a) => Proxy (t a b c d e) -> TypeRep
```

#### `typeOf5Default`

``` purescript
typeOf5Default :: forall t a b c d e f. (Typeable6 t, Typeable a) => Proxy (t a b c d e f) -> TypeRep
```

#### `typeOf6Default`

``` purescript
typeOf6Default :: forall t a b c d e f g. (Typeable7 t, Typeable a) => Proxy (t a b c d e f g) -> TypeRep
```

#### `typeOf7Default`

``` purescript
typeOf7Default :: forall t a b c d e f g h. (Typeable8 t, Typeable a) => Proxy (t a b c d e f g h) -> TypeRep
```

#### `typeOf8Default`

``` purescript
typeOf8Default :: forall t a b c d e f g h i. (Typeable9 t, Typeable a) => Proxy (t a b c d e f g h i) -> TypeRep
```

#### `typeOf9Default`

``` purescript
typeOf9Default :: forall t a b c d e f g h i j. (Typeable10 t, Typeable a) => Proxy (t a b c d e f g h i j) -> TypeRep
```

#### `typeOf10Default`

``` purescript
typeOf10Default :: forall t a b c d e f g h i j k. (Typeable11 t, Typeable a) => Proxy (t a b c d e f g h i j k) -> TypeRep
```

#### `arrayTc`

``` purescript
arrayTc :: TyCon
```

#### `funTc`

``` purescript
funTc :: TyCon
```


