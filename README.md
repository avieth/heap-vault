# A heterogeneous map keyed on stable names

Associate a value with any Haskell value by using its stable name.

The interface of a `HeapVault d` is as follows:

```Haskell
empty :: Proxy d -> IO (HeapVault d)

insert :: key -> HeapVaultImage d key -> HeapVault d -> IO ()

lookup :: key -> HeapVault d -> IO (Maybe (HeapVaultImage d key))

delete :: key -> HeapVault d -> IO (Maybe (HeapVaultImage d key))
```

Placing a key/value pair into a `HeapVault` holds neither the key nor the value
from garbage collection. If either is collected, the entry is removed by a
finalizer (if the runtime decides to run it).

Any type can be used as a key in this map, and the type of the associated value
is determined by `HeapVaultImage`, a type family proxied by the parameter `d` to
`HeapVault`. Some examples:

```Haskell
-- Every key maps to a value of the same type.
data HeapVaultImageId
type instance HeapVaultImage HeapVaultImageId t = t

-- Every key maps to a value of a fixed type.
data HeapVaultImageConst t
type instance HeapVaultImage (HeapCaultImageConst t) r = t
```
