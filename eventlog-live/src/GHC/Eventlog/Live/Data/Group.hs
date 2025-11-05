{- |
Module      : GHC.Eventlog.Live.Data.Group
Description : Core machines for processing data in batches.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.Group (
  -- * GroupBy
  GroupBy (..),
  Group (..),
  GroupedBy,
  singleton,
  elems,
  groups,
) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable (..))
import Data.Kind (Type)
import Data.Semigroup (First (..), Last (..), Max (..), Min (..), Product (..), Sum (..))

{- |
This class defines the key to group by when aggregating.
-}
class (Hashable (Key a)) => GroupBy a where
  type Key a :: Type
  toKey :: a -> Key a

deriving newtype instance (GroupBy a) => GroupBy (First a)
deriving newtype instance (GroupBy a) => GroupBy (Last a)
deriving newtype instance (GroupBy a) => GroupBy (Max a)
deriving newtype instance (GroupBy a) => GroupBy (Min a)
deriving newtype instance (GroupBy a) => GroupBy (Product a)
deriving newtype instance (GroupBy a) => GroupBy (Sum a)

{- |
This type defines a set of groups, grouped by the key given by `GroupBy`.
-}
data GroupedBy a = (GroupBy a) => GroupedBy
  { groups :: HashMap (Key a) (Group a)
  }

{- |
Internal helper.
This type defines a group representative and the group size.
-}
data Group a = Group
  { representative :: !a
  , size :: !Word
  }
  deriving (Show, Functor, Foldable, Traversable)

{- |
Construct the singleton `GroupedBy`.
-}
singleton :: (GroupBy a) => a -> GroupedBy a
singleton a = GroupedBy{groups = M.singleton (toKey a) Group{representative = a, size = 1}}

{- |
Get all group representatives from a `GroupedBy`.
-}
elems :: GroupedBy a -> [a]
elems = fmap (.representative) . groups

{- |
Get all group representatives and sizes from a `GroupedBy`.
-}
groups :: GroupedBy a -> [Group a]
groups = M.elems . (.groups)

instance (Semigroup a) => Semigroup (Group a) where
  (<>) :: Group a -> Group a -> Group a
  x <> y =
    Group
      { representative = x.representative <> y.representative
      , size = x.size + y.size
      }

instance (Semigroup a, GroupBy a) => Semigroup (GroupedBy a) where
  (<>) :: GroupedBy a -> GroupedBy a -> GroupedBy a
  x <> y = GroupedBy{groups = M.unionWith (<>) x.groups y.groups}
