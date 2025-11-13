{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Config
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Config.Default (
  defaultConfig,

  -- * Internal helpers for defining `Default` instances
  MissingDefaultException (..),
  getDefault,
) where

import Control.Exception (Exception (..), throw)
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import Data.Yaml qualified as Y
import GHC.Eventlog.Live.Otelcol.Config.Default.Raw (defaultConfigByteString)
import GHC.Eventlog.Live.Otelcol.Config.Types (Config)
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.Haskell.TH.Lift.Compat (Exp, Lift (..), Q)

{- |
Internal helper.
The default configuration.
-}
defaultConfig :: Config
defaultConfig = $(lift =<< Y.decodeThrow @Q @Config defaultConfigByteString)

{- |
__Warning:__ Compile-time only.

Internal helper.
This exception is thrown when a property is missing from the default configuration file.
-}
newtype MissingDefaultException
  = MissingDefaultException
  { accessors :: [String]
  }

instance Show MissingDefaultException where
  show :: MissingDefaultException -> String
  show e =
    "Missing property '" <> intercalate "." e.accessors <> "' in default configuration"

instance Exception MissingDefaultException

{- |
__Warning:__ Compile-time only.

Internal helper.
Get the default value under the given path of accessors and `lift` it.
-}
getDefault ::
  forall xs a b.
  (GetDefault xs a b, Lift b) =>
  a ->
  Q Exp
getDefault a = either throw lift (getDefaultEither @xs @a @b a)

{- |
__Warning:__ Compile-time only.

Internal helper.
Get the default value under the given accessor after calling a recursor.
This function is used to implement the `GetDefault` instances.
-}
getDefaultEither' ::
  forall x a b c.
  (KnownSymbol x, HasField x a (Maybe b)) =>
  (b -> Either MissingDefaultException c) ->
  a ->
  Either MissingDefaultException c
getDefaultEither' rec a =
  first addAccessor . maybe (Left $ MissingDefaultException []) rec $ getField @x a
 where
  addAccessor :: MissingDefaultException -> MissingDefaultException
  addAccessor e = MissingDefaultException{accessors = symbolVal (Proxy @x) : e.accessors}

{- |
__Warning:__ Compile-time only.

Internal helper.
This class guides the search for `getDefaultEither` functions from a list of accessors.
-}
class GetDefault (xs :: [Symbol]) (a :: Type) (b :: Type) where
  getDefaultEither :: a -> Either MissingDefaultException b

instance (KnownSymbol x, HasField x a (Maybe b)) => GetDefault (x ': '[]) a b where
  getDefaultEither :: a -> Either MissingDefaultException b
  getDefaultEither = getDefaultEither' @x Right
  {-# INLINE getDefaultEither #-}

instance (KnownSymbol x, HasField x a (Maybe b), GetDefault (y ': ys) b c) => GetDefault (x ': y ': ys) a c where
  getDefaultEither :: a -> Either MissingDefaultException c
  getDefaultEither = getDefaultEither' @x (getDefaultEither @(y ': ys))
  {-# INLINE getDefaultEither #-}
