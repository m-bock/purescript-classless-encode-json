module Classless.EncodeJson.Generic
  ( class EncodeRep
  , class EncodeRepArgs
  , class Sum
  , encodeRepWith
  , encodeRepArgs
  , sum
  ) where

import Prelude

import Classless (type (~), NoArgs(..), (~))
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString)
import Data.Generic.Rep (class Generic, NoArguments)
import Data.Generic.Rep as Rep
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object as FO
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons, class Lacks)
import Prim.TypeError (class Fail, Text)
import Record as Record
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

type Encoding =
  { tagKey :: String
  , valuesKey :: String
  , unwrapSingleArguments :: Boolean
  }

defaultEncoding :: Encoding
defaultEncoding =
  { tagKey: "tag"
  , valuesKey: "values"
  , unwrapSingleArguments: false
  }

class EncodeRep sumSpec r | r -> sumSpec where
  encodeRepWith :: { | sumSpec } -> Encoding -> r -> Json

instance EncodeRep () Rep.NoConstructors where
  encodeRepWith e = encodeRepWith e

instance
  ( TypeEquals a (Rep.Constructor name xx)
  , Cons name x sumSpec' sumSpec
  , Cons name x () sumSpec''
  , EncodeRep sumSpec'' a
  , EncodeRep sumSpec' b
  , IsSymbol name
  , Lacks name sumSpec'
  ) =>
  EncodeRep sumSpec (Rep.Sum a b) where
  encodeRepWith sp e (Rep.Inl a) = encodeRepWith (Record.insert (Proxy :: _ name) (Record.get (Proxy :: _ name) sp) {}) e a
  encodeRepWith sp e (Rep.Inr b) = encodeRepWith (Record.delete (Proxy :: _ name) sp) e b

instance
  ( IsSymbol name
  , Cons name NoArgs () sumSpec
  ) =>
  EncodeRep sumSpec (Rep.Constructor name NoArguments) where
  encodeRepWith _ e (Rep.Constructor a) =
    fromObject
      $ FO.insert e.tagKey (fromString (reflectSymbol (Proxy :: Proxy name)))
      $ FO.insert e.valuesKey values
      $ FO.empty
    where
    values = fromArray []

else instance
  ( IsSymbol name
  , EncodeRepArgs prodSpec a
  , Cons name prodSpec () sumSpec
  ) =>
  EncodeRep sumSpec (Rep.Constructor name a) where
  encodeRepWith sumSpec e (Rep.Constructor a) =
    fromObject
      $ FO.insert e.tagKey (fromString (reflectSymbol (Proxy :: Proxy name)))
      $ FO.insert e.valuesKey values
      $ FO.empty
    where
    values =
      let
        prodSpec = Record.get (Proxy :: _ name) sumSpec
        vs = encodeRepArgs prodSpec a
      in
        if e.unwrapSingleArguments then case vs of
          [ v ] -> v
          _ -> fromArray vs
        else fromArray vs

class EncodeRepArgs prodSpec r | r -> prodSpec where
  encodeRepArgs :: prodSpec -> r -> Array Json

instance EncodeRepArgs NoArgs Rep.NoArguments where
  encodeRepArgs _ Rep.NoArguments = []

instance (EncodeRepArgs sa a, EncodeRepArgs sb b) => EncodeRepArgs (sa ~ sb) (Rep.Product a b) where
  encodeRepArgs (spA ~ spB) (Rep.Product a b) = encodeRepArgs spA a <> encodeRepArgs spB b

instance EncodeRepArgs (a -> Json) (Rep.Argument a) where
  encodeRepArgs spec (Rep.Argument a) = [ spec a ]

class Sum sumSpec a | a -> sumSpec where
  sum :: { | sumSpec } -> (a -> Json)

instance (Generic a rep, EncodeRep sumSpec rep) => Sum sumSpec a where
  sum spec = sumWith spec defaultEncoding

sumWith :: forall sumSpec a r. Rep.Generic a r => EncodeRep sumSpec r => { | sumSpec } -> Encoding -> a -> Json
sumWith spec e = encodeRepWith spec e <<< Rep.from

class EncodeLiteral r where
  encodeLiteral :: (String -> String) -> r -> Json

instance (EncodeLiteral a, EncodeLiteral b) => EncodeLiteral (Rep.Sum a b) where
  encodeLiteral tagNameTransform (Rep.Inl a) = encodeLiteral tagNameTransform a
  encodeLiteral tagNameTransform (Rep.Inr b) = encodeLiteral tagNameTransform b

instance (IsSymbol name) => EncodeLiteral (Rep.Constructor name Rep.NoArguments) where
  encodeLiteral tagNameTransform _ = fromString <<< tagNameTransform $ reflectSymbol (Proxy :: Proxy name)

type FailMessage =
  Text """`encodeLiteralSum` can only be used with sum types, where all of the constructors are nullary. This is because a string literal cannot be encoded into a product type."""

instance
  Fail FailMessage =>
  EncodeLiteral (Rep.Product a b) where
  encodeLiteral _ _ = unsafeCrashWith "unreachable encodeLiteral was reached."
