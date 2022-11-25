module Classless.EncodeJson
  ( EncodeJson
  , array
  , boolean
  , char
  , either
  , int
  , maybe
  , module Exp
  , string
  , tuple
  ) where

import Classless.EncodeJson.Generic (sum, class Sum) as Exp
import Classless.EncodeJson.Record (record, class Record) as Exp
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Encoders as Arg
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

type EncodeJson a = a -> Json

int :: EncodeJson Int
int = Arg.encodeInt

string :: EncodeJson String
string = Arg.encodeString

char :: EncodeJson Char
char = Arg.encodeChar

boolean :: EncodeJson Boolean
boolean = Arg.encodeBoolean

maybe :: forall a. EncodeJson a -> EncodeJson (Maybe a)
maybe = Arg.encodeMaybe

array :: forall a. EncodeJson a -> EncodeJson (Array a)
array = Arg.encodeArray

either :: forall a b. EncodeJson a -> EncodeJson b -> EncodeJson (Either a b)
either = Arg.encodeEither

tuple :: forall a b. EncodeJson a -> EncodeJson b -> EncodeJson (Tuple a b)
tuple = Arg.encodeTuple