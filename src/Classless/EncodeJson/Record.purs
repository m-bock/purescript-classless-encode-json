module Classless.EncodeJson.Record
  ( class Record
  , record
  , class GEncodeJson
  , gEncodeJson
  ) where

import Prelude

import Classless as Cls
import Data.Argonaut.Core (Json, fromObject)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object as FO
import Prim.Row (class Cons, class Union)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

class Record (spec :: Row Type) r | r -> spec where
  record :: { | spec } -> { | r } -> Json

instance encodeRecord ::
  ( GEncodeJson spec row list
  , RL.RowToList row list
  ) =>
  Record spec row where
  record spec rec = fromObject $ gEncodeJson spec rec (Proxy :: Proxy list)

class GEncodeJson (spec :: Row Type) (row :: Row Type) (list :: RL.RowList Type) | list row -> spec where
  gEncodeJson :: forall proxy. { | spec } -> Record row -> proxy list -> FO.Object Json

instance GEncodeJson () row RL.Nil where
  gEncodeJson _ _ _ = FO.empty

instance
  ( Cons field (value -> Json) specX spec
  , GEncodeJson specX row tail
  , IsSymbol field
  , Row.Cons field value tail' row
  , Union specX x spec
  ) =>
  GEncodeJson spec row (RL.Cons field value tail) where
  gEncodeJson spec row _ =
    do
      FO.insert
        (reflectSymbol _field)
        (encodeJson $ Record.get _field row)
        (gEncodeJson (Cls.pick spec) row (Proxy :: Proxy tail))
    where
    _field = Proxy :: Proxy field
    encodeJson =
      Record.get _field spec