module Test.Classless.EncodeJson where

import Prelude

import Classless (class Init, class InitRecord, class InitSum, initRecord, initSum, noArgs, (~))
import Classless.EncodeJson (EncodeJson)
import Classless.EncodeJson as Enc
import Data.Argonaut (Json)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Record as Record

type Items = Array
  ( Either
      String
      (Tuple Int Boolean)
  )

encItems'1 :: EncodeJson Items
encItems'1 = Enc.array
  ( Enc.either
      Enc.string
      (Enc.tuple Enc.int Enc.boolean)
  )

type User =
  { name :: String
  , age :: Int
  , loggedIn :: Boolean
  , coordinates :: Array { x :: Int, y :: Int }
  }

encUser'1 :: EncodeJson User
encUser'1 = Enc.record
  { name: Enc.string
  , age: Enc.int
  , loggedIn: Enc.boolean
  , coordinates: Enc.array $ Enc.record
      { x: Enc.int
      , y: Enc.int
      }
  }

data RemoteData
  = NotAsked
  | Loading Int Int Int
  | Error String
  | Success
      { status :: Int
      , body :: String
      }

derive instance Generic RemoteData _

encRemoteData'1 :: EncodeJson RemoteData
encRemoteData'1 = Enc.sum
  { "NotAsked": noArgs
  , "Loading": Enc.int ~ Enc.int ~ Enc.int
  , "Error": Enc.string
  , "Success": Enc.record
      { status: Enc.int
      , body: Enc.string
      }
  }

class MyEncodeJson a where
  encodeJson :: a -> Json

instance MyEncodeJson Int where
  encodeJson = Enc.int

instance MyEncodeJson String where
  encodeJson = Enc.string

instance MyEncodeJson Boolean where
  encodeJson = Enc.boolean

instance (MyEncodeJson a) => MyEncodeJson (Array a) where
  encodeJson = Enc.array encodeJson

instance (MyEncodeJson a) => MyEncodeJson (Maybe a) where
  encodeJson = Enc.maybe encodeJson

instance (MyEncodeJson a, MyEncodeJson b) => MyEncodeJson (Either a b) where
  encodeJson = Enc.either encodeJson encodeJson

instance (MyEncodeJson a, MyEncodeJson b) => MyEncodeJson (Tuple a b) where
  encodeJson = Enc.tuple encodeJson encodeJson

data MyInit = MyInit

instance (MyEncodeJson a) => Init MyInit (EncodeJson a) where
  init _ = encodeJson

instance (Enc.Record r' r, InitRecord MyInit r') => MyEncodeJson (Record r) where
  encodeJson = Enc.record $ initRecord MyInit

genericSum :: forall r' a. Enc.Sum r' a => InitSum MyInit r' => EncodeJson a
genericSum = Enc.sum $ initSum MyInit

encItems'2 :: EncodeJson Items
encItems'2 = encodeJson

encUser'2 :: EncodeJson User
encUser'2 = encodeJson

encRemoteData'2 :: EncodeJson RemoteData
encRemoteData'2 = genericSum

encUser'3 :: EncodeJson User
encUser'3 = Enc.record
  $ Record.union
      { age: Enc.int
      }
  $ initRecord MyInit

encAB :: EncodeJson { a :: Int, b :: Char }
encAB = Enc.record
  $ Record.union
      { b: Enc.char
      }
  $ initRecord MyInit

encRemoteData'3 :: EncodeJson RemoteData
encRemoteData'3 = Enc.sum
  $ Record.union
      { "Error": Enc.string
      }
  $ initSum MyInit

encRemoteData'4 :: EncodeJson RemoteData
encRemoteData'4 = Enc.sum
  $ Record.union
      { "Loading": encodeJson ~ encodeJson ~ Enc.int
      }
  $ initSum MyInit

encRemoteData'5 :: EncodeJson RemoteData
encRemoteData'5 = Enc.sum
  $ Record.union
      { "Success": Enc.record
          $ Record.union
              { status: Enc.int
              }
          $ initRecord MyInit
      }
  $ initSum MyInit
