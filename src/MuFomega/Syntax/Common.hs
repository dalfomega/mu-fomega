module MuFomega.Syntax.Common
  ( Var (..)
  , Builtin (..)
  , BinOp (..)
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Text (Text)

data Var = Var
  { varName :: Text
  , varIndex :: Word
  }
  deriving (Eq, Show)

instance NFData Var where
  rnf (Var name index) = rnf name `seq` rnf index

data Builtin
  = Natural
  | NaturalFold
  | NaturalSubtract
  | Type
  | Kind
  deriving (Eq, Show)

instance NFData Builtin where
  rnf b = b `seq` ()

data BinOp
  = Plus
  | Times
  deriving (Eq, Show)

instance NFData BinOp where
  rnf op = op `seq` ()
