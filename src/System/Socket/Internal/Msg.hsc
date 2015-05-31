{-# LANGUAGE TypeFamilies #-}
module System.Socket.Internal.Msg where

import qualified Data.ByteString.Lazy as LBS

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import System.Socket.Internal.MsgFlags

data IoVec

data Msg a c
   = Msg
     { msgIov     :: LBS.ByteString
     , msgName    :: Maybe a
     , msgControl :: c
     , msgFlags   :: MsgFlags
     }
     deriving (Eq, Show)

class Storable c => MsgControl c where
  msgControlLevel     :: c -> CInt
  msgControlType      :: c -> CInt
