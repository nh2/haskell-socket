{-# LANGUAGE TypeFamilies #-}
module System.Socket.Protocol.SCTP where

import Data.Word
import qualified Data.ByteString as BS

import Foreign.C.Types

import System.Socket.Address
import System.Socket.Type.STREAM
import System.Socket.Type.SEQPACKET
import System.Socket.Protocol
import System.Socket.Internal.Socket
import System.Socket.Internal.Msg
import System.Socket.Internal.MsgFlags

#include "sys/socket.h"
#include "netinet/in.h"
#include "netinet/sctp.h"

data SCTP

data SctpSndRcvInfo
   = SctpSndRcvInfo
     { sinfoStream     :: Word16
     , sinfoSSN        :: Word16
     , sinfoFlags      :: Word16
     , sinfoPPID       :: Word32
     , sinfoContext    :: Word32
     , sinfoTimeToLive :: Word32
     , sinfoTSN        :: Word32
     , sinfoCumTSN     :: Word32
     }
     deriving (Eq, Show)

instance Protocol  SCTP where
  protocolNumber _ = (#const IPPROTO_SCTP)

sctpSend :: Address a =>
  Socket a STREAM SCTP
  -> BS.ByteString
  -> MsgFlags
  -> IO ()
sctpSend = undefined

sctpRecv :: Address a =>
  Socket a STREAM SCTP
  -> Int
  -> MsgFlags
  -> IO BS.ByteString
sctpRecv = undefined

sctpSendMsg :: Address a =>
  Socket a SEQPACKET SCTP
  -> BS.ByteString
  -> a
  -> SctpPPID
  -> SctpFlags
  -> SctpStreamNo
  -> SctpTimeToLive
  -> SctpContext
  -> IO Int
sctpSendMsg = undefined

sctpRecvMsg :: Address a =>
  Socket a SEQPACKET SCTP
  -> Int
  -> IO (BS.ByteString, a, SctpSndRcvInfo, MsgFlags)
sctpRecvMsg = undefined

newtype SctpPPID
      = SctpPPID       Word32

newtype SctpStreamNo
      = SctpStreamNo   Word16

newtype SctpTimeToLive
      = SctpTimeToLive Word32

newtype SctpContext
      = SctpContext    Word32

newtype SctpFlags
      = SctpFlags      Word32
      deriving (Eq, Show)

sctpUNORDERED :: SctpFlags
sctpUNORDERED  = SctpFlags (#const SCTP_UNORDERED)

sctpADDR_OVER :: SctpFlags
sctpADDR_OVER  = SctpFlags (#const SCTP_ADDR_OVER)

sctpABORT     :: SctpFlags
sctpABORT      = SctpFlags (#const SCTP_ABORT)

sctpEOF       :: SctpFlags
sctpEOF        = SctpFlags (#const SCTP_EOF)


