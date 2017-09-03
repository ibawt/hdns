module Server
  ( listenAndServe) where

import           Lib

import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Bits
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import qualified Data.HashMap.Strict       as HM
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import qualified Network.Socket.ByteString as S
import           System.Random

type ResourceRecordCache = HM.HashMap [B.ByteString] [ResourceRecord]

makeQuery :: [B.ByteString] -> IO Message
makeQuery name = do
  txId <- randomIO
  return $ Message txId 256 [Question name 1 1] []

forwardRequest :: [B.ByteString] -> IO Message
forwardRequest name = do
  sock <- socket AF_INET Datagram 0
  pkt <- makeQuery name
  msg <- writePacket pkt
  i <- S.sendTo sock msg $ SockAddrInet 53 (tupleToHostAddress (8, 8, 8, 8))
  (resp, _) <- S.recvFrom sock 512
  readPacket resp

makeReply :: Message -> [ResourceRecord] -> Message
makeReply query = Message (txId query) (bit 15) (questions query)

handleRequest :: TVar ResourceRecordCache -> Socket -> B.ByteString -> SockAddr -> IO ()
handleRequest cache sock bytes clientAddress = do
  msg <- readPacket bytes
  c <- readTVarIO cache
  answers:_ <-
    mapM
      (\q ->
        case HM.lookup (name q) c of
          Nothing -> do
            pkt <- forwardRequest (name q)
            atomically $
              writeTVar cache $
              foldl
                (\c r -> HM.insertWith (++) (rname r) [r] c)
                c
                (answers pkt)
            return (answers pkt)
          Just x -> do
            putStrLn "Reading from cache"
            return x) $
    questions msg
  msg <- writePacket $ makeReply msg answers
  S.sendTo sock msg clientAddress
  return ()

mainLoop :: TVar ResourceRecordCache -> Socket -> IO ()
mainLoop cache sock = do
  (bytes, addr) <- S.recvFrom sock 512
  forkIO $ handleRequest cache sock bytes addr
  mainLoop cache sock

listenAndServe :: IO ()
listenAndServe = do
  sock <- socket AF_INET Datagram 0
  bind sock (SockAddrInet 5354 iNADDR_ANY)
  cache <- newTVarIO HM.empty
  x <- mainLoop cache sock
  return ()
