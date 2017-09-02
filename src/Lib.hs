{-# LANGUAGE DeriveGeneric #-}
module Lib
  ( readPacket
  , writeMessage
  , readMessage
  , fromFile
  , test
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.State
import           Data.Binary.Put
import           Data.Binary.Strict.Get
import           Data.Binary.Strict.Util
import           Data.Bits
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as HM
import           Data.Int
import qualified Data.List                   as List
import           Data.Word
import           Data.Word
import           Network.Socket
import qualified Network.Socket.ByteString   as S
import           System.IO

data Message = Message{ tx_id     :: Word16
                      , flags     :: Word16
                      , questions :: [Question]
                      , answers   :: [ResourceRecord]
                      } deriving (Show)

data Question = Question { name   :: [B.ByteString]
                         , qtype  :: Word16
                         , qclass :: Word16
                         } deriving (Show)

data ResourceRecord = ResourceRecord { rname  :: [B.ByteString]
                                     , rtype  :: Word16
                                     , rclass :: Word16
                                     , rttl   :: Int32
                                     , rdata  :: ResourceData
                                     } deriving (Show)

type ResourceRecordCache = HM.HashMap [B.ByteString] [ResourceRecord]

data ResourceData =
    IPAddr (Word8, Word8, Word8, Word8)
  | Bytes B.ByteString deriving (Show)

type LabelMap = HM.HashMap [B.ByteString] Int

writeLabels :: [B.ByteString] -> StateT (LabelMap, Int) PutM ()
writeLabels labels = do
  (lm, pos) <- get
  case HM.lookup labels lm of
    Nothing -> do
      mapM Lib.putByteString labels
      lift $ putWord8 0
      put (HM.insert labels pos lm, pos + 1 )
    Just pos -> do
      putWord16 $ 0xc000 .|. (fromIntegral pos)

incPos :: Int -> StateT (LabelMap, Int) PutM()
incPos i = do
  modify' $ \(lm, pos) -> (lm, pos + i)

putWord16 :: Word16 -> StateT (LabelMap, Int) PutM()
putWord16 i = do
  lift $ putWord16be i
  incPos 2

putInt32 :: Int32 -> StateT (LabelMap, Int) PutM()
putInt32 i = do
  lift $ putInt32be i
  incPos 4

putByteString :: B.ByteString -> StateT (LabelMap, Int) PutM()
putByteString b = do
  lift $ putWord8 $ fromIntegral $ B.length b
  lift $ Data.Binary.Put.putByteString b
  incPos $ 1 + B.length b


writeMessage :: Message -> StateT (LabelMap, Int) PutM ()
writeMessage msg = do
  putWord16 $ tx_id msg
  putWord16 $ flags msg
  putWord16 $ fromIntegral $ length (questions msg)
  putWord16 $ fromIntegral $ length (answers msg)
  putWord16 0
  putWord16 0

  mapM_ (\q -> do
           writeLabels $ name q
           putWord16 $ qtype q
           putWord16 $ qclass q
       ) $ questions msg


  mapM_ (\r -> do
           writeLabels (rname r)
           putWord16 $ rtype r
           putWord16 $ rclass r
           putInt32 $ rttl r

           case (rdata r) of
             IPAddr (a, b, c, d) -> do
               putWord16 4
               lift $ putWord8 a
               lift $ putWord8 b
               lift $ putWord8 c
               lift $ putWord8 d
               incPos 4
             Bytes x ->
               Lib.putByteString x
       ) $ answers msg


readLabel = do
  n <- getWord8
  getByteString $ fromIntegral n

readOffset = do
  x <- getWord16be
  return $ fromIntegral $ x .&. 0x3fff

readEncodedString s pkt = do
    c <- lookAhead getWord8
    if c == 0
      then skip 1 >>= \_ -> return $ reverse s
      else if (c .&. 192) == 192
      then
             readOffset >>= \offset -> do
               let (x, _) = runGet (readEncodedString [] pkt) (B.drop offset pkt)
               case x of
                  Left x  -> fail $ "can't find offset: " ++ show offset
                  Right x -> return x
      else
             readLabel >>= \o -> readEncodedString (o:s) pkt

readIP = do
  x <- getWord8
  y <- getWord8
  w <- getWord8
  z <- getWord8

  return $ IPAddr (x,y,w,z)

readAnswer bytes = do
  name <- readEncodedString [] bytes
  rtype <- getWord16be
  rclass <- getWord16be
  ttl <- fromIntegral <$> getWord32be
  rd_len <- getWord16be

  rdata <- case rtype of
            1 -> do
              ip <- readIP
              return ip
            _ -> do
               b <- getByteString $ fromIntegral rd_len
               return $ Bytes b

  return $ ResourceRecord name rtype rclass ttl rdata


readQuestion bytes = do
  name <- readEncodedString [] bytes
  qtype <- getWord16be
  qclass <- getWord16be
  return $ Question name (fromIntegral qtype) (fromIntegral qclass)

readMessage bytes = do
  tx_id <- getWord16be
  flags <- getWord16be
  query_count <- getWord16be
  an_count <- getWord16be
  ns_count <- getWord16be
  ar_count <- getWord16be

  queries <- mapM (\_ -> readQuestion bytes) [1..query_count]
  answers <- mapM (\_ -> readAnswer bytes) [1..an_count]

  return $ Message tx_id flags queries answers

readPacket :: B.ByteString -> IO Message
readPacket bytes = do
  let r =  runGet (readMessage bytes) bytes
  case (fst r) of
    Left error -> fail error
    Right x    -> return x

writePacket :: Message -> IO BL.ByteString
writePacket m = do
  let r = evalStateT (writeMessage m) (HM.empty, 0)
  let x = runPut r
  return x

fromFile s = do
  b <- B.readFile s
  pkt <- readPacket b
  return pkt

test s = do
  p <- fromFile s
  b <- writePacket p
  hexDump (BL.toStrict b)
  p2 <- readPacket (BL.toStrict b)
  return p2

readBuffer bytes = do
  let (x, _) = runGet (readMessage bytes) bytes in
    case x of
      Left error -> fail error
      Right x    -> return x

forwardRequest :: B.ByteString -> IO (Maybe Message)
forwardRequest = undefined

handleRequest :: TVar ResourceRecordCache -> B.ByteString -> SockAddr -> IO ()
handleRequest cache bytes clientAddress = do
  let (msg, _) = runGet (readMessage bytes) bytes in
    case msg of
      Left err -> fail err
      Right x -> do
        c <- readTVarIO cache
        let resp = HM.lookup (name (head (questions x))) c in
          case resp of
            _ -> do return ()
        return ()

mainLoop :: TVar ResourceRecordCache -> Socket -> IO ()
mainLoop cache sock = do
  (bytes, addr) <- S.recvFrom sock 512

  forkIO $ (handleRequest cache bytes addr)

  mainLoop cache sock

listenAndServe :: IO ()
listenAndServe = do
  sock <- socket AF_INET Datagram 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 5353 iNADDR_ANY)
  cache <- newTVarIO HM.empty
  x <- mainLoop cache sock
  return ()
