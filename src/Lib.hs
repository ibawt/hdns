{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( readPacket
  , writePacket
  , Message(..)
  , isAuthoritative
  , isQuery
  , opCode
  , OpCode
  , isTruncated
  , isRecursionAvailable
  , isRecursionDesired
  , getQuestions
  , returnCode
  , getName
  , ResourceRecord(..)
  , Question(..)
  ) where

import           Control.Monad.State
import           Data.Binary.Put
import           Data.Binary.Strict.Get
import           Data.Binary.Strict.Util
import           Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as HM
import           Data.Int
import           Data.Word

data Message = Message
  { txId      :: Word16
  , flags     :: Word16
  , questions :: [Question]
  , answers   :: [ResourceRecord]
  } deriving (Show, Eq)

data Question = Question
  { name   :: [B.ByteString]
  , qtype  :: Word16
  , qclass :: Word16
  } deriving (Show, Eq)

data ResourceRecord = ResourceRecord
  { rname  :: [B.ByteString]
  , rtype  :: Word16
  , rclass :: Word16
  , rttl   :: Int32
  , rdata  :: ResourceData
  } deriving (Show, Eq)

data ResourceData
  = IPAddr (Word8, Word8, Word8, Word8)
  | Bytes B.ByteString
  deriving (Show, Eq)

type LabelMap = HM.HashMap [B.ByteString] Int

isQuery :: Message -> Bool
isQuery m = not $ testBit (flags m) 15

data OpCode
 = Query
 | IQuery
 | Status
 | Invalid deriving (Show,Eq)

opCode :: Message ->  OpCode
opCode m = case shiftL (flags m .&. 0x7800) 11 of
  0 -> Query
  1 -> IQuery
  2 -> Status
  _ -> Invalid

isAuthoritative :: Message -> Bool
isAuthoritative m = testBit (flags m) 10

isTruncated :: Message -> Bool
isTruncated m = testBit (flags m) 9

isRecursionDesired :: Message -> Bool
isRecursionDesired m = testBit (flags m) 8

isRecursionAvailable :: Message -> Bool
isRecursionAvailable m = testBit (flags m) 7

data ReturnCode
  = NoError
  | FormatError
  | ServerFailure
  | NameError
  | NotImplemented
  | Refused deriving (Show,Eq)

returnCode :: Message -> ReturnCode
returnCode m = case flags m .&. 0xf of
  0 -> NoError
  1 -> FormatError
  2 -> ServerFailure
  3 -> NameError
  4 -> NotImplemented
  5 -> Refused

showHeader :: Message -> IO ()
showHeader m = do
  putStrLn "-----------------------------------"
  putStrLn $ "isQuery: " ++ show (isQuery m)
  putStrLn $ "OpCode: " ++ show (opCode m)
  putStrLn $ "AA: " ++ show (isAuthoritative m)
  putStrLn $ "TC: " ++ show (isTruncated m)
  putStrLn $ "RD: " ++ show (isRecursionDesired m)
  putStrLn $ "RA: " ++ show (isRecursionAvailable m)
  putStrLn $ "RCODE: " ++ show (returnCode m)

getQuestions = questions
getName = name

writeLabels :: [B.ByteString] -> StateT (LabelMap, Int) PutM ()
writeLabels labels = do
  (lm, pos) <- get
  case HM.lookup labels lm of
    Nothing -> do
      mapM_ Lib.putByteString labels
      lift $ putWord8 0
      put (HM.insert labels pos lm, pos + 1)
    Just pos -> putWord16 $ 0xc000 .|. fromIntegral pos

incPos :: Int -> StateT (LabelMap, Int) PutM()
incPos i = modify' $ \(lm, pos) -> (lm, pos + i)

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
  putWord16 $ txId msg
  putWord16 $ flags msg
  putWord16 $ fromIntegral $ length (questions msg)
  putWord16 $ fromIntegral $ length (answers msg)
  putWord16 0
  putWord16 0
  mapM_
    (\q -> do
       writeLabels $ name q
       putWord16 $ qtype q
       putWord16 $ qclass q) $
    questions msg
  mapM_
    (\r -> do
       writeLabels (rname r)
       putWord16 $ rtype r
       putWord16 $ rclass r
       putInt32 $ rttl r
       case rdata r of
         IPAddr (a, b, c, d) -> do
           putWord16 4
           lift $ putWord8 a
           lift $ putWord8 b
           lift $ putWord8 c
           lift $ putWord8 d
           incPos 4
         Bytes x -> Lib.putByteString x) $
    answers msg


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
    else if (c .&. 0xc0) == 0xc0
           then readOffset >>= \offset -> do
                  let (x, _) =
                        runGet (readEncodedString [] pkt) (B.drop offset pkt)
                  case x of
                    Left x  -> fail $ "can't find offset: " ++ show offset
                    Right x -> return x
           else readLabel >>= \o -> readEncodedString (o : s) pkt

readIP = do
  x <- getWord8
  y <- getWord8
  w <- getWord8
  z <- getWord8
  return $ IPAddr (x, y, w, z)

readAnswer bytes = do
  name <- readEncodedString [] bytes
  rtype <- getWord16be
  rclass <- getWord16be
  ttl <- fromIntegral <$> getWord32be
  rd_len <- getWord16be
  rdata <-
    case rtype of
      1 -> readIP
      _ -> do
        b <- getByteString $ fromIntegral rd_len
        return $ Bytes b
  return $ ResourceRecord name rtype rclass ttl rdata

readQuestion bytes = do
  name <- readEncodedString [] bytes
  qtype <- getWord16be
  qclass <- getWord16be
  return $ Question name qtype qclass

readMessage bytes = do
  txId <- getWord16be
  flags <- getWord16be
  query_count <- getWord16be
  an_count <- getWord16be
  ns_count <- getWord16be
  ar_count <- getWord16be
  queries <- mapM (\_ -> readQuestion bytes) [1 .. query_count]
  answers <- mapM (\_ -> readAnswer bytes) [1 .. an_count]
  return $ Message txId flags queries answers

readPacket :: B.ByteString -> IO Message
readPacket bytes = do
  let r = runGet (readMessage bytes) bytes
  case fst r of
    Left error -> fail error
    Right x    -> return x

writePacket :: Message -> IO B.ByteString
writePacket m = do
  let r = evalStateT (writeMessage m) (HM.empty, 0)
  let x = runPut r
  return $ BL.toStrict x
