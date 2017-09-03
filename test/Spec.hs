{-# LANGUAGE OverloadedStrings#-}
import qualified Data.ByteString as B
import           Lib
import           Test.Hspec

exampleFiles =
  [ "dns_request.bin"
  , "dns_response.bin"
  , "multi_a_request.bin"
  , "multi_a_response.bin"
  ]

readFromFile :: String -> IO Message
readFromFile a = do
  b <- B.readFile a
  readPacket b

main :: IO ()
main =
  hspec $ do
    describe "DNS Parsing" $
      mapM_
        (\f ->
           it ("should parse: " ++ f) $ do
              readFromFile f
              return ())
        exampleFiles

    describe "Properties" $ do
      it "should be a question" $ do
        msg <- readFromFile "dns_request.bin"
        let q = head $ getQuestions msg in
          (getName q) `shouldBe` ["fark", "com"]

    describe "Serialization" $
      mapM_
        (\f ->
           it ("should parse and serialize to the same thing: " ++ f) $ do
             b <- B.readFile f
             pkt <- readPacket b
             b' <- writePacket pkt
             b' `shouldBe` b)
        exampleFiles
