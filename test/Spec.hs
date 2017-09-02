import qualified Data.ByteString as B
import           Lib
import           Test.Hspec

exampleFiles =
  [ "dns_request.bin"
  , "dns_response.bin"
  , "multi_a_request.bin"
  , "multi_a_response.bin"
  ]

main :: IO ()
main =
  hspec $ do
    describe "DNS Parsing" $
      mapM_
        (\f ->
           it ("should parse: " ++ f) $ do
             b <- B.readFile f
             pkt <- readPacket b
             return ())
        exampleFiles
    describe "Serialization" $
      mapM_
        (\f ->
           it ("should parse and serialize to the same thing: " ++ f) $ do
             b <- B.readFile f
             pkt <- readPacket b
             b' <- writePacket pkt
             b' `shouldBe` b
             return ())
        exampleFiles
