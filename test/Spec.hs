import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
        describe "uncomment" $ do
            it "should uncomment a commented line" $
                uncomment "#127.0.0.1 example.com" `shouldBe` "127.0.0.1 example.com"
            it "should uncomment a multiple commented lines" $
                uncomment "###127.0.0.1 example.com" `shouldBe` "127.0.0.1 example.com"
            it "should not fail on empty lines" $
                uncomment "" `shouldBe` ""
        describe "comment" $ do
            it "should comment a uncommented line" $
                comment "127.0.0.1 example.com" `shouldBe` "#127.0.0.1 example.com"
            it "should not double comment a commented line" $
                comment "#127.0.0.1 example.com" `shouldBe` "#127.0.0.1 example.com"
            it "should not change empty lines" $
                comment "" `shouldBe` ""
        describe "createEntryLine" $ do
            it "should create a single entry line" $
                createEntryLine "example" ["example.com"] `shouldBe` "127.0.0.1 example.com #timeout:example"
            it "should create a multiple entry line" $
                createEntryLine "example" ["example.com", "example.org"] `shouldBe` "127.0.0.1 example.com example.org #timeout:example"
        describe "isEntry" $ do
            it "should return true for one of our lines" $
                isEntry "127.0.0.1 example.com #timeout:example" `shouldBe` True
            it "should return false for a different line" $
                isEntry "127.0.0.1 example.com" `shouldBe` False
            it "should return false for a very similar line" $
                isEntry "127.0.0.1 example.com #xxxxx:example" `shouldBe` False
        describe "getName" $ do
            it "should return a name from a line" $
                getName "127.0.0.1 example.com #timeout:example" `shouldBe` (Just "example")
            it "should return Nothing from a non line" $
                getName "127.0.0.1 example.com" `shouldBe` Nothing
