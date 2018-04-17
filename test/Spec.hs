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
        describe "tranformNamedEntries" $ do
            let commented = [ "# localhost is used to configure the loopback interface"
                        , "# when the system is booting.  Do not change this entry."
                        , "##"
                        , "127.0.0.1   localhost"
                        , "#127.0.0.1 example.com #timeout:example"
                        , "#127.0.0.1 example.com #timeout:example2"
                        ]
            let uncommented = [ "# localhost is used to configure the loopback interface"
                        , "# when the system is booting.  Do not change this entry."
                        , "##"
                        , "127.0.0.1   localhost"
                        , "127.0.0.1 example.com #timeout:example"
                        , "#127.0.0.1 example.com #timeout:example2"
                        ]
            let multiuncommented = [ "# localhost is used to configure the loopback interface"
                        , "# when the system is booting.  Do not change this entry."
                        , "##"
                        , "127.0.0.1   localhost"
                        , "127.0.0.1 example.com #timeout:example"
                        , "127.0.0.1 example.com #timeout:example2"
                        ]
            it "should uncomment a named line in a file" $
                transformNamedEntries uncomment ["example"] commented `shouldBe` uncommented
            it "should comment a named line in a file" $
                transformNamedEntries comment ["example"] uncommented `shouldBe` commented
            it "should uncommment multiple named lines" $
                transformNamedEntries uncomment ["example", "example2"] commented `shouldBe` multiuncommented
            it "should leave lines alone in a file with empty names" $
                transformNamedEntries comment [] uncommented `shouldBe` uncommented
        describe "tranformAllEntries" $ do
            let commented = [ "# localhost is used to configure the loopback interface"
                        , "# when the system is booting.  Do not change this entry."
                        , "##"
                        , "127.0.0.1   localhost"
                        , "#127.0.0.1 example.com #timeout:example"
                        , "#127.0.0.1 example.com #timeout:example2"
                        ]
            let uncommented = [ "# localhost is used to configure the loopback interface"
                        , "# when the system is booting.  Do not change this entry."
                        , "##"
                        , "127.0.0.1   localhost"
                        , "127.0.0.1 example.com #timeout:example"
                        , "127.0.0.1 example.com #timeout:example2"
                        ]
            it "should uncomment all entries in a file" $
                transformAllEntries uncomment commented `shouldBe` uncommented
            it "should comment all entries in a file" $
                transformAllEntries comment uncommented `shouldBe` commented
        describe "addEntry" $ do
            let given = [ "# localhost is used to configure the loopback interface"
                        , "# when the system is booting.  Do not change this entry."
                        , "##"
                        , "127.0.0.1   localhost"
                        , "#127.0.0.1 example.com #timeout:example"
                        ]
            let expected = [ "# localhost is used to configure the loopback interface"
                        , "# when the system is booting.  Do not change this entry."
                        , "##"
                        , "127.0.0.1   localhost"
                        , "#127.0.0.1 example.com #timeout:example"
                        , "127.0.0.1 example.com #timeout:example2"
                        ]
            let expectedMulti = [ "# localhost is used to configure the loopback interface"
                        , "# when the system is booting.  Do not change this entry."
                        , "##"
                        , "127.0.0.1   localhost"
                        , "#127.0.0.1 example.com #timeout:example"
                        , "127.0.0.1 example.com test.com #timeout:example2"
                        ]

            it "should add an entry to the end of the file" $ do
                addEntry "example2" ["example.com"] given `shouldBe` expected
            it "should add an entry with multiple urls to the end of the file" $ do
                addEntry "example2" ["example.com", "test.com"] given `shouldBe` expectedMulti
        describe "getGetChangeFromArgs" $ do
            it "should return Off by default" $
                getChangeFromArgs [] `shouldBe` Off
            it "should return Off for off" $
                getChangeFromArgs ["out"] `shouldBe` Off
            it "should return Off for on" $
                getChangeFromArgs ["in"] `shouldBe` On
            it "should return TargetedOn for on with arguments" $
                getChangeFromArgs ["in", "1", "2"] `shouldBe` TargetedOn ["1", "2"]
            it "should return TargetedOff for off with arguments" $
                getChangeFromArgs ["out", "1", "2"] `shouldBe` TargetedOff ["1", "2"]
            it "should return AddEntry for add with arguments" $
                getChangeFromArgs ["add", "1", "2"] `shouldBe` AddEntry "1" ["2"]
