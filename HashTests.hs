module HashTests where

import Main hiding (main)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "exportParse" $ do
    it "should parse a basic PATH assignment" $ do
      let env = exportParse "PATH=/bin" ""
      env `shouldBe` ("PATH", "/bin")

  describe "exportInsert" $ do
    let initEnv = [("PATH", "/bin")]

    it "should insert a new export into the environment" $ do
      let env = exportInsert ("PATH", "foo") []
      env `shouldBe` [("PATH", "foo")]

  describe "replaceEnvVars" $ do
    let env = exportInsert ("PATH", "/bin") []
    it "should replace environment variables with their expanded form" $ do
      let str = "echo $PATH"
      replaceEnvVars env str `shouldBe` "echo /bin"

