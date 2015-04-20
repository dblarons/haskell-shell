module HashTests where

import Main hiding (main)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "replaceEnvVars" $ do
    let env = [("PATH", "/bin")]
    it "should replace environment variables with their expanded form" $ do
      let str = "echo $PATH"
      replaceEnvVars env str `shouldBe` "echo /bin"

