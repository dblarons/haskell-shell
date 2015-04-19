module HashTests where

import Main hiding (main)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "exportParse" $ do
    it "should parse commands that contain $HOME" $ do
      let env = exportParse "PATH=$HOME/bin/perl:$PATH" "Users/aaron" 
      env `shouldBe` (("PATH", "Users/aaron/bin/perl"), EnvPrepend)

    it "should parse appending commands" $ do
      let env = exportParse "PATH=$PATH:~/opt/bin" ""
      env `shouldBe` (("PATH", "~/opt/bin"), EnvAppend)

    it "should parse prepending commands" $ do
      let env = exportParse "PATH=~/opt/bin:$PATH" ""
      env `shouldBe` (("PATH", "~/opt/bin"), EnvPrepend)

  describe "exportInsert" $ do
    let initEnv = [("PATH", "/bin")]

    it "should insert a new export" $ do
      let env = exportInsert ("PATH", "foo") EnvNew []
      env `shouldBe` [("PATH", "foo")]

    it "should append a new export" $ do
      let env = exportInsert ("PATH", "/usr/local/bin") EnvAppend initEnv
      env `shouldBe` [("PATH", "/bin:/usr/local/bin")]

    it "should prepend a new export" $ do
      let env = exportInsert ("PATH", "/usr/local/bin") EnvPrepend initEnv
      env `shouldBe` [("PATH", "/usr/local/bin:/bin")]

    

