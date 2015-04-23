module HashTests where

import Main hiding (main)
import Test.Hspec
import System.Directory

main :: IO ()
main = hspec $ do
  describe "replaceEnvVars" $ do
    let env = [("PATH", "/bin")]
    it "should replace environment variables with their expanded form" $ do
      let str = "echo $PATH"
      replaceEnvVars env str `shouldBe` "echo /bin"

  describe "pipeParser" $ do
    it "should parse a series of piped commands into CommandLike tuples" $ do
      let cmd = "ls -al | grep foo | less"
      pipeParser cmd `shouldBe` Pipe (Cmd "ls -al") (Pipe (Cmd "grep foo") (Cmd "less"))

    it "should parse a single command into a single string" $ do
      let cmd = "ls -al"
      pipeParser cmd `shouldBe` Cmd "ls -al"

    it "should parse a command with two pipes in a row" $ do
      let cmd = "ls -al | | cat"
      pipeParser cmd `shouldBe` Pipe (Cmd "ls -al") (Pipe (Cmd "") (Cmd "cat"))

    it "should parse a file redirection" $ do
      let cmd = "ls -al > foo.txt"
      pipeParser cmd `shouldBe` Pipe (Cmd "ls -al") (HFile "foo.txt")

    it "should parse a file redirection after pipes" $ do
      let cmd = "ls -al | grep foo > foo.txt"
      pipeParser cmd `shouldBe` Pipe (Cmd "ls -al") (Pipe (Cmd "grep foo") (HFile "foo.txt"))

  describe "replaceTilde" $ do
    it "should replace Tilde with the home directory" $ do
      let cmd = "cd ~/Documents/Code"
      home <- getHomeDirectory
      res <- replaceTilde cmd
      res `shouldBe` "cd " ++ home ++ "/Documents/Code"

  describe "backgroundParser" $ do
    it "should return updated string and true if command should be run in background" $ do
      let cmd = "ls -al | grep foo &"
      backgroundParser cmd `shouldBe` ("ls -al | grep foo ", True)

    it "should return same string if command should not be run in background" $ do
      let cmd = "ls -al | grep foo "
      backgroundParser cmd `shouldBe` (cmd, False)

    it "should ignore &'s that take place earlier in the string" $ do
      let cmd = "ls -al& | grep foo"
      backgroundParser cmd `shouldBe` (cmd, False)

