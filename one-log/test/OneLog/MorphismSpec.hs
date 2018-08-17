{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module OneLog.MorphismSpec where

import           Control.Category
import           Control.Lens               hiding ((.=))
import           Control.Monad.State
import           Prelude                    hiding ((.))
--import           Data.Aeson
--import           Data.Text                  (unlines)
--import           Data.Time.Clock
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.FileEmbed             (makeRelativeToProject)
import           Data.Text                  (Text)
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL))
import           OneLog.Morphism
import           Pipes
import           System.IO
import           Test.Hspec

sampleLog :: FilePath
sampleLog = $(LitE . StringL <$> makeRelativeToProject "logs")

collect :: (Monad m) => Consumer Text (StateT Int m) ()
collect = do
  _ <- await
  modify (+1)
  collect

--tag :: (Applicative f) => (Text -> f Text) -> Value -> f Value
tag  :: Traversal' Value Text
tag = key "log".key "message".key "tag"._String

spec :: Spec
spec = describe "Basic Morphisms" $ do

  around (withFile sampleLog ReadMode) $ it "stream simple log extracting tag" $ \ hdl -> do
    output <- flip execStateT 0 $ runEffect $ decodeText hdl >-> applyMorphism tag >-> collect

    output `shouldBe` 15321

  describe "Traversal Parser" $ do
    it "parses tag" $ do
      parseMorphism "log" `shouldBe` Tag "log"

  describe "Morphism application" $ do
    it "extracts tag" $ do
      object [ "log" .= ("bar" :: String) ] ^.. fromMorphism (Tag "log")
        `shouldBe` [String "bar"]
