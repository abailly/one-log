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
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL))
import           OneLog.Morphism
import           Pipes
import           System.IO
import           Test.Hspec

sampleLog :: FilePath
sampleLog = $(LitE . StringL <$> makeRelativeToProject "logs")

collect :: (Monad m) => Consumer [a] (StateT Int m) ()
collect = do
  t <- await
  modify (+ length t)
  collect

--tag :: (Applicative f) => (Text -> f Text) -> Value -> f Value
tag  :: Traversal' Value Value
tag = key "log".key "message".key "tag"

idx  :: Traversal' Value Value
idx = key "log".key "request".key "headers".nth 0.nth 0

spec :: Spec
spec = describe "Basic Morphisms" $ do

  around (withFile sampleLog ReadMode) $ do
    it "stream simple log extracting tag" $ \ hdl -> do
      output <- flip execStateT 0 $ runEffect $ decodeText hdl >-> applyMorphism tag >-> collect

      output `shouldBe` 9382

    it "stream simple log extracting array index" $ \ hdl -> do
      output <- flip execStateT 0 $ runEffect $ decodeText hdl >-> applyMorphism idx >-> collect

      output `shouldBe` 5050

    it "stream simple log extracting array range" $ \ hdl -> do
      output <- flip execStateT 0 $ runEffect $ decodeText hdl >-> applyMorphism (fromMorphism $ parseMorphism "log.request.headers.[0].[0..2]") >-> collect

      output `shouldBe` 5050

  describe "Traversal Parser" $ do
    it "parses tag" $ do
      parseMorphism "log" `shouldBe` Tag "log"
    it "parses tag composition" $ do
      parseMorphism "log.message" `shouldBe` Comp (Tag "log") (Tag "message")
    it "parses array indexation" $ do
      parseMorphism "log.headers.[0].[0]" `shouldBe` (Comp (Comp (Comp (Tag "log") (Tag "headers")) (Idx 0)) (Idx 0))
    it "parses array slices" $ do
      parseMorphism "log.headers.[0].[0..3]" `shouldBe` (Comp (Comp (Comp (Tag "log") (Tag "headers")) (Idx 0)) (Rge 0 3))

  describe "Morphism application" $ do
    it "extracts tag" $ do
      object [ "log" .= ("bar" :: String) ] ^.. fromMorphism (Tag "log")
        `shouldBe` [String "bar"]
