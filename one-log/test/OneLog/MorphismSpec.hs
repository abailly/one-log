{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module OneLog.MorphismSpec where

import           Control.Category
import           Control.Lens
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

collect :: (Monad m) => Pipe Text Text (StateT Int m) ()
collect = do
  c <- await
  modify (+1)
  yield c
  collect

printText :: (MonadIO m) => Proxy () Text y' y m b
printText = forever $ await >>= liftIO . print

--tag :: (Applicative f) => (Text -> f Text) -> Value -> f Value
tag  :: Traversal' Value Text
tag = key "log".key "message".key "tag"._String

spec :: Spec
spec = describe "Basic Morphisms" $ do

  around (withFile sampleLog ReadMode) $ it "stream simple log extracting tag" $ \ hdl -> do
    output <- flip execStateT 0 $ runEffect $ decodeText hdl >-> applyMorphism tag >-> collect >-> printText

    output `shouldBe` 15321
