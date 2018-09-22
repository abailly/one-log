{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module OneLog.TransductionSpec where

import           Control.Category
import           Prelude                    hiding ((.))
--import           Data.Aeson
--import           Data.Text                  (unlines)
--import           Data.Time.Clock
import           Data.FileEmbed             (makeRelativeToProject)
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL))
import           OneLog.Transduction
import           System.IO
import           Test.Hspec

sampleXDuction :: FilePath
sampleXDuction = $(LitE . StringL <$> makeRelativeToProject "xduction.dot")

spec :: Spec
spec = describe "Transductions" $ do

  around (withFile sampleXDuction ReadMode) $ do
    it "can parse a transduction from a graphviz file" $ \ _hdl -> do
      pending

  describe "Terms Parser" $ do
    it "parses " $ do
      parseRule "foo -> bar" `shouldBe` Right (rule "foo" "bar")
