module OneLog.Transduction where

import           Data.GraphViz
import qualified Data.Map            as Map
import           Data.Rewriting.Rule
--import           Data.Rewriting.Rules.Rewrite
import           Data.String
import           Data.Text.Lazy
import           Data.Text.Lazy.IO   as TIO
import           Text.Parsec
--import           Text.Parsec.Text

newtype STerm = STerm { term :: Term String String }
  deriving (Show, Eq)

type SRule = Rule String String

aterm :: STerm
aterm = STerm $ Fun "foo" [Fun "bar" [Var "x", Var "y"]]

arule :: SRule
arule = Rule (Fun "foo" [ Var "z"]) (Fun "bar" [ Fun "qix" [ Var "z" , Var "z" ]])

rule :: STerm -> STerm -> SRule
rule (STerm lhs) (STerm rhs) = Rule lhs rhs

instance IsString STerm where
  fromString = STerm . Var

-- | A (Rational) `Transduction` is formally a directed graph whose edges are labelled with
-- pair of rational expressions from 2 alphabets, the input and the output languages.
-- We define it practically as a graph whose edges are labelled by rewrite `Rule`s : If the lhs
-- of the rule matches the current input, then the rhs is emitted and the current state of
-- the transducer is updated.
--
-- Currently represented as incidence Map giving for each state @q@ the list of pairs of
-- `(STerm, Q)` _from_ that state
type Transduction = Map.Map Q [ (STerm, Q) ]

type Q = Int

-- | A Rule as the form:
-- lhs_term '->' ( '[' condition ']' )? rhs_term
parseRule :: Text -> Either ParseError SRule
parseRule = runParser termParser () ""
  where
    termParser = rule <$> lhsParser <*> (arrow *> rhsParser)
    arrow = spaces *> string "->" <* spaces
    lhsParser = undefined
    rhsParser = undefined

parseGraph :: FilePath -> IO (DotGraph Text)
parseGraph fp = do
  t <- TIO.readFile fp
  pure $ parseDotGraph t
