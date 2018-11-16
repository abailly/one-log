{-# LANGUAGE PatternSynonyms #-}
module OneLog.Transduction where

import           Data.GraphViz        (DotGraph, parseDotGraph)
import qualified Data.Map             as Map
import           Data.Rewriting.Rule
--import           Data.Rewriting.Rules.Rewrite
import           Data.String
import           Data.Text.Lazy
import           Data.Text.Lazy.IO    as TIO
import           Text.Parsec
import           Text.Parsec.Language (haskellDef)
import           Text.Parsec.Token

data Fun = O
         | K String
         -- ^A key identifier mapped to some value
         | S String
         -- ^A literal string
  deriving (Show, Eq)

type STerm = Term Fun String
type SRule = Rule Fun String

pattern Obj :: String -> STerm -> STerm
pattern Obj s v = Fun (K s) [ v ]

pattern Str :: String -> STerm
pattern Str s = Fun (S s) []

aterm :: STerm
aterm = Fun (K "foo") [Fun O [Var "x", Var "y"]]

arule :: SRule
arule = Rule (Fun (K "foo") [ Var "z"]) (Fun O [ Fun O [ Var "z" , Var "z" ]])

rule :: STerm -> STerm -> SRule
rule lhs rhs = Rule lhs rhs

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
parseRule = runParser ruleParser () ""  . unpack
  where
    ruleParser = rule <$> lhsParser <*> (arrow *> rhsParser)
    arrow = spaces *> string "->" <* spaces

    lhsParser = termParser
    rhsParser = termParser

    termParser = (Str <$> stringLiteral lexer <?> "string literal")
                 <|> (Var <$> identifier lexer <?> "variable")

    lexer = makeTokenParser haskellDef

parseGraph :: FilePath -> IO (DotGraph Text)
parseGraph fp = do
  t <- TIO.readFile fp
  pure $ parseDotGraph t
