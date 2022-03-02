module CLI where

import Text.Megaparsec (errorBundlePretty)
import qualified Type
import qualified Syntax 
import qualified Parser
import qualified Typechk

run ∷ IO ()
run = go where
  go ∷ IO ()
  go = do
    putStrLn ""
    line ← getLine
    case line of
      ":exit" → pure ()
      _ → case Parser.parseExpr line of
        Left e → putStrLn (errorBundlePretty e) *> go 
        Right expr → do
          putStr $ "\n  " <> Syntax.prettyExpr expr
          case Typechk.runTC $ Typechk.inferType expr of
            Left e → putStrLn $ "\n" <> e
            Right t → putStrLn $ " : " <> Type.prettyType t
          go