module Main where

import Control.Monad.Except
import Data.Functor
import Data.List
import Data.Void
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

main :: IO ()
main = do
  ls <- lines <$> getContents
  case convertLines ls of
    Right result -> putStr result
    Left err -> error $ show err

type Parser = Parsec Void String

type ParseErr = ParseErrorBundle String Void

type Token = String

convertLines :: (MonadError ParseErr m) => [String] -> m String
convertLines ls = do
  ls' <- mapM convertLine ls
  pure $ intercalate "\n" ls'

-- Convert a line to a CSV-formatted line
convertLine :: (MonadError ParseErr m) => String -> m String
convertLine =
  fmap
    ( intercalate ","
        . map wrapToken
    )
    . parseLine

wrapToken :: Token -> String
wrapToken t = "\"" ++ t ++ "\""

parseLine :: (MonadError ParseErr m) => String -> m [Token]
parseLine =
  liftEither
    . runParser lineP ""

lineP :: Parser [Token]
lineP =
  mapM
    (<* hspace)
    [ accountNumberP
    , dateP
    , dateP
    , moneyP
    , moneyP
    , numP
    ]

concatM :: (Monad m) => [m [a]] -> m [a]
concatM = fmap concat . sequence

accountNumberP :: Parser Token
accountNumberP =
  concatM
    [ some digitChar
    , string "-"
    , some digitChar
    ]

dateP :: Parser Token
dateP =
  concatM
    [ some digitChar
    , string "-"
    , monthP
    , string "-"
    , some digitChar
    ]

monthP :: Parser String
monthP =
  try (string "Jan")
    <|> try (string "Feb")
    <|> try (string "Mar")
    <|> try (string "Apr")
    <|> try (string "May")
    <|> try (string "Jun")
    <|> try (string "Jul")
    <|> try (string "Aug")
    <|> try (string "Sep")
    <|> try (string "Oct")
    <|> try (string "Nov")
    <|> try (string "Dec")

moneyP :: Parser Token
moneyP =
  concatM
    [ string "$"
    , some digitChar
    , string "."
    , some digitChar
    , string " "
    , string "USD"
    ]

numP :: Parser Token
numP =
  sepBy
    (some digitChar)
    (char ',')
    <&> intercalate ","
