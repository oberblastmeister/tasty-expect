{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Expect.Internal
  ( Token (..),
    lexTokens,
    tokensToText,
    unescape,
    escape,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

data Token
  = Text !Text
  | QuoteEnd !Int
  deriving (Show, Eq)

bumpQuoteEnd :: Int -> Token -> Token
bumpQuoteEnd n = \case
  QuoteEnd m -> QuoteEnd (m + n)
  t -> t

lexTokens :: Text -> [Token]
lexTokens t =
  res
  where
    res = case T.uncons barStart of
      Nothing -> [Text beforeBar]
      Just ('|', afterBar) -> do
        let (tildes, afterTildes) = T.span (== '~') afterBar
            tildesLen = T.length tildes
            currentText = (beforeBar <> "|" <> tildes)
        case T.uncons afterTildes of
          Just (']', rest) -> Text beforeBar : QuoteEnd tildesLen : lexTokens rest
          Just (_, _) -> Text currentText : lexTokens afterTildes
          Nothing -> [Text currentText]
      Just (_, _) -> error "impossible"
    (beforeBar, barStart) = T.breakOn "|" t

tokensToText :: [Token] -> Text
tokensToText =
  T.concat . map \case
    Text t -> t
    QuoteEnd n -> "|" <> T.replicate n "~" <> ""

escape :: Text -> Text
escape = tokensToText . fmap (bumpQuoteEnd 1) . lexTokens

unescape :: Text -> Text
unescape = tokensToText . fmap (bumpQuoteEnd (-1)) . lexTokens
