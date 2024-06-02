{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Tasty.Expect.Internal where

import Data.Function ((&))
import Data.Functor.Identity
import Data.Maybe qualified as Maybe
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
    QuoteEnd n -> "|" <> T.replicate n "~" <> "]"

escape :: Text -> Text
escape = tokensToText . fmap (bumpQuoteEnd 1) . lexTokens

unescape :: Text -> Text
unescape = tokensToText . fmap (bumpQuoteEnd (-1)) . lexTokens

splitLinesWithEnd :: Text -> [Text]
splitLinesWithEnd t =
  lines
    & zip [0 :: Int ..]
    & map (\(i, l) -> if i == linesLen - 1 then l else l <> "\n")
  where
    lines = T.splitOn "\n" t
    linesLen = length lines

splitLines :: Text -> [Text]
splitLines = T.splitOn "\n"

-- trimIndent :: Text -> Text
-- trimIndent t = runIdentity do
--   t <- pure $ Maybe.fromMaybe t $ T.stripPrefix "\n" t
--   let indent =
--         splitLines t
--           & filter (T.null . T.strip)
--           & map (\t -> T.length t - T.length (T.stripStart t))
--           & safeMinimum
--           & Maybe.fromMaybe 0
--   pure $
--     splitLines t
--       & map (\t -> if T.null (T.strip t) then t else T.drop indent t)
--       & T.concat

--   [expect||
--     first
--     second
--     third
--   ]

safeMinimum :: (Ord a) => [a] -> Maybe a
safeMinimum [] = Nothing
safeMinimum xs = Just $ minimum xs
