{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.HashMap.Strict (toList)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import Prettyprinter
import System.Clipboard
import Text.Taggy

main :: IO ()
main =
  getClipboardString
    >>= \case
      Just str -> (setClipboardString . show . sep . map convert2 . parseDOM False . T.pack) str
      Nothing -> setClipboardString "I have nothing to do here."

convert2 :: Node -> Doc ann
convert2 = \case
  NodeElement el -> case eltName el of
    "input" -> undefined
    _ ->
      vsep
        [ hsep
            [ "elAttr",
              dquotes (pretty $ TS.unpack $ eltName el),
              if length (toList $ eltAttrs el) > 0
                then
                  "("
                    <> constructAttrs el
                    <> ")"
                else "mempty",
              if length (eltChildren el) > 1
                then "$ do"
                else
                  if length (eltChildren el) == 1
                    then "$"
                    else "$ blank"
            ],
          vsep $ flip map (map convert2 $ eltChildren el) (align . nest 2)
        ]
  NodeContent txt ->
    let text = pretty $ TS.unpack $ TS.strip txt
     in hsep ["text", dquotes text]
  where
    constructAttrs el =
      foldr
        ( \(k, v) doc ->
            (dquotes $ pretty k)
              <> " =: "
              <> (dquotes $ pretty v)
              <> case layoutCompact doc of SEmpty -> mempty; _ -> " <> " <> doc
        )
        mempty
        (toList $ eltAttrs el)
