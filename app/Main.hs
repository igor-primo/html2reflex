{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Text.Taggy

import Data.Map.Strict (Map, fromList, toList)
import Data.Maybe
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import Prettyprinter
import System.Clipboard
import Text.HTML.DOM
import Text.XML

main :: IO ()
main =
  getClipboardString
    >>= \case
      Just str -> do
        (setClipboardString . show . sep . parseElement . documentRoot . parseLT . T.pack) str
      Nothing -> setClipboardString "I have nothing to do here."

parseElement :: Element -> [Doc ann]
parseElement = \case
  (Element "input" attrs _) ->
    [ hsep
        -- (fmap value) is not strictly necessary, but
        -- a useful pattern.
        [ "fmap",
          "value",
          "$",
          "inputElement",
          "$",
          "def",
          "&",
          "inputElementConfig_elementConfig",
          ".",
          "elementConfig_initialAttributes",
          ".~",
          if length (toList attrs) > 0
            then "(" <> constructAttrs attrs <> ")"
            else "mempty"
        ]
    ]
  (Element elName attrs nodes) -> do
    let nextEls = filterNodesGetElements nodes
    let nextCont = filterNodesGetContents nodes
    [ vsep
        [ hsep
            [ if elName == "svg" then "elDynAttrNS Nothing" else "elAttr",
              dquotes (pretty $ TS.unpack $ nameLocalName elName),
              if length (toList attrs) > 0
                then
                  if elName == "svg"
                    then
                      "(constDyn "
                        <> "("
                        <> constructAttrs attrs
                        <> ")"
                        <> ")"
                    else
                      "("
                        <> constructAttrs attrs
                        <> ")"
                else "mempty",
              if length nodes > 1
                then "$ do"
                else
                  if length nodes == 1
                    then "$"
                    else "$ blank"
            ],
          vsep $ flip map (concat $ map (parseContent) $ nextCont) (align . nest 2),
          vsep $ flip map (concat $ map parseElement nextEls) (align . nest 2)
        ]
      ]
  where
    constructAttrs attrs =
      foldr
        ( \(k, v) doc ->
            (dquotes $ pretty k)
              <> " =: "
              <> (dquotes $ pretty v)
              <> case layoutCompact doc of SEmpty -> mempty; _ -> " <> " <> doc
        )
        mempty
        (toList $ correctName attrs)

    correctName :: Map Name TS.Text -> Map TS.Text TS.Text
    correctName mapp = do
      let mappL = toList mapp

      fromList $ flip map mappL \(n, t) -> (nameLocalName n, t)

parseContent :: TS.Text -> [Doc ann]
parseContent txt = do
  let text = pretty $ TS.unpack $ TS.strip txt
   in [hsep ["text", dquotes text]]

filterNodesGetElements :: [Node] -> [Element]
filterNodesGetElements nodes = flip mapMaybe (filter isElement nodes) $ \case
  NodeElement el -> Just el
  _ -> Nothing
  where
    isElement = \case
      NodeElement _ -> True
      _ -> False

filterNodesGetContents :: [Node] -> [TS.Text]
filterNodesGetContents nodes = flip mapMaybe (filter isContent nodes) $ \case
  NodeContent txt -> Just txt
  _ -> Nothing
  where
    isContent = \case
      NodeContent _ -> True
      _ -> False
