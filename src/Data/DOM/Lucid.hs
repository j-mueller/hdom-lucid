{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Use lucid to create HTML
module Data.DOM.Lucid where

import           Control.Monad.Supply
import           Data.DOM             (DomNode (..))
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Lucid                as L
import qualified Lucid.Base           as L

data LucidDomNode a i = LucidDomNode {
  _nodeId          :: !i,
  _nodeType        :: !Text,
  _ClientNamespace :: !Text,
  _style           :: Map Text Text,
  _attributes      :: Map Text Text,
  _onClick         :: Maybe a,
  _children        :: Either Text [LucidDomNode a i]
  } deriving (Functor, Foldable, Traversable)

instance DomNode LucidDomNode where
  type StringType LucidDomNode = Text
  type NamespaceType LucidDomNode = Text
  node i ns tp = LucidDomNode i tp ns Map.empty Map.empty Nothing (Right [])
  children f ld = fmap (\c -> ld { _children = c } ) $ f $ _children ld
  style f ld = fmap (\c -> ld { _style = c } ) $ f $ _style ld
  attributes f ld = fmap (\c -> ld { _attributes = c } ) $ f $ _attributes ld
  onClick f ld = fmap (\c -> ld { _onClick = c } ) $ f $ _onClick ld

instance L.ToHtml (LucidDomNode a Text) where
  toHtmlRaw = L.toHtml
  toHtml LucidDomNode{..} =
    let ele = L.makeElement _nodeType chs
    in L.with ele as' where
      as = [ L.makeAttribute k v | (k, v) <- Map.toList _attributes]
      i = L.makeAttribute "id"
      st m =
        if Map.null m
        then []
        else [L.makeAttribute "style" $ Text.intercalate "; " $ fmap (\t -> fst t `Text.append` ": " `Text.append` snd t) $ Map.toList m]
      as' = i _nodeId : (st _style ++ as)
      chs = either L.toHtml (foldMap L.toHtml) _children

instance L.ToHtml (LucidDomNode a ()) where
  toHtmlRaw = L.toHtml
  toHtml = L.toHtml . flip evalSupply ids . traverse (const supply) where
    ids = (Text.pack . (:) 'i' . show) <$> [1..]
