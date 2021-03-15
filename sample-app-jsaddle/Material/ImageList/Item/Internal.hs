{-# LANGUAGE OverloadedStrings #-}

module Material.ImageList.Item.Internal (Config (..), ImageListItem (..)) where

import qualified Miso

data Config msg = Config
  { label :: Maybe String,
    href :: Maybe String,
    additionalAttributes :: [Miso.Attribute msg],
    image :: String
  }

data ImageListItem msg
  = ImageListItem (Config msg)
