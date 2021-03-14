{-# LANGUAGE OverloadedStrings #-}

module Material.ImageList.Item
  ( Config,
    config,
    setLabel,
    setHref,
    setAttributes,
    ImageListItem,
    imageListItem,
  )
where

import Material.ImageList.Item.Internal (Config (..), ImageListItem (..))
import qualified Miso

-- | Default configuration of an image list item
config :: Config msg
config =
  Config
    { label = Nothing,
      href = Nothing,
      additionalAttributes = [],
      image = ""
    }

-- | Specify an image list item's label
setLabel :: Maybe String -> Config msg -> Config msg
setLabel label config_ =
  config_ {label = label}

-- | Specify whether an image list item is supposed to be a _link image list item_
-- A link image list items behaves essentially like a HTML5 anchor element.
setHref :: Maybe String -> Config msg -> Config msg
setHref href config_ =
  config_ {href = href}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Image list item constructor
imageListItem :: Config msg -> String -> ImageListItem msg
imageListItem config_ image =
  ImageListItem (config_ {image = image})
