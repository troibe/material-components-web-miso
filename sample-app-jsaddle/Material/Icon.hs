{-# LANGUAGE OverloadedStrings #-}

module Material.Icon (icon) where

import Miso.Html
import Miso.String

-- | Icon view function
icon :: [Attribute msg] -> String -> View msg
icon additionalAttributes iconName =
  i_ (class_ "material-icons" : additionalAttributes) [text (toMisoString iconName)]
