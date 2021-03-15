{-# LANGUAGE OverloadedStrings #-}

module Material.Select.Icon.Internal where

import qualified Miso

data Icon msg = Icon
  { node :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg,
    attributes :: [Miso.Attribute msg],
    nodes :: [Miso.View msg],
    onInteraction :: Maybe msg,
    disabled :: Bool
  }

{-- | SvgIcon
    { node :: [Svg.Attribute msg] -> [Svg msg] -> Miso.View msg
    , attributes :: [Svg.Attribute msg]
    , nodes :: [Svg msg]
    , onInteraction :: Maybe msg
    , disabled :: Bool
    } --}
