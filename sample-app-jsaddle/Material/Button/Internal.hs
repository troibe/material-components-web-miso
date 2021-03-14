module Material.Button.Internal (Config (..)) where

import Miso.Html as Html

data Config msg = Config
  { icon :: Maybe String,
    trailingIcon :: Bool,
    disabled :: Bool,
    dense :: Bool,
    href :: Maybe String,
    target :: Maybe String,
    additionalAttributes :: [Html.Attribute msg],
    onClick :: Maybe msg,
    touch :: Bool
  }
