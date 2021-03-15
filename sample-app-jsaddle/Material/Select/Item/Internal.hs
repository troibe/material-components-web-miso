{-# LANGUAGE OverloadedStrings #-}

module Material.Select.Item.Internal where

import qualified Miso

data Config a msg = Config
  { value :: a,
    disabled :: Bool,
    additionalAttributes :: [Miso.Attribute msg]
  }

data SelectItem a msg
  = SelectItem (Config a msg) ([Miso.View msg])
