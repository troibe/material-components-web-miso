{-# LANGUAGE OverloadedStrings #-}

module Material.List.Item.Internal
  ( Config (..),
    ListItem (..),
    Selection (..),
  )
where

import Miso

data Config msg = Config
  { disabled :: Bool,
    selection :: Maybe Selection,
    href :: Maybe String,
    target :: Maybe String,
    additionalAttributes :: [Miso.Attribute msg],
    onClick :: Maybe msg,
    node :: Miso.View msg
  }

data Selection
  = Selected
  | Activated
  deriving (Eq, Show)

data ListItem msg
  = ListItem (Config msg)
  | ListItemDivider (Miso.View msg)
  | ListGroupSubheader (Miso.View msg)
