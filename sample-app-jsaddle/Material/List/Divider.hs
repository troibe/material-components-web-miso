{-# LANGUAGE OverloadedStrings #-}

module Material.List.Divider
  ( Config,
    config,
    setPadded,
    setInset,
    setAttributes,
    listItem,
    group,
  )
where

import qualified Data.Function
import qualified Data.Maybe as Maybe
import qualified Material.List.Item (ListItem)
import qualified Material.List.Item.Internal as ListItem
import qualified Miso

(<|) = (Data.Function.$)

-- | Configuration of a list item divider
data Config msg = Config
  { inset :: Bool,
    padded :: Bool,
    additionalAttributes :: [Miso.Attribute msg]
  }

-- | Default configuration of a list item divider
config :: Config msg
config =
  Config
    { inset = False,
      padded = False,
      additionalAttributes = []
    }

-- | Specify whether a list divider should be _inset_
--
-- Insert list item dividers to not intersect a list item's meta.
setInset :: Bool -> Config msg -> Config msg
setInset inset config_ =
  config_ {inset = inset}

-- | Specify whether a list divider should be _padded_
--
-- Padded list item dividers do not intersect a list item's avatar.
setPadded :: Bool -> Config msg -> Config msg
setPadded padded config_ =
  config_ {padded = padded}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | List item divider view function
listItem :: Config msg -> ListItem.ListItem msg
listItem (config_@Config {additionalAttributes = additionalAttributes}) =
  ListItem.ListItemDivider
    <| Miso.li_
      ( Maybe.mapMaybe
          id
          [ listDividerCs,
            separatorRoleAttr,
            insetCs config_,
            paddedCs config_
          ]
          ++ additionalAttributes
      )
      []

listDividerCs :: Maybe (Miso.Attribute msg)
listDividerCs =
  Just (Miso.class_ "mdc-list-divider")

separatorRoleAttr :: Maybe (Miso.Attribute msg)
separatorRoleAttr =
  Just (Miso.stringProp "role" "separator")

insetCs :: Config msg -> Maybe (Miso.Attribute msg)
insetCs (Config {inset = inset}) =
  if inset
    then Just (Miso.class_ "mdc-list-divider--inset")
    else Nothing

paddedCs :: Config msg -> Maybe (Miso.Attribute msg)
paddedCs (Config {padded = padded}) =
  if padded
    then Just (Miso.class_ "mdc-list-divider--padded")
    else Nothing

-- | List group divider view function
group :: [Miso.Attribute msg] -> Miso.View msg
group additionalAttributes =
  Miso.hr_ (Maybe.mapMaybe id [listDividerCs] ++ additionalAttributes)
