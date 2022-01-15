{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Material.List.Item
  ( Config,
    config,
    setOnClick,
    setDisabled,
    setSelected,
    setHref,
    setTarget,
    setAttributes,
    ListItem,
    listItem,
    graphic,
    meta,
    text,
    Selection,
    selected,
    activated,
    TwoLineListItemText,
  )
where

import qualified Data.Maybe as Maybe
import Material.List.Item.Internal (Config (..), ListItem (..), Selection (..))
import qualified Miso
import qualified Miso.Html.Property

-- type Config = Material.List.Item.Internal.Config

-- | Default configuration of a list item
config :: Config msg
config =
  Config
    { disabled = False,
      selection = Nothing,
      href = Nothing,
      target = Nothing,
      additionalAttributes = [],
      onClick = Nothing,
      node = Miso.text ""
    }

-- | Specify whether a list item should be disabled
--
-- Disabled list items cannot be interacted with and have not visual interaction
-- effect.
setDisabled :: Bool -> Config msg -> Config msg
setDisabled disabled config_ =
  config_ {disabled = disabled}

-- | Selection of a list item
--
-- A list item may be either in selected or in activated selection state.

-- type Selection =
--    Material.List.Item.Internal.Selection

-- | Selected selection state
selected :: Selection
selected =
  Selected

-- | Activated selection state
activated :: Selection
activated =
  Activated

-- | Specify whether a list item is selected
--
-- A selected list item may be either _selected_ or _activated_. A list item that
-- may change its selection state within the current page, should be selected. A
-- list item that may not change its state within the current page should be
-- activated.
--
-- As a rule of thumb, a navigation list item should be activated, while any other
-- list item should be selected.
setSelected :: Maybe Selection -> Config msg -> Config msg
setSelected selection config_ =
  config_ {selection = selection}

-- | Specify whether a list item is a _link list item_
--
-- Link list items essentially behave like a HTML5 anchor element.
setHref :: Maybe String -> Config msg -> Config msg
setHref href config_ =
  config_ {href = href}

-- | Specify a link list item's HTML5 target attribute
--
-- Note that non-link list items ignore this configuration option.
setTarget :: Maybe String -> Config msg -> Config msg
setTarget target config_ =
  config_ {target = target}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user interacts with the list item
setOnClick :: msg -> Config msg -> Config msg
setOnClick onClick config_ =
  config_ {onClick = Just onClick}

-- | List item type
--
-- List items can only be rendered within a [list container](Material-List).

-- type ListItem msg =
--    Material.List.Item.Internal.ListItem msg

-- | List item constructor
listItem :: Config msg -> [Miso.View msg] -> ListItem msg
listItem (config_@Config {additionalAttributes, href}) nodes =
  ListItem (config_ {node = listItemView config_ nodes})

listItemView :: Config msg -> [Miso.View msg] -> Miso.View msg
listItemView (config_@Config {additionalAttributes, href}) nodes =
  ( \attributes ->
      if href /= Nothing
        then Miso.nodeHtml "mdc-list-item" [] [Miso.a_ attributes nodes]
        else Miso.nodeHtml "mdc-list-item" attributes nodes
  )
    ( Maybe.mapMaybe
        id
        [ listItemCs,
          hrefAttr config_,
          targetAttr config_,
          disabledCs config_,
          selectedCs config_,
          activatedCs config_,
          ariaSelectedAttr config_
        ]
        ++ additionalAttributes
    )

listItemCs :: Maybe (Miso.Attribute msg)
listItemCs =
  Just (Miso.class_ "mdc-list-item")

disabledCs :: Config msg -> Maybe (Miso.Attribute msg)
disabledCs (Config {disabled}) =
  if disabled
    then Just (Miso.class_ "mdc-list-item--disabled")
    else Nothing

selectedCs :: Config msg -> Maybe (Miso.Attribute msg)
selectedCs (Config {selection}) =
  if selection == Just Selected
    then Just (Miso.class_ "mdc-list-item--selected")
    else Nothing

activatedCs :: Config msg -> Maybe (Miso.Attribute msg)
activatedCs (Config {selection}) =
  if selection == Just Activated
    then Just (Miso.class_ "mdc-list-item--activated")
    else Nothing

ariaSelectedAttr :: Config msg -> Maybe (Miso.Attribute msg)
ariaSelectedAttr (Config {selection}) =
  if selection /= Nothing
    then Just (Miso.stringProp "aria-selected" "true")
    else Nothing

hrefAttr :: Config msg -> Maybe (Miso.Attribute msg)
hrefAttr (Config {href}) =
  Maybe.maybe Nothing (\h -> Just (Miso.stringProp "href" h)) href

targetAttr :: Config msg -> Maybe (Miso.Attribute msg)
targetAttr (Config {href, target}) =
  Maybe.maybe Nothing (\h -> Just (Miso.stringProp "target" h)) target

data TwoLineListItemText msg = TwoLineListItemText
  { primary :: [Miso.View msg],
    secondary :: [Miso.View msg]
  }

-- | Two-line list item's text
text ::
  [Miso.Attribute msg] ->
  TwoLineListItemText msg ->
  Miso.View msg
text additionalAttributes (TwoLineListItemText {primary, secondary}) =
  Miso.div_
    (Miso.class_ "mdc-list-item__text" : additionalAttributes)
    [ primaryText [] primary,
      secondaryText [] secondary
    ]

primaryText :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg
primaryText additionalAttributes nodes =
  Miso.div_ (Miso.class_ "mdc-list-item__primary-text" : additionalAttributes) nodes

secondaryText :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg
secondaryText additionalAttributes nodes =
  Miso.div_ (Miso.class_ "mdc-list-item__secondary-text" : additionalAttributes) nodes

-- | A list item's graphic tile
graphic :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg
graphic additionalAttributes nodes =
  Miso.div_ (Miso.class_ "mdc-list-item__graphic" : additionalAttributes) nodes

-- | A list item's meta tile
meta :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg
meta additionalAttributes nodes =
  Miso.div_ (Miso.class_ "mdc-list-item__meta" : additionalAttributes) nodes
