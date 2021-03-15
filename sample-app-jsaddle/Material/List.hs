{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Material.List
  ( Config,
    config,
    setNonInteractive,
    setDense,
    setAvatarList,
    setTwoLine,
    setAttributes,
    setWrapFocus,
    list,
    group,
    subheader,
  )
where

import Data.Aeson.Types
import qualified Data.Function
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Material.List.Item (Config, ListItem)
import qualified Material.List.Item.Internal as ListItem
import qualified Miso
import qualified Miso.Html.Event
import qualified Miso.String

(|>) = (Data.Function.&)

-- | Configuration of a list
data Config msg = Config
  { nonInteractive :: Bool,
    dense :: Bool,
    avatarList :: Bool,
    twoLine :: Bool,
    vertical :: Bool,
    wrapFocus :: Bool,
    additionalAttributes :: [Miso.Attribute msg]
  }

-- | Default configuration of a list
config :: Config msg
config =
  Config
    { nonInteractive = False,
      dense = False,
      avatarList = False,
      twoLine = False,
      vertical = False,
      wrapFocus = False,
      additionalAttributes = []
    }

-- | Specify whether a list should be non-interactive
--
-- Non-interactive lists do not feature keyboard interaction and list items have
-- no visual interaction effect.
setNonInteractive :: Bool -> Config msg -> Config msg
setNonInteractive nonInteractive config_ =
  config_ {nonInteractive = nonInteractive}

-- | Specify whether a list should be _dense_
--
-- Dense lists are more compact and feature smaller than normal margins
setDense :: Bool -> Config msg -> Config msg
setDense dense config_ =
  config_ {dense = dense}

-- | Specify whether a list should be an _avatar_ list
--
-- An avatar list features a larger than usual list item _graphic_.
setAvatarList :: Bool -> Config msg -> Config msg
setAvatarList avatarList config_ =
  config_ {avatarList = avatarList}

-- | Specify whether a list should be a _two line_ list
--
-- Two line lists feature list items with a primary and a secondary text line.
setTwoLine :: Bool -> Config msg -> Config msg
setTwoLine twoLine config_ =
  config_ {twoLine = twoLine}

-- | Specify whether a list should wrap focus
--
-- A list that wraps focus focuses the first list item after pressing tab on the
-- last list item. By default, a list in that case passes focus to the next
-- focusable control.
setWrapFocus :: Bool -> Config msg -> Config msg
setWrapFocus wrapFocus config_ =
  config_ {wrapFocus = wrapFocus}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | List view function
--
-- The list view function takes its list items as two arguments. The first
-- argument represents the first list item, and the second argument reresents the
-- remaining list items. This way we guarantee lists to be non-empty.
list :: Config msg -> ListItem.ListItem msg -> [ListItem.ListItem msg] -> Miso.View msg
list (config_@Config {additionalAttributes}) firstListItem remainingListItems =
  let listItems =
        firstListItem : remainingListItems
   in Miso.nodeHtml
        "mdc-list"
        ( Maybe.mapMaybe
            id
            [ rootCs,
              nonInteractiveCs config_,
              denseCs config_,
              avatarListCs config_,
              twoLineCs config_,
              wrapFocusProp config_,
              clickHandler listItems,
              selectedIndexProp listItems
            ]
            ++ additionalAttributes
        )
        ( Prelude.map
            ( \listItem_ ->
                case listItem_ of
                  ListItem.ListItem (ListItem.Config {ListItem.node}) ->
                    node
                  ListItem.ListItemDivider node ->
                    node
                  ListItem.ListGroupSubheader node ->
                    node
            )
            listItems
        )

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-list")

nonInteractiveCs :: Config msg -> Maybe (Miso.Attribute msg)
nonInteractiveCs (Config {nonInteractive}) =
  if nonInteractive
    then Just (Miso.class_ "mdc-list--non-interactive")
    else Nothing

denseCs :: Config msg -> Maybe (Miso.Attribute msg)
denseCs (Config {dense}) =
  if dense
    then Just (Miso.class_ "mdc-list--dense")
    else Nothing

avatarListCs :: Config msg -> Maybe (Miso.Attribute msg)
avatarListCs (Config {avatarList}) =
  if avatarList
    then Just (Miso.class_ "mdc-list--avatar-list")
    else Nothing

twoLineCs :: Config msg -> Maybe (Miso.Attribute msg)
twoLineCs (Config {twoLine}) =
  if twoLine
    then Just (Miso.class_ "mdc-list--two-line")
    else Nothing

clickHandler :: [ListItem.ListItem msg] -> Maybe (Miso.Attribute msg)
clickHandler listItems =
  let getOnClick listItem_ =
        case listItem_ of
          ListItem.ListItem (ListItem.Config {ListItem.onClick}) ->
            Just onClick
          ListItem.ListItemDivider _ ->
            Nothing
          ListItem.ListGroupSubheader _ ->
            Nothing

      nthOnClick index =
        listItems
          |> Prelude.map getOnClick
          |> Maybe.mapMaybe id
          |> List.drop index
          |> List.head

      mergedClickHandler index = Maybe.fromJust (nthOnClick index) -- TODO: What if there is no onClick?
   in Just (Miso.Html.Event.on "MDCList:action" detailIndexDecoder mergedClickHandler)

detailIndexDecoder :: Miso.Decoder Int
detailIndexDecoder = Miso.Decoder {..}
  where
    decodeAt = Miso.DecodeTarget ["detail"]
    decoder = withObject "detail" $ \o -> o .: "index"

selectedIndexProp :: [ListItem.ListItem msg] -> Maybe (Miso.Attribute msg)
selectedIndexProp listItems =
  let selectedIndex =
        listItems
          |> List.filter
            ( \listItem_ ->
                case listItem_ of
                  ListItem.ListItem _ ->
                    True
                  ListItem.ListItemDivider _ ->
                    False
                  ListItem.ListGroupSubheader _ ->
                    False
            )
          |> List.zipWith
            ( \index listItem_ ->
                case listItem_ of
                  ListItem.ListItem (ListItem.Config {ListItem.selection}) ->
                    if selection /= Nothing
                      then Just index
                      else Nothing
                  ListItem.ListItemDivider _ ->
                    Nothing
                  ListItem.ListGroupSubheader _ ->
                    Nothing
            )
            [0 ..]
          |> List.filter Maybe.isJust
          |> Maybe.listToMaybe
          |> Maybe.fromMaybe Nothing
          |> Maybe.fromMaybe 0 -- TODO: 0 or -1?
   in Just (Miso.intProp "selectedIndex" selectedIndex)

-- | List group view function
group :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg
group additionalAttributes nodes =
  Miso.div_ (listGroupCs : additionalAttributes) nodes

listGroupCs :: Miso.Attribute msg
listGroupCs =
  Miso.class_ "mdc-list-group"

-- | List group subheader view function
subheader :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg
subheader additionalAttributes nodes =
  Miso.span_ (listGroupSubheaderCs : additionalAttributes) nodes

listGroupSubheaderCs :: Miso.Attribute msg
listGroupSubheaderCs =
  Miso.class_ "mdc-list-group__subheader"

wrapFocusProp :: Config msg -> Maybe (Miso.Attribute msg)
wrapFocusProp (Config {wrapFocus}) =
  Just (Miso.boolProp "wrapFocus" wrapFocus)
