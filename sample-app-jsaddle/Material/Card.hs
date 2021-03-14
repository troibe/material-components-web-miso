{-# LANGUAGE OverloadedStrings #-}

module Material.Card
  ( Config (..),
    config,
    setOutlined,
    setAttributes,
    card,
    Content (..),
    Block (..),
    block,
    squareMedia,
    sixteenToNineMedia,
    media,
    primaryAction,
    Actions,
    cardActions,
    Button,
    button,
    Icon,
    icon,
    fullBleedActions,
  )
where

import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe
import qualified Material.Button as Button
import qualified Material.Button.Internal
import qualified Material.IconButton as IconButton
import qualified Material.IconButton.Internal
import Miso.Html
import Miso.String

-- | Configuration of a card
data Config msg = Config
  { outlined :: Bool,
    additionalAttributes :: [Attribute msg]
  }

-- | Default configuration of a card
config :: Config msg
config =
  Config
    { outlined = False,
      additionalAttributes = []
    }

-- | Specify whether a card should have a visual outline
setOutlined :: Bool -> Config msg -> Config msg
setOutlined outlined config_ =
  config_ {outlined = outlined}

-- | Specify additional attributes
setAttributes :: [Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Card view function
card :: Config msg -> Content msg -> View msg
card (config_@Config {additionalAttributes = additionalAttributes}) content =
  nodeHtml
    "mdc-card"
    ( mapMaybe
        id
        [ rootCs,
          outlinedCs config_
        ]
        ++ additionalAttributes
    )
    ( List.concat
        [ blocksElt content,
          actionsElt content
        ]
    )

blocksElt :: Content msg -> [View msg]
blocksElt Content {blocks = blocks} =
  List.map (\(Block html) -> html) blocks

actionsElt :: Content msg -> [View msg]
actionsElt (Content {actions = actions}) =
  case actions of
    Just (Actions {buttons = buttons, icons = icons, fullBleed = fullBleed}) ->
      [ div_
          ( mapMaybe
              id
              [ Just (class_ "mdc-card__actions"),
                if fullBleed
                  then Just (class_ "mdc-card__actions--full-bleed")
                  else Nothing
              ]
          )
          ( List.concat
              [ if not (List.null buttons)
                  then
                    [ div_
                        [class_ "mdc-card__action-buttons"]
                        (List.map (\(Button button_) -> button_) buttons)
                    ]
                  else [],
                if not (List.null icons)
                  then
                    [ div_
                        [class_ "mdc-card__action-icons"]
                        (List.map (\(Icon icon_) -> icon_) icons)
                    ]
                  else []
              ]
          )
      ]
    Nothing ->
      []

rootCs :: Maybe (Attribute msg)
rootCs =
  Just (class_ "mdc-card")

outlinedCs :: Config msg -> Maybe (Attribute msg)
outlinedCs (Config {outlined = outlined}) =
  if outlined
    then Just (class_ "mdc-card--outlined")
    else Nothing

-- | The content of a card is comprised of _blocks_ and _actions_.
data Content msg = Content
  { blocks :: [Block msg],
    actions :: Maybe (Actions msg)
  }

-- | A card's content block
data Block msg
  = Block (View msg)

-- | Card block containing arbitrary HTML
--    Card.block <|
--        Html.div [] [ text "Lorem ipsum…" ]
block :: View msg -> Block msg
block =
  Block

data Aspect
  = Square
  | SixteenToNine

mediaView :: Maybe Aspect -> [Attribute msg] -> String -> Block msg
mediaView aspect additionalAttributes backgroundImage =
  Block $
    div_
      ( mapMaybe
          id
          [ mediaCs,
            backgroundImageAttr backgroundImage,
            aspectCs aspect
          ]
          ++ additionalAttributes
      )
      []

-- | Card media block with a square aspect ratio
squareMedia :: [Attribute msg] -> String -> Block msg
squareMedia additionalAttributes backgroundImage =
  mediaView (Just Square) additionalAttributes backgroundImage

-- | Card media block with a 16:9 aspect ratio
sixteenToNineMedia :: [Attribute msg] -> String -> Block msg
sixteenToNineMedia additionalAttributes backgroundImage =
  mediaView (Just SixteenToNine) additionalAttributes backgroundImage

-- | Card media block of unspecified aspect ratio
media :: [Attribute msg] -> String -> Block msg
media additionalAttributes backgroundImage =
  mediaView Nothing additionalAttributes backgroundImage

mediaCs :: Maybe (Attribute msg)
mediaCs =
  Just (class_ "mdc-card__media")

backgroundImageAttr :: String -> Maybe (Attribute msg)
backgroundImageAttr url =
  Just (style_ $ M.singleton "background-image" (toMisoString ("url(\"" ++ url ++ "\")")))

aspectCs :: Maybe Aspect -> Maybe (Attribute msg)
aspectCs aspect =
  case aspect of
    Just Square ->
      Just (class_ "mdc-card__media--square")
    Just SixteenToNine ->
      Just (class_ "mdc-card__media--16-9")
    Nothing ->
      Nothing

-- | A card's primary action block
primaryAction :: [Attribute msg] -> [Block msg] -> [Block msg]
primaryAction additionalAttributes blocks =
  [ Block $
      div_
        ( [ primaryActionCs,
            tabIndexProp 0
          ]
            ++ additionalAttributes
        )
        (List.map (\(Block html) -> html) blocks)
  ]

primaryActionCs :: Attribute msg
primaryActionCs =
  class_ "mdc-card__primary-action"

tabIndexProp :: Int -> Attribute msg
tabIndexProp tabIndex =
  intProp "tabIndex" tabIndex

-- | Card actions type
data Actions msg = Actions
  { buttons :: [Button msg],
    icons :: [Icon msg],
    fullBleed :: Bool
  }

-- | Card actions
-- A card may contain as actions buttons as well as icons.
cardActions :: [Button msg] -> [Icon msg] -> Actions msg
cardActions buttons icons =
  Actions {buttons = buttons, icons = icons, fullBleed = False}

-- | Card full bleed action
-- If a card's action is comprised of a single button, that button can be made
-- full width by using `cardFullBleedActions`.
--    Card.fullBleedActions
--        (Card.button Button.config "Visit")
fullBleedActions :: Button msg -> Actions msg
fullBleedActions button_ =
  Actions {buttons = [button_], icons = [], fullBleed = True}

-- | Card action's button type
data Button msg
  = Button (View msg)

-- | A card action button
--    Card.button Button.config "Visit"
button :: Button.Config msg -> String -> Button msg
button (buttonConfig@Button.Config {Material.Button.Internal.additionalAttributes = additionalAttributes}) label =
  Button $
    Button.text
      ( buttonConfig
          { Material.Button.Internal.additionalAttributes =
              class_ "mdc-card__action" :
              class_ "mdc-card__action--button" :
              additionalAttributes
          }
      )
      label

-- | Card action's icon type
data Icon msg
  = Icon (View msg)

-- | Card action icon
--    Card.icon IconButton.config "favorite"
icon :: IconButton.Config msg -> String -> Icon msg
icon (iconButtonConfig@IconButton.Config {Material.IconButton.Internal.additionalAttributes = additionalAttributes}) iconName =
  Icon $
    IconButton.iconButton
      ( iconButtonConfig
          { Material.IconButton.Internal.additionalAttributes =
              class_ "mdc-card__action" :
              class_ "mdc-card__action--icon" :
              additionalAttributes
          }
      )
      iconName
