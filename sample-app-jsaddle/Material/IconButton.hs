{-# LANGUAGE OverloadedStrings #-}

module Material.IconButton
  ( Config (..),
    config,
    setOnClick,
    setDisabled,
    setLabel,
    setAttributes,
    iconButton,
    custom,
  )
where

import Data.Maybe
import Material.IconButton.Internal
import Miso.Html
import Miso.Html.Event
import Miso.String

-- | Default icon button configuration
config :: Config msg
config =
  Config
    { disabled = False,
      label = Nothing,
      additionalAttributes = [],
      Material.IconButton.Internal.onClick = Nothing
    }

-- | Specify whether an icon button is disabled
-- Disabled icon buttons cannot be interacted with and have no visual interaction
-- effect.
setDisabled :: Bool -> Config msg -> Config msg
setDisabled disabled config_ =
  config_ {disabled = disabled}

-- | Specify an icon button's HTML5 arial-label attribute
setLabel :: Maybe String -> Config msg -> Config msg
setLabel label config_ =
  config_ {label = label}

-- | Specify additional attributes
setAttributes :: [Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user clicks on an icon button
setOnClick :: msg -> Config msg -> Config msg
setOnClick onClick config_ =
  config_ {Material.IconButton.Internal.onClick = Just onClick}

-- | Icon button view function
iconButton :: Config msg -> String -> View msg
iconButton ((config_@Config {additionalAttributes = additionalAttributes})) iconName =
  nodeHtml
    "mdc-icon-button"
    ( mapMaybe
        id
        [ rootCs,
          materialIconsCs,
          tabIndexProp,
          clickHandler config_
        ]
        ++ additionalAttributes
    )
    [text (toMisoString iconName)]

-- | TODO
custom :: Config msg -> [View msg] -> View msg
custom ((config_@Config {additionalAttributes = additionalAttributes})) nodes =
  nodeHtml
    "mdc-icon-button"
    ( mapMaybe
        id
        [ rootCs,
          tabIndexProp,
          clickHandler config_
        ]
        ++ additionalAttributes
    )
    nodes

rootCs :: Maybe (Attribute msg)
rootCs =
  Just (class_ "mdc-icon-button")

materialIconsCs :: Maybe (Attribute msg)
materialIconsCs =
  Just (class_ "material-icons")

tabIndexProp :: Maybe (Attribute msg)
tabIndexProp =
  Just (intProp "tabindex" 0)

clickHandler :: Config msg -> Maybe (Attribute msg)
clickHandler (Config {Material.IconButton.Internal.onClick = onClick}) = case onClick of
  Nothing -> Nothing
  Just o -> Just (Miso.Html.Event.onClick o)
