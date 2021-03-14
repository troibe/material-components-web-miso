{-# LANGUAGE OverloadedStrings #-}

module Material.Chip.Action
  ( Config,
    config,
    setOnClick,
    setIcon,
    setAttributes,
    chip,
    Chip,
    Icon,
    icon,
    customIcon,
    svgIcon,
  )
where

import qualified Data.Void as Void
import qualified Material.Chip.Action.Internal as Internal
import qualified Miso
import qualified Miso.String
import qualified Miso.Svg as Svg

-- | Configuration of an action chip
type Config msg =
  Internal.Config msg

-- | Default configuration of an action chip
config :: Config msg
config =
  Internal.Config
    { Internal.icon = Nothing,
      Internal.additionalAttributes = [],
      Internal.onClick = Nothing
    }

-- | Specify whether the chip displays an icon
setIcon :: Maybe Icon -> Config msg -> Config msg
setIcon icon_ config_ =
  config_ {Internal.icon = icon_}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {Internal.additionalAttributes = additionalAttributes}

-- | Specify a message when the user clicks on a chip
setOnClick :: msg -> Config msg -> Config msg
setOnClick onClick config_ =
  config_ {Internal.onClick = Just onClick}

-- | Action chip type
type Chip msg =
  Internal.Chip msg

-- | Action chip view function
chip :: Config msg -> String -> Chip msg
chip =
  Internal.Chip

-- | Icon type
type Icon =
  Internal.Icon

-- | Material Icon
--    ActionChip.chip
--        (ActionChip.config
--            |> ActionChip.setIcon (ActionChip.icon "favorite")
--        )
--        "Add to favorites"
icon :: String -> Icon
icon iconName =
  customIcon Miso.i_ [Miso.class_ "material-icons"] [Miso.text (Miso.String.toMisoString iconName)]

-- | Custom icon
--    ActionChip.chip
--        (ActionChip.config
--            |> ActionChip.setIcon
--                (ActionChip.customIcon Miso.i
--                    [ Miso.class_ "fab fa-font-awesome" ]
--                    []
--                )
--        )
--        "Font awesome"
customIcon ::
  ([Miso.Attribute Void.Void] -> [Miso.View Void.Void] -> Miso.View Void.Void) ->
  [Miso.Attribute Void.Void] ->
  [Miso.View Void.Void] ->
  Icon
customIcon node attributes nodes =
  Internal.Icon {Internal.node = node, Internal.attributes = attributes, Internal.nodes = nodes}

-- | SVG icon
--    ActionChip.chip
--        (ActionChip.config
--            |> ActionChip.setIcon
--                (ActionChip.svgIcon
--                    [ Svg.Attributes.viewBox "…" ]
--                    [-- …
--                    ]
--                )
--        )
--        "Font awesome"
svgIcon :: [Miso.Attribute Void.Void] -> [Miso.View Void.Void] -> Icon
svgIcon attributes nodes =
  Internal.SvgIcon {Internal.node = Svg.svg_, Internal.attributes = attributes, Internal.nodes = nodes}
