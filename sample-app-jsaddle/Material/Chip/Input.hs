{-# LANGUAGE OverloadedStrings #-}

module Material.Chip.Input
  ( Config,
    config,
    setOnClick,
    setOnDelete,
    setLeadingIcon,
    setTrailingIcon,
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
import qualified Material.Chip.Input.Internal as Internal
import qualified Miso
import qualified Miso.String
import qualified Miso.Svg as Svg

-- | Configuration of an input chip
type Config msg =
  Internal.Config msg

-- | Default configuration of an input chip
config :: Config msg
config =
  Internal.Config
    { Internal.leadingIcon = Nothing,
      Internal.trailingIcon = Nothing,
      Internal.additionalAttributes = [],
      Internal.onDelete = Nothing,
      Internal.onClick = Nothing
    }

-- | Specify whether an input chip displays a leading icon
setLeadingIcon :: Maybe Icon -> Config msg -> Config msg
setLeadingIcon leadingIcon config_ =
  config_ {Internal.leadingIcon = leadingIcon}

-- | Specify whether an input chip displays a trailing icon
setTrailingIcon :: Maybe Icon -> Config msg -> Config msg
setTrailingIcon trailingIcon config_ =
  config_ {Internal.trailingIcon = trailingIcon}

-- | Specify additonal attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {Internal.additionalAttributes = additionalAttributes}

-- | Specify a message when the user clicks on a chip's trailing icon
setOnDelete :: msg -> Config msg -> Config msg
setOnDelete onDelete config_ =
  config_ {Internal.onDelete = Just onDelete}

-- | Specify a message when the user clicks on a chip
setOnClick :: msg -> Config msg -> Config msg
setOnClick onClick config_ =
  config_ {Internal.onClick = Just onClick}

-- | Input chip type
type Chip msg =
  Internal.Chip msg

-- | Input chip view function
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
