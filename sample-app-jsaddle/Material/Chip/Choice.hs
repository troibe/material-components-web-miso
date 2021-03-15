{-# LANGUAGE OverloadedStrings #-}

module Material.Chip.Choice
  ( Config,
    config,
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
import qualified Material.Chip.Choice.Internal as Internal
import qualified Miso
import qualified Miso.String
import qualified Miso.Svg as Svg

-- | Configuration of a choice chip
type Config msg =
  Internal.Config msg

-- | Default configuration of a choice chip
config :: Config msg
config =
  Internal.Config
    { Internal.icon = Nothing,
      Internal.additionalAttributes = []
    }

-- | Specify whether the chip displays an icon
setIcon :: Maybe Icon -> Config msg -> Config msg
setIcon icon_ config_ =
  config_ {Internal.icon = icon_}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {Internal.additionalAttributes = additionalAttributes}

-- | Choice chip type
type Chip a msg =
  Internal.Chip a msg

-- | Choice chip view function
chip :: Config msg -> a -> Chip a msg
chip =
  Internal.Chip

-- | Icon type
type Icon =
  Internal.Icon

-- | Material Icon
--    ChoiceChip.chip
--        (ChoiceChip.config
--            |> ChoiceChip.setIcon (ChoiceChip.icon "favorite")
--        )
--        "Add to favorites"
icon :: String -> Icon
icon iconName =
  customIcon Miso.i_ [Miso.class_ "material-icons"] [Miso.text (Miso.String.toMisoString iconName)]

-- | Custom icon
--    ChoiceChip.chip
--        (ChoiceChip.config
--            |> ChoiceChip.setIcon
--                (ChoiceChip.customIcon Miso.i
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
--    ChoiceChp.chip
--        (ActonChip.config
--            > ChoiceChip.setIcon
--                (ChoiceChip.svgIcon
--                    [ Svg.Attributes.viewBox "…" ]
--                    [-- …
--                    ]
--                )
--        )
--        "Fon awesome"
svgIcon :: [Miso.Attribute Void.Void] -> [Miso.View Void.Void] -> Icon
svgIcon attributes nodes =
  Internal.SvgIcon {Internal.node = Svg.svg_, Internal.attributes = attributes, Internal.nodes = nodes}
