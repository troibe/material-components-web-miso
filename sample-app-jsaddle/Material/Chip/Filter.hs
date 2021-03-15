{-# LANGUAGE OverloadedStrings #-}

module Material.Chip.Filter
  ( Config,
    config,
    setOnChange,
    setIcon,
    setSelected,
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
import qualified Material.Chip.Filter.Internal as Internal
import qualified Miso
import qualified Miso.String
import qualified Miso.Svg as Svg

-- | Configuration of a filter chip
type Config msg =
  Internal.Config msg

-- | Default configuration of a filter chip
config :: Config msg
config =
  Internal.Config
    { Internal.selected = False,
      Internal.icon = Nothing,
      Internal.onChange = Nothing,
      Internal.additionalAttributes = []
    }

-- | Specify whether a filter chip is selected
setSelected :: Bool -> Config msg -> Config msg
setSelected selected config_ =
  config_ {Internal.selected = selected}

-- | Specify whether a chip displays an icon
setIcon :: Maybe Icon -> Config msg -> Config msg
setIcon icon_ config_ =
  config_ {Internal.icon = icon_}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {Internal.additionalAttributes = additionalAttributes}

-- | Specify a message when the user clicks on a chip
setOnChange :: msg -> Config msg -> Config msg
setOnChange onChange config_ =
  config_ {Internal.onChange = Just onChange}

-- | Filter chip type
type Chip msg =
  Internal.Chip msg

-- | Filter chip view function
chip :: Config msg -> String -> Chip msg
chip =
  Internal.Chip

-- | Icon type
type Icon =
  Internal.Icon

-- | Material Icon
--    FilterChip.chip
--        (FilterChip.config
--            |> FilterChip.setIcon (FilterChip.icon "favorite")
--        )
--        "Add to favorites"
icon :: String -> Icon
icon iconName =
  customIcon Miso.i_ [Miso.class_ "material-icons"] [Miso.text (Miso.String.toMisoString iconName)]

-- | Custom icon
--    FilterChip.chip
--        (FilterChip.config
--            |> FilterChip.setIcon
--                (FilterChip.customIcon Miso.i
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
--    FilterChp.chip
--        (ActonChip.config
--            > FilterChip.setIcon
--                (FilterChip.svgIcon
--                    [ Svg.Attributes.viewBox "…" ]
--                    [-- …
--                    ]
--                )
--        )
--        "Fon awesome"
svgIcon :: [Miso.Attribute Void.Void] -> [Miso.View Void.Void] -> Icon
svgIcon attributes nodes =
  Internal.SvgIcon {Internal.node = Svg.svg_, Internal.attributes = attributes, Internal.nodes = nodes}
