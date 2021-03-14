{-# LANGUAGE OverloadedStrings #-}

module Material.Tab
  ( Config,
    config,
    setOnClick,
    setActive,
    setAttributes,
    Tab,
    tab,
    Internal.Content,
    icon,
    Icon,
    customIcon,
    svgIcon,
  )
where

import qualified Data.Void as Void
import qualified Material.Tab.Internal as Internal
import qualified Miso
import qualified Miso.String
import qualified Miso.Svg as Svg

-- | Configuration of a tab
type Config msg =
  Internal.Config msg

-- | Default configuration of a tab
config :: Config msg
config =
  Internal.Config
    { Internal.active = False,
      Internal.additionalAttributes = [],
      Internal.onClick = Nothing,
      Internal.content = Internal.Content {Internal.label = "", Internal.icon = Nothing}
    }

-- | Specify a message when the user clicks a tab
setOnClick :: msg -> Config msg -> Config msg
setOnClick onClick config_ =
  config_ {Internal.onClick = Just onClick}

-- | Specify whether the tab is active
setActive :: Bool -> Config msg -> Config msg
setActive active config_ =
  config_ {Internal.active = active}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {Internal.additionalAttributes = additionalAttributes}

-- | Tab type
-- Tabs can only be rendered within a [tab bar](Material-TabBar).
type Tab msg =
  Internal.Tab msg

-- | Tab constructor
tab :: Config msg -> String -> Maybe Icon -> Tab msg
tab config_ label icon =
  Internal.Tab (config_ {Internal.content = Internal.Content {Internal.label = label, Internal.icon = icon}})

-- | Icon type
type Icon =
  Internal.Icon

-- | Material Icon
--    Tab.tab Tab.config
--        { label = "Add to favorites"
--        , icon = Just (Tab.icon "favorite")
--        }
icon :: String -> Icon
icon iconName =
  customIcon Miso.i_ [Miso.class_ "material-icons"] [Miso.text (Miso.String.toMisoString iconName)]

-- | Custom icon
--    Tab.tab Tab.config
--        { label = "Font Awesome"
--        , icon =
--            Just
--                (Tab.customIcon Miso.i
--                    [ Miso.class_ "fab fa-font-awesome" ]
--                    []
--                )
--        }
customIcon ::
  ([Miso.Attribute Void.Void] -> [Miso.View Void.Void] -> Miso.View Void.Void) ->
  [Miso.Attribute Void.Void] ->
  [Miso.View Void.Void] ->
  Icon
customIcon node attributes nodes =
  Internal.Icon {Internal.node = node, Internal.attributes = attributes, Internal.nodes = nodes}

-- | SVG icon
--    Tab.tab Tab.config
--        { label = "Tab"
--        , icon =
--            Just
--                (Tab.svgIcon
--                    [ Svg.Attributes.viewBox "…" ]
--                    [-- …
--                    ]
--                )
--        }
svgIcon :: [Miso.Attribute Void.Void] -> [Miso.View Void.Void] -> Icon
svgIcon attributes nodes =
  Internal.SvgIcon {Internal.node = Svg.svg_, Internal.attributes = attributes, Internal.nodes = nodes}
