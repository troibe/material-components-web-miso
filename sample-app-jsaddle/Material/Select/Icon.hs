{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Material.Select.Icon
  ( Icon,
    icon,
    customIcon,
    -- , svgIcon
    setOnInteraction,
    setDisabled,
  )
where

import Material.Select.Icon.Internal (Icon (..))
import qualified Miso
import Miso.String

-- | Icon type

-- data alias Icon msg =
--    Material.Select.Icon.Internal.Icon msg

-- | Material Icon
--
--    Select.filled
--        (Select.config
--            |> Select.setLeadingIcon
--                (Just (Select.icon "favorite"))
--        )
icon :: String -> Icon msg
icon iconName =
  customIcon Miso.i_ [Miso.class_ "material-icons"] [Miso.text $ toMisoString iconName]

-- | Custom icon
--
--    Select.raised
--        (Select.config
--            |> Select.setLeadingIcon
--                (Just
--                    (Select.customIcon Miso.i
--                        [ Miso.class_ "fab fa-font-awesome" ]
--                        []
--                    )
--                )
--        )
customIcon ::
  ([Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg) ->
  [Miso.Attribute msg] ->
  [Miso.View msg] ->
  Icon msg
customIcon node attributes nodes =
  Icon
    { node = node,
      attributes = attributes,
      nodes = nodes,
      onInteraction = Nothing,
      disabled = False
    }

-- | SVG icon
--
--    Select.raised
--        (Select.config
--            |> Select.setLeadingIcon
--                (Just
--                    (Select.svgIcon
--                        [ Svg.Attributes.viewBox "…" ]
--                        [-- …
--                        ]
--                    )
--                )
--        )

{-- svgIcon :: [Svg.Attribute msg] -> [Svg msg] -> Icon msg
svgIcon attributes nodes =
    SvgIcon
        { node = Svg.svg
        , attributes = attributes
        , nodes = nodes
        , onInteraction = Nothing
        , disabled = False
        }
--}

-- | Specify a message when the user interacts with the icon
--
--    SelectIcon.icon "favorite"
--        |> SelectIcon.setOnInteraction Interacted
setOnInteraction :: msg -> Icon msg -> Icon msg
setOnInteraction onInteraction icon_ =
  case icon_ of
    icon@Icon {..} ->
      icon {onInteraction = onInteraction}

{-- SvgIcon svgIcon ->
    SvgIcon svgIcon { onInteraction = Just onInteraction } --}

-- | Specify an icon to be disabled
--
-- Disabled icons cannot be interacted with and have no visual interaction
-- effect.
--
--    SelectIcon.icon "favorite"
--        |> SelectIcon.setDisabled True
setDisabled :: Bool -> Icon msg -> Icon msg
setDisabled disabled icon_ =
  case icon_ of
    icon@Icon {..} ->
      icon {disabled = disabled}

{-- SvgIcon svgIcon ->
    SvgIcon svgIcon { disabled = disabled } --}
