{-# LANGUAGE OverloadedStrings #-}

module Material.IconToggle
  ( Config,
    config,
    setOnChange,
    setOn,
    setDisabled,
    setLabel,
    setAttributes,
    iconToggle,
    Icon,
    icon,
    customIcon,
    svgIcon,
  )
where

import qualified Data.Function
import qualified Data.Maybe as Maybe
import qualified Data.Void as Void
import qualified Miso
import qualified Miso.String
import qualified Miso.Svg as Svg

(|>) = (Data.Function.&)

(<|) = (Data.Function.$)

(<<) = (Data.Function..)

-- | Icon toggle configuration
data Config msg = Config
  { on :: Bool,
    disabled :: Bool,
    label :: Maybe String,
    additionalAttributes :: [Miso.Attribute msg],
    onChange :: Maybe msg
  }

-- | Default icon toggle configuration
config :: Config msg
config =
  Config
    { on = False,
      disabled = False,
      label = Nothing,
      additionalAttributes = [],
      onChange = Nothing
    }

-- | Specify whether an icon toggle is on
setOn :: Bool -> Config msg -> Config msg
setOn on config_ =
  config_ {on = on}

-- | Specify whether an icon toggle is disabled
-- Disabled icon buttons cannot be interacted with and have no visual interaction
-- effect.
setDisabled :: Bool -> Config msg -> Config msg
setDisabled disabled config_ =
  config_ {disabled = disabled}

-- | Specify the HTML5 aria-label attribute of an icon toggle
setLabel :: Maybe String -> Config msg -> Config msg
setLabel label config_ =
  config_ {label = label}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user changes the icon toggle
setOnChange :: msg -> Config msg -> Config msg
setOnChange onChange config_ =
  config_ {onChange = Just onChange}

-- | Icon toggle view function
iconToggle :: Config msg -> Icon -> Icon -> Miso.View msg
iconToggle (config_@Config {additionalAttributes = additionalAttributes}) onIcon offIcon =
  Miso.nodeHtml
    "mdc-icon-button"
    ( Maybe.mapMaybe
        id
        [ rootCs,
          onProp config_,
          tabIndexProp,
          ariaHiddenAttr,
          ariaPressedAttr config_,
          ariaLabelAttr config_,
          changeHandler config_,
          disabledAttr config_
        ]
        ++ additionalAttributes
    )
    [ iconElt "mdc-icon-button__icon mdc-icon-button__icon--on" onIcon,
      iconElt "mdc-icon-button__icon" offIcon
    ]

iconElt :: String -> Icon -> Miso.View msg
iconElt className icon_ =
  fmap Void.absurd
    <| case icon_ of
      Icon {node = node, attributes = attributes, nodes = nodes} ->
        node (Miso.class_ (Miso.String.toMisoString className) : attributes) nodes
      SvgIcon {node = node, attributes = attributes, nodes = nodes} ->
        node (Svg.class_' (Miso.String.toMisoString className) : attributes) nodes

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-icon-button")

onProp :: Config msg -> Maybe (Miso.Attribute msg)
onProp (Config {on = on}) =
  Just (Miso.boolProp "on" on)

tabIndexProp :: Maybe (Miso.Attribute msg)
tabIndexProp =
  Just (Miso.intProp "tabindex" 0)

ariaHiddenAttr :: Maybe (Miso.Attribute msg)
ariaHiddenAttr =
  Just (Miso.textProp "aria-hidden" "true")

ariaPressedAttr :: Config msg -> Maybe (Miso.Attribute msg)
ariaPressedAttr (Config {on = on}) =
  Just
    ( Miso.textProp
        "aria-pressed"
        ( if on
            then "true"
            else "false"
        )
    )

ariaLabelAttr :: Config msg -> Maybe (Miso.Attribute msg)
ariaLabelAttr (Config {label = label}) =
  fmap (Miso.textProp "aria-label" << Miso.String.toMisoString) label

changeHandler :: Config msg -> Maybe (Miso.Attribute msg)
changeHandler (Config {onChange = onChange}) =
  fmap
    (Miso.on "MDCIconButtonToggle:change" Miso.emptyDecoder << const)
    onChange

disabledAttr :: Config msg -> Maybe (Miso.Attribute msg)
disabledAttr (Config {disabled = disabled}) =
  Just (Miso.disabled_ disabled)

-- | Icon type
data Icon
  = Icon
      { node :: [Miso.Attribute Void.Void] -> [Miso.View Void.Void] -> Miso.View Void.Void,
        attributes :: [Miso.Attribute Void.Void],
        nodes :: [Miso.View Void.Void]
      }
  | SvgIcon
      { node :: [Miso.Attribute Void.Void] -> [Miso.View Void.Void] -> Miso.View Void.Void,
        attributes :: [Miso.Attribute Void.Void],
        nodes :: [Miso.View Void.Void]
      }

-- | Material Icon
--    IconToggle.iconToggle IconToggle.config
--        { offIcon = IconToggle.icon "favorite"
--        , onIcon = IconToggle.icon "favorite_border"
--        }
icon :: String -> Icon
icon iconName =
  customIcon Miso.i_ [Miso.class_ "material-icons"] [Miso.text (Miso.String.toMisoString iconName)]

-- | Custom icon
--    IconToggle.iconToggle IconToggle.config
--        { offIcon =
--            IconToggle.customIcon Miso.i
--                [ Miso.class_ "fab fa-font-awesome-alt" ]
--                []
--        , onIcon =
--            IconToggle.customIcon Miso.i
--                [ Miso.class_ "fab fa-font-awesome" ]
--                []
--        }
customIcon ::
  ([Miso.Attribute Void.Void] -> [Miso.View Void.Void] -> Miso.View Void.Void) ->
  [Miso.Attribute Void.Void] ->
  [Miso.View Void.Void] ->
  Icon
customIcon node attributes nodes =
  Icon {node = node, attributes = attributes, nodes = nodes}

-- | SVG icon
--    IconToggle.iconToggle IconToggle.config
--        { offIcon =
--            IconToggle.svgIcon [ Svg.Attributes.viewBox "…" ]
--                [-- …
--                ]
--        , onIcon =
--            IconToggle.svgIcon [ Svg.Attributes.viewBox "…" ]
--                [-- …
--                ]
--        }
svgIcon :: [Miso.Attribute Void.Void] -> [Miso.View Void.Void] -> Icon
svgIcon attributes nodes =
  SvgIcon {node = Svg.svg_, attributes = attributes, nodes = nodes}
