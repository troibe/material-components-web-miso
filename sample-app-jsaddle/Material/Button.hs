{-# LANGUAGE OverloadedStrings #-}

module Material.Button
  ( Config (..),
    config,
    setOnClick,
    setIcon,
    setTrailingIcon,
    setDisabled,
    setDense,
    setHref,
    setTarget,
    setTouch,
    setAttributes,
    Material.Button.text,
    outlined,
    raised,
    unelevated,
  )
where

import Data.Maybe
import Material.Button.Internal
import Miso.Html
import Miso.Html.Event
import Miso.String

-- | Default configuration of a button
config :: Config msg
config =
  Config
    { icon = Nothing,
      trailingIcon = False,
      disabled = False,
      dense = False,
      href = Nothing,
      target = Nothing,
      additionalAttributes = [],
      Material.Button.Internal.onClick = Nothing,
      touch = True
    }

-- | Specify whether the button features an icon
setIcon :: Maybe String -> Config msg -> Config msg
setIcon icon config_ =
  config_ {icon = icon}

-- | Specify whether a button's icon is a _trailing icon_.
-- Trailing icons are displayed after the button's label rather than before.
setTrailingIcon :: Bool -> Config msg -> Config msg
setTrailingIcon trailingIcon config_ =
  config_ {trailingIcon = trailingIcon}

-- | Specify whether the button is disabled
-- Disabled buttons cannot be interacted with and do not have no visual
-- interaction effect.
setDisabled :: Bool -> Config msg -> Config msg
setDisabled disabled config_ =
  config_ {disabled = disabled}

-- | Specify whether a button is _dense_
-- Dense buttons feature smaller than normal padding.
setDense :: Bool -> Config msg -> Config msg
setDense dense config_ =
  config_ {dense = dense}

-- | Specify whether a button is a _link button_.
-- Link buttons behave like normal HTML5 anchor tags. Note that link buttons
-- cannot be disabled and ignore that configuration option.
setHref :: Maybe String -> Config msg -> Config msg
setHref href config_ =
  config_ {href = href}

-- | Specify the target for a link button.
-- Note that this configuration option will be ignored by buttons that do not also
-- set `setHref`.
setTarget :: Maybe String -> Config msg -> Config msg
setTarget target config_ =
  config_ {target = target}

-- | Specify additional attributes
setAttributes :: [Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user clicks a button
setOnClick :: msg -> Config msg -> Config msg
setOnClick onClick config_ =
  config_ {Material.Button.Internal.onClick = Just onClick}

-- | Specify whether touch support is enabled (enabled by default)
-- Touch support is an accessibility guideline that states that touch targets
-- should be at least 48 x 48 pixels in size. Use this configuration option to
-- disable increased touch target size.
-- **Note:** Buttons with touch support will be wrapped in a HTML div element to
-- prevent potentially overlapping touch targets on adjacent elements.
setTouch :: Bool -> Config msg -> Config msg
setTouch touch config_ =
  config_ {touch = touch}

data Variant
  = Text
  | Raised
  | Unelevated
  | Outlined

button :: Variant -> Config msg -> String -> View msg
button variant ((config_@Config {additionalAttributes = additionalAttributes, touch = touch, href = href})) label =
  let wrapTouch node =
        if touch
          then div_ [class_ "mdc-touch-target-wrapper"] [node]
          else node
   in wrapTouch $
        nodeHtml
          "mdc-button"
          (mapMaybe id [disabledProp config_])
          [ ( if href /= Nothing
                then a_
                else button_
            )
              ( mapMaybe
                  id
                  [ rootCs,
                    variantCs variant,
                    denseCs config_,
                    touchCs config_,
                    disabledAttr config_,
                    tabIndexProp config_,
                    hrefAttr config_,
                    targetAttr config_,
                    clickHandler config_
                  ]
                  ++ additionalAttributes
              )
              ( mapMaybe
                  id
                  [ rippleElt,
                    leadingIconElt config_,
                    labelElt label,
                    trailingIconElt config_,
                    touchElt config_
                  ]
              )
          ]

-- | Text button variant (flush without outline)
text :: Config msg -> String -> View msg
text config_ label =
  button Text config_ label

-- | Outlined button variant (flush with outline)
outlined :: Config msg -> String -> View msg
outlined config_ label =
  button Outlined config_ label

-- | Raised button variant (contained with elevation)
raised :: Config msg -> String -> View msg
raised config_ label =
  button Raised config_ label

-- | Unelevated button variant (contained without elevation)
unelevated :: Config msg -> String -> View msg
unelevated config_ label =
  button Unelevated config_ label

rootCs :: Maybe (Attribute msg)
rootCs =
  Just (class_ "mdc-button")

disabledProp :: Config msg -> Maybe (Attribute msg)
disabledProp (Config {disabled = disabled}) =
  Just (boolProp "disabled" disabled)

disabledAttr :: Config msg -> Maybe (Attribute msg)
disabledAttr (Config {disabled = disabled}) =
  Just (disabled_ disabled)

tabIndexProp :: Config msg -> Maybe (Attribute msg)
tabIndexProp (Config {disabled = disabled}) =
  if disabled
    then Just (intProp "tabIndex" (-1))
    else Just (intProp "tabIndex" 0)

hrefAttr :: Config msg -> Maybe (Attribute msg)
hrefAttr (Config {href = href}) = case href of
  Nothing -> Nothing
  Just h -> Just (href_ . toMisoString $ h)

targetAttr :: Config msg -> Maybe (Attribute msg)
targetAttr (Config {href = href, target = target}) =
  if href /= Nothing
    then case target of
      Nothing -> Nothing
      Just t -> Just (target_ . toMisoString $ t)
    else Nothing

clickHandler :: Config msg -> Maybe (Attribute msg)
clickHandler (Config {Material.Button.Internal.onClick = onClick}) =
  case onClick of
    Nothing -> Nothing
    Just o -> Just (Miso.Html.Event.onClick o)

variantCs :: Variant -> Maybe (Attribute msg)
variantCs variant =
  case variant of
    Text ->
      Nothing
    Raised ->
      Just (class_ "mdc-button--raised")
    Unelevated ->
      Just (class_ "mdc-button--unelevated")
    Outlined ->
      Just (class_ "mdc-button--outlined")

denseCs :: Config msg -> Maybe (Attribute msg)
denseCs (Config {dense = dense}) =
  if dense
    then Just (class_ "mdc-button--dense")
    else Nothing

touchCs :: Config msg -> Maybe (Attribute msg)
touchCs (Config {touch = touch}) =
  if touch
    then Just (class_ "mdc-button--touch")
    else Nothing

iconElt :: Config msg -> Maybe (View msg)
iconElt (Config {icon = icon}) =
  case icon of
    Nothing -> Nothing
    Just i ->
      Just
        ( i_
            [ class_ "mdc-button__icon material-icons",
              textProp "aria-hidden" "true"
            ]
            [Miso.Html.text (toMisoString i)]
        )

rippleElt :: Maybe (View msg)
rippleElt =
  Just (div_ [class_ "mdc-button__ripple"] [])

leadingIconElt :: Config msg -> Maybe (View msg)
leadingIconElt (config_@Config {trailingIcon = trailingIcon}) =
  if not trailingIcon
    then iconElt config_
    else Nothing

trailingIconElt :: Config msg -> Maybe (View msg)
trailingIconElt (config_@Config {trailingIcon = trailingIcon}) =
  if trailingIcon
    then iconElt config_
    else Nothing

touchElt :: Config msg -> Maybe (View msg)
touchElt (Config {touch = touch}) =
  if touch
    then Just (div_ [class_ "mdc-button__touch"] [])
    else Nothing

labelElt :: String -> Maybe (View msg)
labelElt label =
  Just (span_ [class_ "mdc-button__label"] [Miso.Html.text (toMisoString label)])
