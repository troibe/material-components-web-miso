{-# LANGUAGE OverloadedStrings #-}

module Material.Radio
  ( Config,
    config,
    setOnChange,
    setChecked,
    setDisabled,
    setTouch,
    setAttributes,
    radio,
  )
where

import qualified Data.Maybe as Maybe
import qualified Miso

-- | Radio button configuration
data Config msg = Config
  { checked :: Bool,
    disabled :: Bool,
    additionalAttributes :: [Miso.Attribute msg],
    onChange :: Maybe msg,
    touch :: Bool
  }

-- | Default radio button configuration
config :: Config msg
config =
  Config
    { checked = False,
      disabled = False,
      additionalAttributes = [],
      onChange = Nothing,
      touch = True
    }

-- | Specify whether a radio button is checked
setChecked :: Bool -> Config msg -> Config msg
setChecked checked config_ =
  config_ {checked = checked}

-- | Specify whether a radio button is disabled
-- Disabled radio buttons cannot be interacted with and have no visual interaction
-- effect.
setDisabled :: Bool -> Config msg -> Config msg
setDisabled disabled config_ =
  config_ {disabled = disabled}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user changes a radio
setOnChange :: msg -> Config msg -> Config msg
setOnChange onChange config_ =
  config_ {onChange = Just onChange}

-- | Specify whether touch support is enabled (enabled by default)
-- Touch support is an accessibility guideline that states that touch targets
-- should be at least 48 x 48 pixels in size. Use this configuration option to
-- disable increased touch target size.
-- **Note:** Radios with touch support will be wrapped in a HTML div element to
-- prevent potentially overlapping touch targets on adjacent elements.
setTouch :: Bool -> Config msg -> Config msg
setTouch touch config_ =
  config_ {touch = touch}

-- | Radio button view function
radio :: Config msg -> Miso.View msg
radio (config_@Config {touch = touch, additionalAttributes = additionalAttributes}) =
  let wrapTouch node =
        if touch
          then Miso.div_ [Miso.class_ "mdc-touch-target-wrapper"] [node]
          else node
   in wrapTouch $
        Miso.nodeHtml
          "mdc-radio"
          ( Maybe.mapMaybe
              id
              [ rootCs,
                touchCs config_,
                checkedProp config_,
                disabledProp config_
              ]
              ++ additionalAttributes
          )
          [ nativeControlElt config_,
            backgroundElt,
            rippleElt
          ]

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-radio")

touchCs :: Config msg -> Maybe (Miso.Attribute msg)
touchCs (Config {touch = touch}) =
  if touch
    then Just (Miso.class_ "mdc-radio--touch")
    else Nothing

checkedProp :: Config msg -> Maybe (Miso.Attribute msg)
checkedProp (Config {checked = checked}) =
  Just (Miso.boolProp "checked" checked)

disabledProp :: Config msg -> Maybe (Miso.Attribute msg)
disabledProp (Config {disabled = disabled}) =
  Just (Miso.boolProp "disabled" disabled)

changeHandler :: Config msg -> Maybe (Miso.Attribute msg)
changeHandler (Config {checked = checked, onChange = onChange}) =
  Maybe.maybe Nothing (\x -> Just (Miso.on "change" Miso.emptyDecoder (const x))) onChange

nativeControlElt :: Config msg -> Miso.View msg
nativeControlElt config_ =
  Miso.input_
    ( Maybe.mapMaybe
        id
        [ nativeControlCs,
          radioTypeAttr,
          checkedProp config_,
          changeHandler config_
        ]
    )

nativeControlCs :: Maybe (Miso.Attribute msg)
nativeControlCs =
  Just (Miso.class_ "mdc-radio__native-control")

radioTypeAttr :: Maybe (Miso.Attribute msg)
radioTypeAttr =
  Just (Miso.type_ "radio")

backgroundElt :: Miso.View msg
backgroundElt =
  Miso.div_ [Miso.class_ "mdc-radio__background"] [outerCircleElt, innerCircleElt]

outerCircleElt :: Miso.View msg
outerCircleElt =
  Miso.div_ [Miso.class_ "mdc-radio__outer-circle"] []

innerCircleElt :: Miso.View msg
innerCircleElt =
  Miso.div_ [Miso.class_ "mdc-radio__inner-circle"] []

rippleElt :: Miso.View msg
rippleElt =
  Miso.div_ [Miso.class_ "mdc-radio__ripple"] []
