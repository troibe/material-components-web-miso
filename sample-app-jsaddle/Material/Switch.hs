{-# LANGUAGE OverloadedStrings #-}

module Material.Switch
  ( switch,
    Config,
    config,
    setOnChange,
    setChecked,
    setDisabled,
    setAttributes,
  )
where

import Data.Maybe as Maybe
import qualified Miso

-- | Configuration of a switch
data Config msg = Config
  { checked :: Bool,
    disabled :: Bool,
    additionalAttributes :: [Miso.Attribute msg],
    onChange :: Maybe msg
  }

-- | Default configuration of a switch
config :: Config msg
config =
  Config
    { checked = False,
      disabled = False,
      additionalAttributes = [],
      onChange = Nothing
    }

-- | Specify whether a switch is checked
setChecked :: Bool -> Config msg -> Config msg
setChecked checked config_ =
  config_ {checked = checked}

-- | Specify whether a switch is disabled
-- Disabled switches cannot be interacted with and have no visual interaction
-- effect.
setDisabled :: Bool -> Config msg -> Config msg
setDisabled disabled config_ =
  config_ {disabled = disabled}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user changes a switch
setOnChange :: msg -> Config msg -> Config msg
setOnChange onChange config_ =
  config_ {onChange = Just onChange}

-- | Switch view function
switch :: Config msg -> Miso.View msg
switch (config_@Config {additionalAttributes = additionalAttributes}) =
  Miso.nodeHtml
    "mdc-switch"
    ( Maybe.mapMaybe
        id
        [ rootCs,
          checkedProp config_,
          disabledProp config_
        ]
        ++ additionalAttributes
    )
    [ trackElt,
      thumbUnderlayElt config_
    ]

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-switch")

checkedProp :: Config msg -> Maybe (Miso.Attribute msg)
checkedProp (Config {checked = checked}) =
  Just (Miso.boolProp "checked" checked)

disabledProp :: Config msg -> Maybe (Miso.Attribute msg)
disabledProp (Config {disabled = disabled}) =
  Just (Miso.boolProp "disabled" disabled)

nativeControlCs :: Maybe (Miso.Attribute msg)
nativeControlCs =
  Just (Miso.class_ "mdc-switch__native-control")

switchRoleAttr :: Maybe (Miso.Attribute msg)
switchRoleAttr =
  Just (Miso.textProp "role" "switch")

checkboxTypeAttr :: Maybe (Miso.Attribute msg)
checkboxTypeAttr =
  Just (Miso.type_ "checkbox")

changeHandler :: Config msg -> Maybe (Miso.Attribute msg)
changeHandler (Config {onChange = onChange}) =
  Maybe.maybe Nothing (\x -> Just (Miso.on "change" Miso.emptyDecoder (const x))) onChange

trackElt :: Miso.View msg
trackElt =
  Miso.div_ [Miso.class_ "mdc-switch__track"] []

thumbUnderlayElt :: Config msg -> Miso.View msg
thumbUnderlayElt config_ =
  Miso.div_
    [Miso.class_ "mdc-switch__thumb-underlay"]
    [ thumbElt,
      nativeControlElt config_
    ]

thumbElt :: Miso.View msg
thumbElt =
  Miso.div_ [Miso.class_ "mdc-switch__thumb"] []

nativeControlElt :: Config msg -> Miso.View msg
nativeControlElt config_ =
  Miso.input_
    ( Maybe.mapMaybe
        id
        [ nativeControlCs,
          checkboxTypeAttr,
          switchRoleAttr,
          checkedProp config_,
          changeHandler config_
        ]
    )
