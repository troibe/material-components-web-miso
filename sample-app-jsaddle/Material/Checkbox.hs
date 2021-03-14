{-# LANGUAGE OverloadedStrings #-}

module Material.Checkbox
  ( Config (..),
    config,
    setOnChange,
    State (..),
    setState,
    setDisabled,
    setTouch,
    setAttributes,
    checkbox,
    checked,
    unchecked,
    indeterminate,
  )
where

import Data.Maybe as Maybe
import Material.Checkbox.Internal
import qualified Miso
import Miso.Svg.Attribute
import Miso.Svg.Element

-- | Default configuration of a checkbox
config :: Config msg
config =
  Config
    { state = Nothing,
      disabled = False,
      additionalAttributes = [],
      onChange = Nothing,
      touch = True
    }

-- | Specify a checkbox' state
-- A checkbox may be in `checked`, `unchecked` or `indeterminate` state.
setState :: Maybe State -> Config msg -> Config msg
setState state config_ =
  config_ {state = state}

-- | Specify whether a checkbox is disabled
-- Disabled checkboxes cannot be interacted with and have no visual interaction
-- effect.
setDisabled :: Bool -> Config msg -> Config msg
setDisabled disabled config_ =
  config_ {disabled = disabled}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user changes a checkbox
setOnChange :: msg -> Config msg -> Config msg
setOnChange onChange config_ =
  config_ {onChange = Just onChange}

-- | Specify whether touch support is enabled (enabled by default)
-- Touch support is an accessibility guideline that states that touch targets
-- should be at least 48 x 48 pixels in size. Use this configuration option to
-- disable increased touch target size.
-- **Note:** Checkboxes with touch support will be wrapped in a HTML div element
-- to prevent potentially overlapping touch targets on adjacent elements.
setTouch :: Bool -> Config msg -> Config msg
setTouch touch config_ =
  config_ {touch = touch}

-- | Unchecked state
unchecked :: State
unchecked =
  Unchecked

-- | Checked state
checked :: State
checked =
  Checked

-- | Indeterminate state
indeterminate :: State
indeterminate =
  Indeterminate

-- | Checkbox view function
checkbox :: Config msg -> Miso.View msg
checkbox config_@Config {touch = touch, additionalAttributes = additionalAttributes} =
  let wrapTouch node =
        if touch
          then Miso.div_ [Miso.class_ "mdc-touch-target-wrapper"] [node]
          else node
   in wrapTouch $
        Miso.nodeHtml
          "mdc-checkbox"
          ( mapMaybe
              id
              [ rootCs,
                touchCs config_,
                checkedProp config_,
                indeterminateProp config_,
                disabledProp config_
              ]
              ++ additionalAttributes
          )
          [ nativeControlElt config_,
            backgroundElt
          ]

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-checkbox")

touchCs :: Config msg -> Maybe (Miso.Attribute msg)
touchCs Config {touch = touch} =
  if touch
    then Just (Miso.class_ "mdc-checkbox--touch")
    else Nothing

checkedProp :: Config msg -> Maybe (Miso.Attribute msg)
checkedProp Config {state = state} =
  Just (Miso.boolProp "checked" (state == Just Checked))

indeterminateProp :: Config msg -> Maybe (Miso.Attribute msg)
indeterminateProp Config {state = state} =
  Just (Miso.boolProp "indeterminate" (state == Just Indeterminate))

disabledProp :: Config msg -> Maybe (Miso.Attribute msg)
disabledProp Config {disabled = disabled} =
  Just (Miso.boolProp "disabled" disabled)

changeHandler :: Config msg -> Maybe (Miso.Attribute msg)
changeHandler Config {state = state, onChange = onChange} =
  Maybe.maybe Nothing (\x -> Just (Miso.on "change" Miso.emptyDecoder (const x))) onChange

nativeControlElt :: Config msg -> Miso.View msg
nativeControlElt config_ =
  Miso.input_
    ( mapMaybe
        id
        [ Just (Miso.type_ "checkbox"),
          Just (Miso.class_ "mdc-checkbox__native-control"),
          checkedProp config_,
          indeterminateProp config_,
          changeHandler config_
        ]
    )

backgroundElt :: Miso.View msg
backgroundElt =
  Miso.div_
    [Miso.class_ "mdc-checkbox__background"]
    [ svg_
        [ Miso.class_ "mdc-checkbox__checkmark",
          viewBox_ "0 0 24 24"
        ]
        [ Miso.Svg.Element.path_
            [ Miso.class_ "mdc-checkbox__checkmark-path",
              fill_ "none",
              d_ "M1.73,12.91 8.1,19.28 22.79,4.59"
            ]
            []
        ],
      Miso.div_ [Miso.class_ "mdc-checkbox__mixedmark"] []
    ]
