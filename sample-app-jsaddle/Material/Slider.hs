{-# LANGUAGE OverloadedStrings #-}

module Material.Slider
  ( Config,
    config,
    setOnInput,
    setDiscrete,
    setDisplayMarkers,
    setMin,
    setMax,
    setStep,
    setValue,
    setDisabled,
    setAttributes,
    slider,
  )
where

import Data.Aeson.Types
import qualified Data.Function
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Miso
import qualified Miso.String
import qualified Miso.Svg
import Prelude hiding (max, min)

(|>) = (Data.Function.&)

(<|) = (Data.Function.$)

(<<) = (Data.Function..)

-- TODO: Prevent FOUC

-- | Configuration of a slider
data Config msg = Config
  { discrete :: Bool,
    displayMarkers :: Bool,
    min :: Maybe Float,
    max :: Maybe Float,
    step :: Maybe Float,
    value :: Maybe Float,
    disabled :: Bool,
    additionalAttributes :: [Miso.Attribute msg],
    onInput :: Maybe (Float -> msg)
  }

-- | Default configuration of a slider
config :: Config msg
config =
  Config
    { discrete = False,
      displayMarkers = False,
      min = Nothing,
      max = Nothing,
      step = Nothing,
      value = Nothing,
      disabled = False,
      additionalAttributes = [],
      onInput = Nothing
    }

-- | Specify whether a slider is _discrete_
-- Discrete sliders feature a pin that indicates the current value while
-- interacting with the slider.
-- This works best for integer-valued sliders, but this is not a requirement.
setDiscrete :: Bool -> Config msg -> Config msg
setDiscrete discrete config_ =
  config_ {discrete = discrete}

-- | Specify whether a slider should display markers
-- Note that this option is ignored by non-discrete sliders.
setDisplayMarkers :: Bool -> Config msg -> Config msg
setDisplayMarkers displayMarkers config_ =
  config_ {displayMarkers = displayMarkers}

-- | Specify a slider's minimum value
setMin :: Maybe Float -> Config msg -> Config msg
setMin min config_ =
  config_ {min = min}

-- | Specify a slider's maximum value
setMax :: Maybe Float -> Config msg -> Config msg
setMax max config_ =
  config_ {max = max}

-- | Specify a slider's step value
setStep :: Maybe Float -> Config msg -> Config msg
setStep step config_ =
  config_ {step = step}

-- | Specify a slider's value
setValue :: Maybe Float -> Config msg -> Config msg
setValue value config_ =
  config_ {value = value}

-- | Specify whether a slider is disabled
-- Disabled sliders canot be interacted with and have no visual interaction
-- effect.
setDisabled :: Bool -> Config msg -> Config msg
setDisabled disabled config_ =
  config_ {disabled = disabled}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user interacts with the slider
setOnInput :: (Float -> msg) -> Config msg -> Config msg
setOnInput onInput config_ =
  config_ {onInput = Just onInput}

-- | Slider view function
slider :: Config msg -> Miso.View msg
slider (config_@Config {additionalAttributes = additionalAttributes}) =
  Miso.nodeHtml
    "mdc-slider"
    ( Maybe.mapMaybe
        id
        [ rootCs,
          displayCss,
          discreteCs config_,
          displayMarkersCs config_,
          tabIndexProp,
          sliderRoleAttr,
          valueProp config_,
          minProp config_,
          maxProp config_,
          stepProp config_,
          disabledProp config_,
          ariaValueMinAttr config_,
          ariaValueMaxAttr config_,
          ariaValuenowAttr config_,
          changeHandler config_
        ]
        ++ additionalAttributes
    )
    [ trackContainerElt,
      thumbContainerElt config_
    ]

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-slider")

displayCss :: Maybe (Miso.Attribute msg)
displayCss =
  Just (Miso.style_ <| Map.singleton "display" "block")

discreteCs :: Config msg -> Maybe (Miso.Attribute msg)
discreteCs (Config {discrete = discrete}) =
  if discrete
    then Just (Miso.class_ "mdc-slider--discrete")
    else Nothing

displayMarkersCs :: Config msg -> Maybe (Miso.Attribute msg)
displayMarkersCs (Config {discrete = discrete, displayMarkers = displayMarkers}) =
  if discrete && displayMarkers
    then Just (Miso.class_ "mdc-slider--display-markers")
    else Nothing

tabIndexProp :: Maybe (Miso.Attribute msg)
tabIndexProp =
  Just (Miso.intProp "tabindex" 0)

sliderRoleAttr :: Maybe (Miso.Attribute msg)
sliderRoleAttr =
  Just (Miso.textProp "role" "slider")

valueProp :: Config msg -> Maybe (Miso.Attribute msg)
valueProp (Config {value = value}) =
  fmap (Miso.textProp "value" << Miso.String.toMisoString << show) value

minProp :: Config msg -> Maybe (Miso.Attribute msg)
minProp (Config {min = min}) =
  fmap (Miso.textProp "min" << Miso.String.toMisoString << show) min

maxProp :: Config msg -> Maybe (Miso.Attribute msg)
maxProp (Config {max = max}) =
  fmap (Miso.textProp "max" << Miso.String.toMisoString << show) max

stepProp :: Config msg -> Maybe (Miso.Attribute msg)
stepProp (Config {step = step}) =
  fmap (Miso.textProp "step" << Miso.String.toMisoString << show) step

disabledProp :: Config msg -> Maybe (Miso.Attribute msg)
disabledProp (Config {disabled = disabled}) =
  Just (Miso.boolProp "disabled" disabled)

ariaValueMinAttr :: Config msg -> Maybe (Miso.Attribute msg)
ariaValueMinAttr (Config {min = min}) =
  fmap (Miso.textProp "aria-valuemin" << Miso.String.toMisoString << show) min

ariaValueMaxAttr :: Config msg -> Maybe (Miso.Attribute msg)
ariaValueMaxAttr (Config {max = max}) =
  fmap (Miso.textProp "aria-valuemax" << Miso.String.toMisoString << show) max

ariaValuenowAttr :: Config msg -> Maybe (Miso.Attribute msg)
ariaValuenowAttr (Config {value = value}) =
  fmap (Miso.textProp "aria-valuenow" << Miso.String.toMisoString << show) value

-- | Retrieves "value" field in `Decoder`
valueDecoder :: Miso.Decoder Float
valueDecoder =
  Miso.Decoder
    { Miso.decodeAt = Miso.DecodeTarget ["target"],
      Miso.decoder = withObject "target" $ \o -> o .: "value"
    }

changeHandler :: Config msg -> Maybe (Miso.Attribute msg)
changeHandler (Config {onInput = onInput}) =
  fmap (\x -> Miso.on "MDCSlider:input" valueDecoder (x)) (onInput)

trackContainerElt :: Miso.View msg
trackContainerElt =
  Miso.div_ [Miso.class_ "mdc-slider__track-container"] [trackElt, trackMarkerContainerElt]

trackElt :: Miso.View msg
trackElt =
  Miso.div_ [Miso.class_ "mdc-slider__track"] []

trackMarkerContainerElt :: Miso.View msg
trackMarkerContainerElt =
  Miso.div_ [Miso.class_ "mdc-slider__track-marker-container"] []

thumbContainerElt :: Config msg -> Miso.View msg
thumbContainerElt (Config {discrete = discrete}) =
  Miso.div_
    [Miso.class_ "mdc-slider__thumb-container"]
    ( if discrete
        then [pinElt, thumbElt, focusRingElt]
        else [thumbElt, focusRingElt]
    )

pinElt :: Miso.View msg
pinElt =
  Miso.div_ [Miso.class_ "mdc-slider__pin"] [pinValueMarkerElt]

pinValueMarkerElt :: Miso.View msg
pinValueMarkerElt =
  Miso.div_ [Miso.class_ "mdc-slider__pin-value-marker"] []

thumbElt :: Miso.View msg
thumbElt =
  Miso.Svg.svg_
    [ Miso.Svg.class_' "mdc-slider__thumb",
      Miso.Svg.width_ "21",
      Miso.Svg.height_ "21"
    ]
    [ Miso.Svg.circle_
        [ Miso.Svg.cx_ "10.5",
          Miso.Svg.cy_ "10.5",
          Miso.Svg.r_ "7.875"
        ]
        []
    ]

focusRingElt :: Miso.View msg
focusRingElt =
  Miso.div_ [Miso.class_ "mdc-slider__focus-ring"] []
