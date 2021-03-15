{-# LANGUAGE OverloadedStrings #-}

module Material.LinearProgress
  ( Config (..),
    config,
    setReverse,
    setClosed,
    setAttributes,
    indeterminate,
    determinate,
    buffered,
  )
where

import qualified Data.Map as M
import Data.Maybe
import Miso

-- | Linear progress configuration
data Config msg = Config
  { reverse :: Bool,
    closed :: Bool,
    additionalAttributes :: [Attribute msg]
  }

data Variant
  = Indeterminate
  | Determinate Float
  | Buffered Float Float
  deriving (Eq)

-- | Default linear progress configuration
config :: Config msg
config =
  Config
    { Material.LinearProgress.reverse = False,
      closed = False,
      additionalAttributes = []
    }

-- | Specify whether a linear progress indicator should be hidden
setClosed :: Bool -> Config msg -> Config msg
setClosed closed config_ =
  config_ {closed = closed}

-- | Specify whether the direction of a linear progress indicator should be reversed
setReverse :: Bool -> Config msg -> Config msg
setReverse reverse config_ =
  config_ {Material.LinearProgress.reverse = reverse}

-- | Specify additional attributes
setAttributes :: [Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

linearProgress :: Variant -> Config msg -> View msg
linearProgress variant (config_@Config {additionalAttributes = additionalAttributes}) =
  nodeHtml
    "mdc-linear-progress"
    ( mapMaybe
        id
        [ rootCs,
          displayCss,
          roleAttr,
          variantCs variant,
          determinateProp variant,
          progressProp variant,
          bufferProp variant,
          reverseProp config_,
          closedProp config_
        ]
        ++ additionalAttributes
    )
    [ bufferElt,
      primaryBarElt,
      secondaryBarElt
    ]

-- | Indeterminate linear progress variant
indeterminate :: Config msg -> View msg
indeterminate config_ =
  linearProgress Indeterminate config_

-- | Determinate linear progress variant
determinate :: Config msg -> Float -> View msg
determinate config_ progress =
  linearProgress (Determinate progress) config_

-- | Buffered linear progress variant
buffered :: Config msg -> Float -> Float -> View msg
buffered config_ progress buffered =
  linearProgress (Buffered progress buffered) config_

rootCs :: Maybe (Attribute msg)
rootCs =
  Just (class_ "mdc-linear-progress")

displayCss :: Maybe (Attribute msg)
displayCss =
  Just (style_ $ M.singleton "display" "block")

roleAttr :: Maybe (Attribute msg)
roleAttr =
  Just (stringProp "role" "progressbar")

variantCs :: Variant -> Maybe (Attribute msg)
variantCs variant =
  case variant of
    Indeterminate ->
      Just (class_ "mdc-linear-progress--indeterminate")
    _ ->
      Nothing

determinateProp :: Variant -> Maybe (Attribute msg)
determinateProp variant =
  Just (boolProp "determinate" (variant /= Indeterminate))

progressProp :: Variant -> Maybe (Attribute msg)
progressProp variant =
  Just
    ( prop
        "progress"
        ( case variant of
            Determinate progress ->
              progress
            Buffered progress _ ->
              progress
            _ ->
              0
        )
    )

bufferProp :: Variant -> Maybe (Attribute msg)
bufferProp variant =
  Just
    ( prop
        "buffer"
        ( case variant of
            Buffered _ buffer ->
              buffer
            _ ->
              0
        )
    )

reverseProp :: Config msg -> Maybe (Attribute msg)
reverseProp Config {Material.LinearProgress.reverse = reverse} = case reverse of
  False -> Nothing
  True -> Just (boolProp "reverse" reverse)

closedProp :: Config msg -> Maybe (Attribute msg)
closedProp Config {closed = closed} =
  Just (boolProp "closed" closed)

bufferElt :: View msg
bufferElt =
  div_
    [class_ "mdc-linear-progress__buffer"]
    [ bufferBarElt,
      bufferDotsElt
    ]

bufferBarElt :: View msg
bufferBarElt =
  div_ [class_ "mdc-linear-progress__buffer-bar"] []

bufferDotsElt :: View msg
bufferDotsElt =
  div_ [class_ "mdc-linear-progress__buffer-dots"] []

primaryBarElt :: View msg
primaryBarElt =
  div_
    [class_ "mdc-linear-progress__bar mdc-linear-progress__primary-bar"]
    [barInnerElt]

secondaryBarElt :: View msg
secondaryBarElt =
  div_
    [class_ "mdc-linear-progress__bar mdc-linear-progress__secondary-bar"]
    [barInnerElt]

barInnerElt :: View msg
barInnerElt =
  div_ [class_ "mdc-linear-progress__bar-inner"] []
