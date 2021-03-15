{-# LANGUAGE OverloadedStrings #-}

module Material.Ripple
  ( Config,
    config,
    setColor,
    setAttributes,
    bounded,
    unbounded,
    Color,
    primary,
    accent,
  )
where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Miso

-- | Ripple configuration
data Config msg = Config
  { color :: Maybe Color,
    additionalAttributes :: [Miso.Attribute msg]
  }

-- | Default ripple configuration
config :: Config msg
config =
  Config
    { color = Nothing,
      additionalAttributes = []
    }

-- | Specify a ripple effect's color
setColor :: Maybe Color -> Config msg -> Config msg
setColor color config_ =
  config_ {color = color}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Ripple effect's color
data Color
  = Primary
  | Accent

-- | Primary variant of a ripple effect's color
primary :: Color
primary =
  Primary

-- | Accent variant of a ripple effect's color
accent :: Color
accent =
  Accent

ripple :: Bool -> Config msg -> Miso.View msg
ripple isUnbounded (config_@Config {additionalAttributes = additionalAttributes}) =
  Miso.nodeHtml
    "mdc-ripple"
    ( Maybe.mapMaybe
        id
        [ unboundedProp isUnbounded,
          unboundedData isUnbounded,
          colorCs config_,
          rippleSurface,
          Just (Miso.style_ $ Map.singleton "position" "absolute"),
          Just (Miso.style_ $ Map.singleton "top" "0"),
          Just (Miso.style_ $ Map.singleton "left" "0"),
          Just (Miso.style_ $ Map.singleton "right" "0"),
          Just (Miso.style_ $ Map.singleton "bottom" "0")
        ]
        ++ additionalAttributes
    )
    []

-- | Bounded ripple view function
bounded :: Config msg -> Miso.View msg
bounded =
  ripple False

-- | Unbounded ripple view function
unbounded :: Config msg -> Miso.View msg
unbounded =
  ripple True

rippleSurface :: Maybe (Miso.Attribute msg)
rippleSurface =
  Just (Miso.class_ "mdc-ripple-surface")

colorCs :: Config msg -> Maybe (Miso.Attribute msg)
colorCs (Config {color = color}) =
  case color of
    Just Primary ->
      Just (Miso.class_ "mdc-ripple-surface--primary")
    Just Accent ->
      Just (Miso.class_ "mdc-ripple-surface--accent")
    Nothing ->
      Nothing

unboundedProp :: Bool -> Maybe (Miso.Attribute msg)
unboundedProp isUnbounded =
  Just (Miso.boolProp "unbounded" isUnbounded)

unboundedData :: Bool -> Maybe (Miso.Attribute msg)
unboundedData isUnbounded =
  if isUnbounded
    then Just (Miso.textProp "data-mdc-ripple-is-unbounded" "")
    else Nothing
