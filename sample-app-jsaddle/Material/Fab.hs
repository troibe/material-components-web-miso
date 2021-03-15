{-# LANGUAGE OverloadedStrings #-}

module Material.Fab
  ( Config,
    config,
    setOnClick,
    setMini,
    setExited,
    setAttributes,
    fab,
  )
where

import qualified Data.Maybe as Maybe
import qualified Miso
import qualified Miso.String

-- | Floating action button configuration
data Config msg = Config
  { mini :: Bool,
    exited :: Bool,
    additionalAttributes :: [Miso.Attribute msg],
    onClick :: Maybe msg
  }

-- | Default floating action button configuration
config :: Config msg
config =
  Config
    { mini = False,
      exited = False,
      onClick = Nothing,
      additionalAttributes = []
    }

-- | Specify whether the floating actions button should be smaller than normally
setMini :: Bool -> Config msg -> Config msg
setMini mini config_ =
  config_ {mini = mini}

-- | Specify whether a floating action button should transition off the screen
setExited :: Bool -> Config msg -> Config msg
setExited exited config_ =
  config_ {exited = exited}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user clicks the floating action button
setOnClick :: msg -> Config msg -> Config msg
setOnClick onClick config_ =
  config_ {onClick = Just onClick}

-- | Floating action button view function
fab :: Config msg -> String -> Miso.View msg
fab (config_@Config {additionalAttributes = additionalAttributes}) iconName =
  Miso.nodeHtml
    "mdc-fab"
    ( Maybe.mapMaybe
        id
        [ rootCs,
          miniCs config_,
          exitedCs config_,
          clickHandler config_,
          tabIndexProp 0
        ]
        ++ additionalAttributes
    )
    [ rippleElt,
      iconElt iconName
    ]

tabIndexProp :: Int -> Maybe (Miso.Attribute msg)
tabIndexProp tabIndex =
  Just (Miso.intProp "tabIndex" tabIndex)

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-fab")

miniCs :: Config msg -> Maybe (Miso.Attribute msg)
miniCs (Config {mini = mini}) =
  if mini
    then Just (Miso.class_ "mdc-fab--mini")
    else Nothing

exitedCs :: Config msg -> Maybe (Miso.Attribute msg)
exitedCs (Config {exited = exited}) =
  if exited
    then Just (Miso.class_ "mdc-fab--exited")
    else Nothing

rippleElt :: Miso.View msg
rippleElt =
  Miso.div_ [Miso.class_ "mdc-fab__ripple"] []

iconElt :: String -> Miso.View msg
iconElt iconName =
  Miso.span_ [Miso.class_ "material-icons", Miso.class_ "mdc-fab__icon"] [Miso.text (Miso.String.toMisoString iconName)]

clickHandler :: Config msg -> Maybe (Miso.Attribute msg)
clickHandler (Config {onClick = onClick}) = case onClick of
  Nothing -> Nothing
  Just o -> Just (Miso.onClick o)
