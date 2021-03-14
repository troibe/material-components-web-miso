{-# LANGUAGE OverloadedStrings #-}

module Material.Menu
  ( Config,
    config,
    setOnClose,
    setOpen,
    setQuickOpen,
    setAttributes,
    menu,
    surfaceAnchor,
  )
where

import qualified Data.Maybe as Maybe
import qualified Miso

-- | Configuration of a menu
data Config msg = Config
  { open :: Bool,
    quickOpen :: Bool,
    additionalAttributes :: [Miso.Attribute msg],
    onClose :: Maybe msg
  }

-- | Default configuration of a menu
config :: Config msg
config =
  Config
    { open = False,
      quickOpen = False,
      additionalAttributes = [],
      onClose = Nothing
    }

-- | Specify whether a menu is open
setOpen :: Bool -> Config msg -> Config msg
setOpen open config_ =
  config_ {open = open}

-- | Specify whether a menu is _opening quickly_
-- A quickly opening menu opens without showing an animation.
setQuickOpen :: Bool -> Config msg -> Config msg
setQuickOpen quickOpen config_ =
  config_ {quickOpen = quickOpen}

-- | Specify a message when the user closes the menu
setOnClose :: msg -> Config msg -> Config msg
setOnClose onClose config_ =
  config_ {onClose = Just onClose}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Menu view function
menu :: Config msg -> [Miso.View msg] -> Miso.View msg
menu (config_@Config {additionalAttributes = additionalAttributes}) nodes =
  Miso.nodeHtml
    "mdc-menu"
    ( Maybe.mapMaybe
        id
        [ rootCs,
          openProp config_,
          quickOpenProp config_,
          closeHandler config_
        ]
        ++ additionalAttributes
    )
    nodes

-- | Menu surface anchor attribute
-- Use this on a HTML element that contains both the triggering element and the
-- menu itself.
surfaceAnchor :: Miso.Attribute msg
surfaceAnchor =
  Miso.class_ "mdc-menu-surface--anchor"

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-menu mdc-menu-surface")

openProp :: Config msg -> Maybe (Miso.Attribute msg)
openProp (Config {open = open}) =
  Just (Miso.boolProp "open" open)

quickOpenProp :: Config msg -> Maybe (Miso.Attribute msg)
quickOpenProp (Config {quickOpen = quickOpen}) =
  Just (Miso.boolProp "quickOpen" quickOpen)

closeHandler :: Config msg -> Maybe (Miso.Attribute msg)
closeHandler (Config {onClose = onClose}) =
  fmap (\x -> Miso.on "MDCMenuSurface:close" Miso.emptyDecoder (const x)) onClose
