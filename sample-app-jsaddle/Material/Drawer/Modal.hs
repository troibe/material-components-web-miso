{-# LANGUAGE OverloadedStrings #-}

module Material.Drawer.Modal
  ( Config,
    config,
    setOnClose,
    setOpen,
    setAttributes,
    drawer,
    content,
    scrim,
    header,
    title,
    subtitle,
  )
where

import Data.Maybe
import Miso

-- | Configuration of a model drawer
data Config msg = Config
  { open :: Bool,
    additionalAttributes :: [Attribute msg],
    onClose :: Maybe msg
  }

-- | Default configuration of a modal drawer
config :: Config msg
config =
  Config
    { open = False,
      additionalAttributes = [],
      onClose = Nothing
    }

-- | Specify whether the drawer is open
setOpen :: Bool -> Config msg -> Config msg
setOpen open config_ =
  config_ {open = open}

-- | Specify message when the user closes the drawer
setOnClose :: msg -> Config msg -> Config msg
setOnClose onClose config_ =
  config_ {onClose = Just onClose}

-- | Specify additional attributes
setAttributes :: [Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Modal drawer view function
drawer :: Config msg -> [View msg] -> View msg
drawer config_@Config {additionalAttributes = additionalAttributes} nodes =
  nodeHtml
    "mdc-drawer"
    ( mapMaybe
        id
        [ rootCs,
          modalCs,
          openProp config_,
          closeHandler config_
        ]
        ++ additionalAttributes
    )
    nodes

-- | Drawer content
content :: [Attribute msg] -> [View msg] -> View msg
content attributes nodes =
  div_ (class_ "mdc-drawer__content" : attributes) nodes

-- | Drawer header view function
--    ModalDrawer.drawer ModalDrawer.config
--        [ ModalDrawer.header []
--            [ Html.h3 [ ModalDrawer.title ] [ text "Title" ]
--            , Html.h6 [ ModalDrawer.subtitle ]
--                [ text "Subtitle" ]
--            ]
--        , ModalDrawer.content [] []
--        ]
header :: [Attribute msg] -> [View msg] -> View msg
header additionalAttributes nodes =
  div_ (class_ "mdc-drawer__header" : additionalAttributes) nodes

-- | Attribute to mark the title text element of the drawer header
title :: Attribute msg
title =
  class_ "mdc-drawer__title"

-- | Attribute to mark the subtitle text element of the drawer header
subtitle :: Attribute msg
subtitle =
  class_ "mdc-drawer__subtitle"

rootCs :: Maybe (Attribute msg)
rootCs =
  Just (class_ "mdc-drawer")

modalCs :: Maybe (Attribute msg)
modalCs =
  Just (class_ "mdc-drawer--modal")

openProp :: Config msg -> Maybe (Attribute msg)
openProp Config {open = open} =
  Just (boolProp "open" open)

closeHandler :: Config msg -> Maybe (Attribute msg)
closeHandler Config {onClose = onClose} = case onClose of
  Nothing -> Nothing
  Just o -> Just (on "MDCDrawer:close" emptyDecoder (const o))

-- | Modal drawer's scrim element
-- Prevents the application from interaction while the modal drawer is open. Has
-- to be the next sibling after the `modalDrawer` and before the page's content.
scrim :: [Attribute msg] -> [View msg] -> View msg
scrim additionalAttributes nodes =
  div_ (class_ "mdc-drawer-scrim" : additionalAttributes) nodes
