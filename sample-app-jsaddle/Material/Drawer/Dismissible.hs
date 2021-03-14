{-# LANGUAGE OverloadedStrings #-}

module Material.Drawer.Dismissible
  ( Config,
    config,
    setOnClose,
    setOpen,
    setAttributes,
    drawer,
    content,
    appContent,
    header,
    title,
    subtitle,
  )
where

import Data.Maybe
import Miso

-- | Configuration of a dismissible drawer
data Config msg = Config
  { open :: Bool,
    additionalAttributes :: [Attribute msg],
    onClose :: Maybe msg
  }

-- | Default configuration of a dismissible drawer
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

-- | Dismissible drawer view function
drawer :: Config msg -> [View msg] -> View msg
drawer config_@Config {additionalAttributes = additionalAttributes} nodes =
  nodeHtml
    "mdc-drawer"
    ( mapMaybe
        id
        [ rootCs,
          dismissibleCs,
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
--    DismissibleDrawer.drawer DismissibleDrawer.config
--        [ DismissibleDrawer.header []
--            [ Html.h3 [ DismissibleDrawer.title ]
--                [ text "Title" ]
--            , Html.h6 [ DismissibleDrawer.subtitle ]
--                [ text "Subtitle" ]
--            ]
--        , DismissibleDrawer.content [] []
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

dismissibleCs :: Maybe (Attribute msg)
dismissibleCs =
  Just (class_ "mdc-drawer--dismissible")

openProp :: Config msg -> Maybe (Attribute msg)
openProp Config {open = open} =
  Just (boolProp "open" open)

closeHandler :: Config msg -> Maybe (Attribute msg)
closeHandler Config {onClose = onClose} = case onClose of
  Nothing -> Nothing
  Just o -> Just (on "MDCDrawer:close" emptyDecoder (const o))

-- | Dismissible drawer's app content marker
-- Apply this attribute to the page's content for the open/close animation to
-- work. The page content has to be the next sibling of the dismissible drawer.
appContent :: Attribute msg
appContent =
  class_ "mdc-drawer-app-content"
