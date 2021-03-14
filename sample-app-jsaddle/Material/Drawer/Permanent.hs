{-# LANGUAGE OverloadedStrings #-}

module Material.Drawer.Permanent
  ( Config,
    config,
    setAttributes,
    drawer,
    content,
    header,
    title,
    subtitle,
  )
where

import Data.Maybe
import Miso

-- | Configuration of a permanent drawer
data Config msg = Config {additionalAttributes :: [Attribute msg]}

-- | Default configuration of a permanent drawer
config :: Config msg
config =
  Config {additionalAttributes = []}

-- | Specify additional attributes
setAttributes :: [Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Permanent drawer view function
drawer :: Config msg -> [View msg] -> View msg
drawer Config {additionalAttributes = additionalAttributes} nodes =
  div_
    (mapMaybe id [rootCs] ++ additionalAttributes)
    nodes

-- | Drawer content
content :: [Attribute msg] -> [View msg] -> View msg
content attributes nodes =
  div_ (class_ "mdc-drawer__content" : attributes) nodes

-- | Drawer header view function
--    PermanentDrawer.drawer PermanentDrawer.config
--        [ PermanentDrawer.header []
--            [ Html.h3 [ PermanentDrawer.title ]
--                [ text "Title" ]
--            , Html.h6 [ PermanentDrawer.subtitle ]
--                [ text "Subtitle" ]
--            ]
--        , PermanentDrawer.content [] []
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
