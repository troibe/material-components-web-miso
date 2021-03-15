{-# LANGUAGE OverloadedStrings #-}

module Material.TopAppBar
  ( Config (..),
    config,
    setFixed,
    setDense,
    setAttributes,
    regular,
    row,
    section,
    alignEnd,
    alignStart,
    navigationIcon,
    title,
    actionItem,
    fixedAdjust,
    denseFixedAdjust,
    denseProminentFixedAdjust,
    prominentFixedAdjust,
    shortFixedAdjust,
    short,
    shortCollapsed,
    prominent,
  )
where

import Data.Maybe
import Miso

-- | Configuration of a top app bar
data Config msg = Config
  { dense :: Bool,
    fixed :: Bool,
    additionalAttributes :: [Attribute msg]
  }

data Variant
  = Regular
  | Short
  | ShortCollapsed
  | Prominent

-- | Default configuration of a top app bar
config :: Config msg
config =
  Config
    { dense = False,
      fixed = False,
      additionalAttributes = []
    }

-- | Specify whether a top app bar is dense
-- A dense top app bar is more compact, featuring smaller than usual margins.
setDense :: Bool -> Config msg -> Config msg
setDense dense config_ =
  config_ {dense = dense}

-- | Specify whether a top app bar is fixed
-- A fixed top app bar does not scroll away when the user is scrolling the page.
setFixed :: Bool -> Config msg -> Config msg
setFixed fixed config_ =
  config_ {fixed = fixed}

-- | Specify additional attributes
setAttributes :: [Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

genericTopAppBar :: Variant -> Config msg -> [View msg] -> View msg
genericTopAppBar variant (config_@Config {additionalAttributes = additionalAttributes}) nodes =
  nodeHtml
    "mdc-top-app-bar"
    ( mapMaybe
        id
        [ rootCs,
          variantCs variant,
          denseCs config_,
          fixedCs config_
        ]
        ++ additionalAttributes
    )
    nodes

-- | Regular top app bar view function
regular :: Config msg -> [View msg] -> View msg
regular config_ nodes =
  genericTopAppBar Regular config_ nodes

-- | Short top app bar view function
short :: Config msg -> [View msg] -> View msg
short config_ nodes =
  genericTopAppBar Short config_ nodes

-- | Short always closed top app bar view function
shortCollapsed :: Config msg -> [View msg] -> View msg
shortCollapsed config_ nodes =
  genericTopAppBar ShortCollapsed config_ nodes

-- | Prominent top app bar view function
prominent :: Config msg -> [View msg] -> View msg
prominent config_ nodes =
  genericTopAppBar Prominent config_ nodes

-- | A row is the first child of a top app bar. It contains the top app bar's
-- `section`s.
row :: [Attribute msg] -> [View msg] -> View msg
row attributes nodes =
  section_ ([class_ "mdc-top-app-bar__row"] ++ attributes) nodes

-- | Sections subdivide the top app bar's rows. A section may be start- or
-- end-aligned. Usually, the first section is start-aligned and contains the top
-- app bar's navigation icon and title.
section :: [Attribute msg] -> [View msg] -> View msg
section attributes nodes =
  section_ ([class_ "mdc-top-app-bar__section"] ++ attributes) nodes

-- | Start-align a top app bar's `section`
alignStart :: Attribute msg
alignStart =
  class_ "mdc-top-app-bar__section--align-start"

-- | End-align a top app bar's `section`
alignEnd :: Attribute msg
alignEnd =
  class_ "mdc-top-app-bar__section--align-end"

-- | Apply this attribute to an icon button to mark it as a top app bar's
-- navigation icon
navigationIcon :: Attribute msg
navigationIcon =
  class_ "mdc-top-app-bar__navigation-icon"

-- | Apply this attribute to a element to mark it as the top app bar's title
title :: Attribute msg
title =
  class_ "mdc-top-app-bar__title"

-- | Apply this attribute to a icon button to mark it as a top app bar's action
-- item
actionItem :: Attribute msg
actionItem =
  class_ "mdc-top-app-bar__action-item"

rootCs :: Maybe (Attribute msg)
rootCs =
  Just (class_ "mdc-top-app-bar")

variantCs :: Variant -> Maybe (Attribute msg)
variantCs variant =
  case variant of
    Regular ->
      Nothing
    Short ->
      Just (class_ "mdc-top-app-bar--short")
    ShortCollapsed ->
      Just (class_ "mdc-top-app-bar--short mdc-top-app-bar--short-collapsed")
    Prominent ->
      Just (class_ "mdc-top-app-bar--prominent")

denseCs :: Config msg -> Maybe (Attribute msg)
denseCs (Config {dense = dense}) =
  if dense
    then Just (class_ "mdc-top-app-bar--dense")
    else Nothing

fixedCs :: Config msg -> Maybe (Attribute msg)
fixedCs (Config {fixed = fixed}) =
  if fixed
    then Just (class_ "mdc-top-app-bar--fixed")
    else Nothing

-- | Appropriate padding for a fixed top app bar.
-- Apply this to the page's content so that a fixed top app bar does not overlay
-- the content.
fixedAdjust :: Attribute msg
fixedAdjust =
  class_ "mdc-top-app-bar--fixed-adjust"

-- | Appropriate padding for a dense fixed top app bar.
-- Apply this to the page's content so that a fixed top app bar does not overlay
-- the content.
denseFixedAdjust :: Attribute msg
denseFixedAdjust =
  class_ "mdc-top-app-bar--dense-fixed-adjust"

-- | Appropriate padding for a short fixed top app bar.
-- Apply this to the page's content so that a fixed top app bar does not overlay
-- the content.
shortFixedAdjust :: Attribute msg
shortFixedAdjust =
  class_ "mdc-top-app-bar--short-fixed-adjust"

-- | Appropriate padding for a prominent fixed top app bar.
-- Apply this to the page's content so that a fixed top app bar does not overlay
-- the content.
prominentFixedAdjust :: Attribute msg
prominentFixedAdjust =
  class_ "mdc-top-app-bar--prominent-fixed-adjust"

-- | Appropriate padding for a dense prominent fixed top app bar.
-- Apply this to the page's content so that a fixed top app bar does not overlay
-- the content.
denseProminentFixedAdjust :: Attribute msg
denseProminentFixedAdjust =
  class_ "mdc-top-app-bar--dense-prominent-fixed-adjust"
