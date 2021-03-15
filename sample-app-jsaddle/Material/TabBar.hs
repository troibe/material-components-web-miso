{-# LANGUAGE OverloadedStrings #-}

module Material.TabBar
  ( Config,
    config,
    setStacked,
    setMinWidth,
    setIndicatorSpansContent,
    setAlign,
    setAttributes,
    tabBar,
    Align (..),
  )
where

import qualified Data.Function
import qualified Data.Maybe as Maybe
import qualified Data.Void as Void
import qualified Material.Tab.Internal as Tab
import qualified Miso
import qualified Miso.String

(|>) = (Data.Function.&)

(<|) = (Data.Function.$)

(<<) = (Data.Function..)

-- | Configuration of a tab bar
data Config msg = Config
  { stacked :: Bool,
    minWidth :: Bool,
    indicatorSpansContent :: Bool,
    additionalAttributes :: [Miso.Attribute msg],
    align :: Maybe Align
  }

-- | Default configuration of a tab bar
config :: Config msg
config =
  Config
    { stacked = False,
      minWidth = False,
      indicatorSpansContent = False,
      align = Nothing,
      additionalAttributes = []
    }

-- | Specify a tab bar's tabs to be stacked
-- Stacked tabs display their icon below the their label.
setStacked :: Bool -> Config msg -> Config msg
setStacked stacked config_ =
  config_ {stacked = stacked}

-- | Specify whether a tab bar's tabs should be of minimum width
-- Usually, a tab bar's tabs have a minimum with. Using this option, tabs are as
-- narrow as possible.
setMinWidth :: Bool -> Config msg -> Config msg
setMinWidth minWidth config_ =
  config_ {minWidth = minWidth}

-- | Specify whether a tab bar's tab indicator spans its content
-- Usually, a tab bar's tab indicator spans the entire tab. Use this option to
-- make it span only it's label instead.
setIndicatorSpansContent :: Bool -> Config msg -> Config msg
setIndicatorSpansContent indicatorSpansContent config_ =
  config_ {indicatorSpansContent = indicatorSpansContent}

-- | Specify tab bar's alignment of tabs in case they overflow horizontally
setAlign :: Maybe Align -> Config msg -> Config msg
setAlign align config_ =
  config_ {align = align}

-- | Specify additional attribtues
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Tab bar view function
tabBar :: Config msg -> [Tab.Tab msg] -> Miso.View msg
tabBar config_@Config {additionalAttributes = additionalAttributes, align = align} tabs =
  Miso.nodeHtml
    "mdc-tab-bar"
    ( Maybe.mapMaybe
        id
        [ rootCs,
          tablistRoleAttr,
          activeTabIndexProp tabs
        ]
        ++ additionalAttributes
    )
    [tabScroller config_ align tabs]

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-tab-bar")

tablistRoleAttr :: Maybe (Miso.Attribute msg)
tablistRoleAttr =
  Just (Miso.textProp "role" "tablist")

indexedMap :: (Int -> a -> b) -> [a] -> [b]
indexedMap f xs =
  let indexedMapHelper _ [] _ = []
      indexedMapHelper f (x : xs) i = f i x : indexedMapHelper f xs (i + 1)
   in indexedMapHelper f xs 0

pair :: a -> b -> (a, b)
pair a b = (a, b)

activeTabIndexProp :: [Tab.Tab msg] -> Maybe (Miso.Attribute msg)
activeTabIndexProp tabs =
  let activeTabIndex =
        indexedMap pair tabs
          |> filter (\(_, Tab.Tab (Tab.Config {Tab.active = active})) -> active)
          |> Maybe.listToMaybe
          |> fmap fst
   in fmap (Miso.intProp "activeTabIndex") activeTabIndex

viewTab :: Config msg -> Tab.Tab msg -> Miso.View msg
viewTab barConfig@Config {indicatorSpansContent = indicatorSpansContent} tab@(Tab.Tab tabConfig@Tab.Config {Tab.additionalAttributes = additionalAttributes, Tab.content = content}) =
  Miso.button_
    ( Maybe.mapMaybe
        id
        [ tabCs,
          tabRoleAttr,
          tabStackedCs barConfig,
          tabMinWidthCs barConfig,
          tabClickHandler tabConfig
        ]
        ++ additionalAttributes
    )
    ( Maybe.mapMaybe id
        <| if indicatorSpansContent
          then
            [ tabContentElt barConfig tabConfig content,
              tabRippleElt
            ]
          else
            [ tabContentElt barConfig tabConfig content,
              tabIndicatorElt tabConfig,
              tabRippleElt
            ]
    )

tabCs :: Maybe (Miso.Attribute msg)
tabCs =
  Just (Miso.class_ "mdc-tab")

tabStackedCs :: Config msg -> Maybe (Miso.Attribute msg)
tabStackedCs (Config {stacked = stacked}) =
  if stacked
    then Just (Miso.class_ "mdc-tab--stacked")
    else Nothing

tabMinWidthCs :: Config msg -> Maybe (Miso.Attribute msg)
tabMinWidthCs (Config {minWidth = minWidth}) =
  if minWidth
    then Just (Miso.class_ "mdc-tab--min-width")
    else Nothing

tabRoleAttr :: Maybe (Miso.Attribute msg)
tabRoleAttr =
  Just (Miso.textProp "role" "tab")

tabClickHandler :: Tab.Config msg -> Maybe (Miso.Attribute msg)
tabClickHandler (Tab.Config {Tab.onClick = onClick}) =
  fmap (Miso.on "MDCTab:interacted" Miso.emptyDecoder << const) onClick

tabContentElt :: Config msg -> Tab.Config msg -> Tab.Content -> Maybe (Miso.View msg)
tabContentElt barConfig@Config {indicatorSpansContent = indicatorSpansContent} config_ content =
  Just
    ( Miso.div_
        [Miso.class_ "mdc-tab__content"]
        ( if indicatorSpansContent
            then
              Maybe.mapMaybe
                id
                [ tabIconElt content,
                  tabTextLabelElt content,
                  tabIndicatorElt config_
                ]
            else
              Maybe.mapMaybe
                id
                [ tabIconElt content,
                  tabTextLabelElt content
                ]
        )
    )

tabIconElt :: Tab.Content -> Maybe (Miso.View msg)
tabIconElt Tab.Content {Tab.icon = icon} =
  fmap (fmap Void.absurd)
    <| case icon of
      Just (Tab.Icon {Tab.node = node, Tab.attributes = attributes, Tab.nodes = nodes}) ->
        Just (node (Miso.class_ "mdc-tab__icon" : attributes) nodes)
      Just (Tab.SvgIcon {Tab.node = node, Tab.attributes = attributes, Tab.nodes = nodes}) ->
        Just (node (Miso.class_ "mdc-tab__icon" : attributes) nodes)
      Nothing ->
        Nothing

tabTextLabelElt :: Tab.Content -> Maybe (Miso.View msg)
tabTextLabelElt Tab.Content {Tab.label = label} =
  Just (Miso.span_ [Miso.class_ "mdc-tab__text-label"] [Miso.text (Miso.String.toMisoString label)])

tabIndicatorElt :: Tab.Config msg -> Maybe (Miso.View msg)
tabIndicatorElt config_ =
  Just (Miso.span_ [Miso.class_ "mdc-tab-indicator"] [tabIndicatorContentElt])

tabIndicatorContentElt :: Miso.View msg
tabIndicatorContentElt =
  Miso.span_
    [ Miso.class_ "mdc-tab-indicator__content",
      Miso.class_ "mdc-tab-indicator__content--underline"
    ]
    []

tabRippleElt :: Maybe (Miso.View msg)
tabRippleElt =
  Just (Miso.span_ [Miso.class_ "mdc-tab__ripple"] [])

-- | Alignment of a tab scroller
data Align
  = Start
  | End
  | Center

tabScroller :: Config msg -> Maybe Align -> [Tab.Tab msg] -> Miso.View msg
tabScroller config_ align tabs =
  Miso.div_
    ( Maybe.mapMaybe
        id
        [ tabScrollerCs,
          tabScrollerAlignCs align
        ]
    )
    [tabScrollerScrollAreaElt config_ tabs]

tabScrollerCs :: Maybe (Miso.Attribute msg)
tabScrollerCs =
  Just (Miso.class_ "mdc-tab-scroller")

tabScrollerAlignCs :: Maybe Align -> Maybe (Miso.Attribute msg)
tabScrollerAlignCs align =
  case align of
    Just Start ->
      Just (Miso.class_ "mdc-tab-scroller--align-start")
    Just End ->
      Just (Miso.class_ "mdc-tab-scroller--align-end")
    Just Center ->
      Just (Miso.class_ "mdc-tab-scroller--align-center")
    Nothing ->
      Nothing

tabScrollerScrollAreaElt :: Config msg -> [Tab.Tab msg] -> Miso.View msg
tabScrollerScrollAreaElt barConfig tabs =
  Miso.div_
    [Miso.class_ "mdc-tab-scroller__scroll-area"]
    [tabScrollerScrollContentElt barConfig tabs]

tabScrollerScrollContentElt :: Config msg -> [Tab.Tab msg] -> Miso.View msg
tabScrollerScrollContentElt barConfig tabs =
  Miso.div_
    [Miso.class_ "mdc-tab-scroller__scroll-content"]
    (Prelude.map (viewTab barConfig) tabs)
