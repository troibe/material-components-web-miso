{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Material.Select
  ( Config,
    config,
    setOnChange,
    setLabel,
    setSelected,
    setDisabled,
    setRequired,
    setValid,
    setLeadingIcon,
    setAttributes,
    filled,
    outlined,
  )
where

import qualified Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Elm.List
import qualified Material.List as List
import qualified Material.List.Item as ListItem
import qualified Material.Menu as Menu
import qualified Material.Select.Icon as SelectIcon
import qualified Material.Select.Icon.Internal as SelectIcon
import qualified Material.Select.Item (SelectItem)
import qualified Material.Select.Item.Internal as SelectItem
import qualified Miso
import qualified Miso.Html.Event
import Miso.String

(|>) = (Data.Function.&)

-- | Configuration of a select
data Config a msg = Config
  { label :: Maybe String,
    disabled :: Bool,
    required :: Bool,
    valid :: Bool,
    selected :: Maybe a,
    leadingIcon :: Maybe (SelectIcon.Icon msg),
    additionalAttributes :: [Miso.Attribute msg],
    onChange :: Maybe (a -> msg)
  }

-- | Default configuration of a select
config :: Config a msg
config =
  Config
    { label = Nothing,
      disabled = False,
      required = False,
      valid = True,
      selected = Nothing,
      leadingIcon = Nothing,
      additionalAttributes = [],
      onChange = Nothing
    }

-- | Specify a select's label
setLabel :: Maybe String -> Config a msg -> Config a msg
setLabel label config_ =
  config_ {label = label}

-- | Specify a select's selected value
setSelected :: Maybe a -> Config a msg -> Config a msg
setSelected selected config_ =
  config_ {selected = selected}

-- | Specify whether a select is disabled
--
-- Disabled selects cannot be interacted with an have no visual interaction
-- effect.
setDisabled :: Bool -> Config a msg -> Config a msg
setDisabled disabled config_ =
  config_ {disabled = disabled}

-- | Specify whether a select is required
setRequired :: Bool -> Config a msg -> Config a msg
setRequired required config_ =
  config_ {required = required}

-- | Specify whether a select is valid
setValid :: Bool -> Config a msg -> Config a msg
setValid valid config_ =
  config_ {valid = valid}

-- | Specify a select's leading icon
setLeadingIcon :: Maybe (SelectIcon.Icon msg) -> Config a msg -> Config a msg
setLeadingIcon leadingIcon config_ =
  config_ {leadingIcon = leadingIcon}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config a msg -> Config a msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user changes the select
setOnChange :: (a -> msg) -> Config a msg -> Config a msg
setOnChange onChange config_ =
  config_ {onChange = Just onChange}

data Variant
  = Filled
  | Outlined
  deriving (Eq)

select :: Eq a => Variant -> Config a msg -> SelectItem.SelectItem a msg -> [SelectItem.SelectItem a msg] -> Miso.View msg
select variant (config_@Config {leadingIcon, selected, additionalAttributes, onChange}) firstSelectItem remainingSelectItems =
  let selectedIndex =
        indexedMap
          ( \index (SelectItem.SelectItem (SelectItem.Config {SelectItem.value}) _) ->
              if Just value == selected
                then Just index
                else Nothing
          )
          (firstSelectItem : remainingSelectItems)
          |> List.filter Maybe.isJust
          |> List.head
   in Miso.nodeHtml
        "mdc-select"
        ( Maybe.mapMaybe
            id
            [ rootCs,
              outlinedCs variant,
              leadingIconCs config_,
              disabledProp config_,
              selectedIndexProp selectedIndex,
              validProp config_,
              requiredProp config_
            ]
            ++ additionalAttributes
        )
        [ anchorElt
            []
            ( Prelude.concat
                [ [ rippleElt,
                    leadingIconElt config_,
                    selectedTextElt,
                    dropdownIconElt
                  ],
                  if variant == Outlined
                    then [notchedOutlineElt config_]
                    else
                      [ floatingLabelElt config_,
                        lineRippleElt
                      ]
                ]
            ),
          menuElt leadingIcon selected onChange firstSelectItem remainingSelectItems
        ]

-- | Filled select view function
filled :: Eq a => Config a msg -> SelectItem.SelectItem a msg -> [SelectItem.SelectItem a msg] -> Miso.View msg
filled config_ firstSelectItem remainingSelectItems =
  select Filled config_ firstSelectItem remainingSelectItems

-- | Outlined select view function
outlined :: Eq a => Config a msg -> SelectItem.SelectItem a msg -> [SelectItem.SelectItem a msg] -> Miso.View msg
outlined config_ firstSelectItem remainingSelectItems =
  select Outlined config_ firstSelectItem remainingSelectItems

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-select")

outlinedCs :: Variant -> Maybe (Miso.Attribute msg)
outlinedCs variant =
  if variant == Outlined
    then Just (Miso.class_ "mdc-select--outlined")
    else Nothing

leadingIconCs :: Config a msg -> Maybe (Miso.Attribute msg)
leadingIconCs (Config {leadingIcon}) =
  Maybe.maybe Nothing (\_ -> Just $ Miso.class_ "mdc-select--with-leading-icon") leadingIcon

disabledProp :: Config a msg -> Maybe (Miso.Attribute msg)
disabledProp (Config {disabled}) =
  Just (Miso.boolProp "disabled" disabled)

validProp :: Config a msg -> Maybe (Miso.Attribute msg)
validProp (Config {valid}) =
  Just (Miso.boolProp "valid" valid)

selectedIndexProp :: Maybe Int -> Maybe (Miso.Attribute msg)
selectedIndexProp selectedIndex =
  Just
    ( Miso.intProp
        "selectedIndex"
        (Maybe.fromMaybe (negate 1) selectedIndex)
    )

requiredProp :: Config a msg -> Maybe (Miso.Attribute msg)
requiredProp (Config {required}) =
  Just (Miso.boolProp "required" required)

rippleElt :: Miso.View msg
rippleElt =
  Miso.span_ [Miso.class_ "mdc-text-field__ripple"] []

anchorElt :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg
anchorElt additionalAttributes nodes =
  Miso.div_ (Miso.class_ "mdc-select__anchor" : additionalAttributes) nodes

leadingIconElt :: Config a msg -> Miso.View msg
leadingIconElt (Config {leadingIcon}) =
  case leadingIcon of
    Nothing ->
      Miso.text ""
    Just (SelectIcon.Icon {SelectIcon.node, SelectIcon.attributes, SelectIcon.nodes, SelectIcon.onInteraction, SelectIcon.disabled}) ->
      node
        ( Miso.class_ "mdc-select__icon" :
          ( case onInteraction of
              Just msg ->
                if not disabled
                  then
                    Miso.intProp "tabindex" 0 :
                    Miso.stringProp "role" "button" :
                    Miso.Html.Event.onClick msg
                    {-- : Miso.Html.Event.onKeyDown
                        (Miso.Event.Types.KeyCode
                            |> Decode.andThen
                                (\keyCode ->
                                    if keyCode == 13 then
                                        Decode.succeed msg

                                    else
                                        Decode.fail ""
                                )
                        ) --}
                    :
                    attributes
                  else
                    Miso.stringProp "tabindex" "-1" :
                    Miso.stringProp "role" "button" :
                    attributes
              Nothing ->
                attributes
          )
        )
        nodes

{-- Just (SelectIcon.SvgIcon { SelectIcon.node, SelectIcon.attributes, SelectIcon.nodes, SelectIcon.onInteraction, SelectIcon.disabled }) ->
    node
        (Miso.Svg.Attributes.class_ "mdc-select__icon"
            : (case onInteraction of
                    Just msg ->
                        if not disabled then
                            Miso.Attributes.tabindex 0
                                : Miso.Attributes.attribute "role" "button"
                                : Miso.Events.onClick msg
                                : Miso.Events.on "keydown"
                                    (Miso.Events.keyCode
                                        |> Decode.andThen
                                            (\keyCode ->
                                                if keyCode == 13 then
                                                    Decode.succeed msg
                                                else
                                                    Decode.fail ""
                                            )
                                    )
                                : attributes

                        else
                            Miso.Attributes.tabindex -1
                                : Miso.Attributes.attribute "role" "button"
                                : attributes

                    Nothing ->
                        attributes
               )
        )
        nodes --}

dropdownIconElt :: Miso.View msg
dropdownIconElt =
  Miso.i_ [Miso.class_ "mdc-select__dropdown-icon"] []

floatingLabelElt :: Config a msg -> Miso.View msg
floatingLabelElt (Config {label}) =
  Miso.div_ [Miso.class_ "mdc-floating-label"] [Miso.text $ toMisoString $ Maybe.fromMaybe "" label]

lineRippleElt :: Miso.View msg
lineRippleElt =
  Miso.label_ [Miso.class_ "mdc-line-ripple"] []

notchedOutlineElt :: Config a msg -> Miso.View msg
notchedOutlineElt (Config {label}) =
  Miso.span_
    [Miso.class_ "mdc-notched-outline"]
    [ Miso.span_ [Miso.class_ "mdc-notched-outline__leading"] [],
      Miso.span_
        [Miso.class_ "mdc-notched-outline__notch"]
        [ Miso.label_
            [Miso.class_ "mdc-floating-label"]
            [Miso.text $ toMisoString (Maybe.fromMaybe "" label)]
        ],
      Miso.span_ [Miso.class_ "mdc-notched-outline__trailing"] []
    ]

menuElt :: Maybe (SelectIcon.Icon msg) -> Maybe a -> Maybe (a -> msg) -> SelectItem.SelectItem a msg -> [SelectItem.SelectItem a msg] -> Miso.View msg
menuElt leadingIcon selected onChange firstSelectItem remainingSelectItems =
  Menu.menu
    ( Menu.config
        |> Menu.setAttributes
          [ Miso.class_ "mdc-select__menu",
            Miso.style_ $ Map.singleton "width" "100%"
          ]
    )
    [ List.list
        (List.config |> List.setWrapFocus True)
        (listItem leadingIcon selected onChange firstSelectItem)
        (Prelude.map (listItem leadingIcon selected onChange) remainingSelectItems)
    ]

listItem :: Maybe (SelectIcon.Icon msg) -> Maybe a -> Maybe (a -> msg) -> SelectItem.SelectItem a msg -> ListItem.ListItem msg
listItem leadingIcon selected onChange (SelectItem.SelectItem config_ nodes) =
  ListItem.listItem
    (listItemConfig selected onChange config_)
    ( if Maybe.isJust leadingIcon
        then ListItem.graphic [] [] : nodes
        else nodes
    )

listItemConfig :: Maybe a -> Maybe (a -> msg) -> SelectItem.Config a msg -> ListItem.Config msg
listItemConfig selectedValue onChange (SelectItem.Config {SelectItem.value, SelectItem.disabled, SelectItem.additionalAttributes}) =
  ListItem.config
    |> ListItem.setDisabled disabled
    |> ListItem.setAttributes additionalAttributes
    |> ( case onChange of
           Just onChange_ ->
             ListItem.setOnClick (onChange_ value)
           Nothing ->
             id
       )

selectedTextElt :: Miso.View msg
selectedTextElt =
  Miso.input_
    [ Miso.class_ "mdc-select__selected-text",
      Miso.boolProp "disabled" True,
      Miso.boolProp "readonly" True
    ]
