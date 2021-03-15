{-# LANGUAGE OverloadedStrings #-}

module Material.ChipSet.Input (chipSet) where

import qualified Data.Function
import qualified Data.Maybe as Maybe
import qualified Data.Void as Void
import qualified Material.Chip.Input as Chip
import qualified Material.Chip.Input.Internal as Internal
import qualified Miso
import qualified Miso.String
import qualified Miso.Svg as Svg

(|>) = (Data.Function.&)

(<|) = (Data.Function.$)

(<<) = (Data.Function..)

-- | Input chip set view function
chipSet :: String -> [Miso.Attribute msg] -> (String, Chip.Chip msg) -> [(String, Chip.Chip msg)] -> Miso.View msg
chipSet id additionalAttributes firstChip otherChips =
  Miso.nodeHtmlKeyed
    "mdc-chip-set"
    (Miso.toKey id)
    (chipSetCs : chipSetInputCs : gridRole : additionalAttributes)
    (Prelude.map (chip << snd) (firstChip : otherChips))

chip :: Chip.Chip msg -> Miso.View msg
chip (Internal.Chip (config_@Internal.Config {Internal.additionalAttributes = additionalAttributes}) label) =
  Miso.div_
    [Miso.class_ "mdc-touch-target-wrapper"]
    [ Miso.nodeHtml
        "mdc-chip"
        ( Maybe.mapMaybe
            id
            [ chipCs,
              chipTouchCs,
              rowRole,
              removalHandler config_
            ]
            ++ additionalAttributes
        )
        ( Maybe.mapMaybe
            id
            [ rippleElt,
              leadingIconElt config_,
              primaryActionElt label,
              trailingIconElt config_
            ]
        )
    ]

chipSetCs :: Miso.Attribute msg
chipSetCs =
  Miso.class_ "mdc-chip-set"

chipSetInputCs :: Miso.Attribute msg
chipSetInputCs =
  Miso.class_ "mdc-chip-set--input"

gridRole :: Miso.Attribute msg
gridRole =
  Miso.textProp "role" "grid"

chipCs :: Maybe (Miso.Attribute msg)
chipCs =
  Just (Miso.class_ "mdc-chip")

chipTextCs :: Miso.Attribute msg
chipTextCs =
  Miso.class_ "mdc-chip__text"

chipTouchCs :: Maybe (Miso.Attribute msg)
chipTouchCs =
  Just (Miso.class_ "mdc-chip--touch")

chipPrimaryActionCs :: Miso.Attribute msg
chipPrimaryActionCs =
  Miso.class_ "mdc-chip__primary-action"

tabIndexProp :: Int -> Miso.Attribute msg
tabIndexProp tabIndex =
  Miso.intProp "tabIndex" tabIndex

buttonRole :: Miso.Attribute msg
buttonRole =
  Miso.textProp "role" "button"

rowRole :: Maybe (Miso.Attribute msg)
rowRole =
  Just (Miso.textProp "role" "row")

gridcellRole :: Miso.Attribute msg
gridcellRole =
  Miso.textProp "role" "gridcell"

removalHandler :: Chip.Config msg -> Maybe (Miso.Attribute msg)
removalHandler (Internal.Config {Internal.onDelete = onDelete}) =
  fmap (Miso.on "MDCChip:removal" Miso.emptyDecoder << const) onDelete

rippleElt :: Maybe (Miso.View msg)
rippleElt =
  Just (Miso.div_ [Miso.class_ "mdc-chip__ripple"] [])

leadingIconElt :: Chip.Config msg -> Maybe (Miso.View msg)
leadingIconElt (Internal.Config {Internal.leadingIcon = leadingIcon}) =
  fmap (fmap Void.absurd)
    <| case leadingIcon of
      Just (Internal.Icon {Internal.node = node, Internal.attributes = attributes, Internal.nodes = nodes}) ->
        Just
          <| node
            ( Miso.class_ "mdc-chip__icon" :
              Miso.class_ "mdc-chip__icon--leading" :
              tabIndexProp (-1) :
              buttonRole :
              attributes
            )
            nodes
      Just (Internal.SvgIcon {Internal.node = node, Internal.attributes = attributes, Internal.nodes = nodes}) ->
        Just
          <| node
            ( Svg.class_' "mdc-chip__icon" :
              Svg.class_' "mdc-chip__icon--leading" :
              tabIndexProp (-1) :
              buttonRole :
              attributes
            )
            nodes
      Nothing ->
        Nothing

primaryActionElt :: String -> Maybe (Miso.View msg)
primaryActionElt label =
  Just
    <| Miso.span_
      [chipPrimaryActionCs, gridcellRole, tabIndexProp (-1)]
      (Maybe.mapMaybe id [textElt label, touchElt])

textElt :: String -> Maybe (Miso.View msg)
textElt label =
  Just (Miso.span_ [chipTextCs, buttonRole] [Miso.text (Miso.String.toMisoString label)])

touchElt :: Maybe (Miso.View msg)
touchElt =
  Just (Miso.div_ [Miso.class_ "mdc-chip__touch"] [])

trailingIconElt :: Chip.Config msg -> Maybe (Miso.View msg)
trailingIconElt (Internal.Config {Internal.trailingIcon = trailingIcon, Internal.onDelete = onDelete}) =
  fmap
    ( \_ ->
        fmap Void.absurd
          <| case trailingIcon of
            Just (Internal.Icon {Internal.node = node, Internal.attributes = attributes, Internal.nodes = nodes}) ->
              node
                ( Miso.class_ "mdc-chip__icon" :
                  Miso.class_ "mdc-chip__icon--trailing" :
                  tabIndexProp (-1) :
                  buttonRole :
                  attributes
                )
                nodes
            Just (Internal.SvgIcon {Internal.node = node, Internal.attributes = attributes, Internal.nodes = nodes}) ->
              node
                ( Svg.class_' "mdc-chip__icon" :
                  Svg.class_' "mdc-chip__icon--trailing" :
                  tabIndexProp (-1) :
                  buttonRole :
                  attributes
                )
                nodes
            Nothing ->
              Miso.i_
                [ Miso.class_ "material-icons",
                  Miso.class_ "mdc-chip__icon",
                  Miso.class_ "mdc-chip__icon--trailing",
                  tabIndexProp (-1),
                  buttonRole
                ]
                [Miso.text "cancel"]
    )
    onDelete
