{-# LANGUAGE OverloadedStrings #-}

module Material.ChipSet.Filter (chipSet) where

import qualified Data.Function
import qualified Data.Maybe as Maybe
import qualified Data.Void as Void
import qualified Material.Chip.Filter as Chip
import qualified Material.Chip.Filter.Internal as Internal
import qualified Miso
import qualified Miso.String
import qualified Miso.Svg as Svg

(|>) = (Data.Function.&)

(<|) = (Data.Function.$)

(<<) = (Data.Function..)

-- | Filter chip set view function
chipSet :: [Miso.Attribute msg] -> Chip.Chip msg -> [Chip.Chip msg] -> Miso.View msg
chipSet additionalAttributes firstChip otherChips =
  Miso.nodeHtml
    "mdc-chip-set"
    (chipSetCs : chipSetFilterCs : gridRole : additionalAttributes)
    (Prelude.map chip (firstChip : otherChips))

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
              selectedProp config_,
              interactionHandler config_
            ]
            ++ additionalAttributes
        )
        ( Maybe.mapMaybe
            id
            [ rippleElt,
              leadingIconElt config_,
              checkmarkElt,
              primaryActionElt label
            ]
        )
    ]

chipSetCs :: Miso.Attribute msg
chipSetCs =
  Miso.class_ "mdc-chip-set"

chipSetFilterCs :: Miso.Attribute msg
chipSetFilterCs =
  Miso.class_ "mdc-chip-set--filter"

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

selectedProp :: Chip.Config msg -> Maybe (Miso.Attribute msg)
selectedProp (Internal.Config {Internal.selected = selected}) =
  Just (Miso.boolProp "selected" selected)

buttonRole :: Miso.Attribute msg
buttonRole =
  Miso.textProp "role" "button"

rowRole :: Maybe (Miso.Attribute msg)
rowRole =
  Just (Miso.textProp "role" "row")

gridcellRole :: Miso.Attribute msg
gridcellRole =
  Miso.textProp "role" "gridcell"

interactionHandler :: Chip.Config msg -> Maybe (Miso.Attribute msg)
interactionHandler (Internal.Config {Internal.onChange = onChange}) =
  fmap (Miso.on "MDCChip:interaction" Miso.emptyDecoder << const) onChange

rippleElt :: Maybe (Miso.View msg)
rippleElt =
  Just (Miso.div_ [Miso.class_ "mdc-chip__ripple"] [])

leadingIconElt :: Chip.Config msg -> Maybe (Miso.View msg)
leadingIconElt (Internal.Config {Internal.icon = icon, Internal.selected = selected}) =
  fmap (fmap Void.absurd)
    <| case icon of
      Just (Internal.Icon {Internal.node = node, Internal.attributes = attributes, Internal.nodes = nodes}) ->
        Just
          <| node
            ( Miso.class_ "mdc-chip__icon mdc-chip__icon--leading" :
              attributes
            )
            nodes
      Just (Internal.SvgIcon {Internal.node = node, Internal.attributes = attributes, Internal.nodes = nodes}) ->
        Just
          <| node
            ( Svg.class_' "mdc-chip__icon mdc-chip__icon--leading" :
              attributes
            )
            nodes
      Nothing ->
        Nothing

checkmarkElt :: Maybe (Miso.View msg)
checkmarkElt =
  Just
    ( Miso.div_
        [Miso.class_ "mdc-chip__checkmark"]
        [ Svg.svg_
            [ Svg.class_' "mdc-chip__checkmark-svg",
              Svg.viewBox_ "-2 -3 30 30"
            ]
            [ Svg.path_
                [ Svg.class_' "mdc-chip__checkmark-path",
                  Svg.fill_ "none",
                  Svg.stroke_ "black",
                  Svg.d_ "M1.73,12.91 8.1,19.28 22.79,4.59"
                ]
                []
            ]
        ]
    )

primaryActionElt :: String -> Maybe (Miso.View msg)
primaryActionElt label =
  Just
    <| Miso.span_
      [chipPrimaryActionCs, gridcellRole]
      (Maybe.mapMaybe id [textElt label, touchElt])

textElt :: String -> Maybe (Miso.View msg)
textElt label =
  Just (Miso.span_ [chipTextCs, buttonRole] [Miso.text (Miso.String.toMisoString label)])

touchElt :: Maybe (Miso.View msg)
touchElt =
  Just (Miso.div_ [Miso.class_ "mdc-chip__touch"] [])
