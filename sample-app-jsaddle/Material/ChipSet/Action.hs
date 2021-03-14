{-# LANGUAGE OverloadedStrings #-}

module Material.ChipSet.Action (chipSet) where

import qualified Data.Function
import qualified Data.Maybe as Maybe
import qualified Data.Void as Void
import qualified Material.Chip.Action as Chip
import qualified Material.Chip.Action.Internal as Internal
import qualified Miso
import qualified Miso.String
import qualified Miso.Svg as Svg

(|>) = (Data.Function.&)

(<|) = (Data.Function.$)

(<<) = (Data.Function..)

-- | Chip set view function
chipSet :: [Miso.Attribute msg] -> Chip.Chip msg -> [Chip.Chip msg] -> Miso.View msg
chipSet additionalAttributes firstChip otherChips =
  Miso.nodeHtml
    "mdc-chip-set"
    (chipSetCs : gridRole : additionalAttributes)
    (Prelude.map chip (firstChip : otherChips))

chipSetCs :: Miso.Attribute msg
chipSetCs =
  Miso.class_ "mdc-chip-set"

gridRole :: Miso.Attribute msg
gridRole =
  Miso.textProp "role" "grid"

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
              interactionHandler config_
            ]
            ++ additionalAttributes
        )
        ( Maybe.mapMaybe
            id
            [ rippleElt,
              leadingIconElt config_,
              primaryActionElt label
            ]
        )
    ]

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
interactionHandler (Internal.Config {Internal.onClick = onClick}) =
  fmap (Miso.on "MDCChip:interaction" Miso.emptyDecoder << const) onClick

rippleElt :: Maybe (Miso.View msg)
rippleElt =
  Just (Miso.div_ [Miso.class_ "mdc-chip__ripple"] [])

leadingIconElt :: Chip.Config msg -> Maybe (Miso.View msg)
leadingIconElt (Internal.Config {Internal.icon = icon}) =
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
