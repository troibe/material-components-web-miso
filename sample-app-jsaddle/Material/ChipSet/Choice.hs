{-# LANGUAGE OverloadedStrings #-}

module Material.ChipSet.Choice
  ( Config,
    config,
    setSelected,
    setOnChange,
    setAttributes,
    chipSet,
  )
where

import qualified Data.Function
import qualified Data.Maybe as Maybe
import qualified Data.Void as Void
import qualified Material.Chip.Choice as Chip
import qualified Material.Chip.Choice.Internal as Internal
import qualified Miso
import qualified Miso.String
import qualified Miso.Svg as Svg

(|>) = (Data.Function.&)

(<|) = (Data.Function.$)

(<<) = (Data.Function..)

-- | Configuration of a choice chip set
data Config a msg = Config
  { selected :: Maybe a,
    onChange :: Maybe (a -> msg),
    toLabel :: a -> String,
    additionalAttributes :: [Miso.Attribute msg]
  }

-- | Default configuration of a choice chip set
config :: (a -> String) -> Config a msg
config toLabel =
  Config
    { selected = Nothing,
      onChange = Nothing,
      toLabel = toLabel,
      additionalAttributes = []
    }

-- | Specify which chip is selected
setSelected :: Maybe a -> Config a msg -> Config a msg
setSelected selected config_ =
  config_ {selected = selected}

-- | Specify a message when the user clicks on a chip
setOnChange :: (a -> msg) -> Config a msg -> Config a msg
setOnChange onChange config_ =
  config_ {onChange = Just onChange}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config a msg -> Config a msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Choice chip set view function
chipSet :: (Eq a) => Config a msg -> Chip.Chip a msg -> [Chip.Chip a msg] -> Miso.View msg
chipSet (config_@Config {selected = selected, onChange = onChange, toLabel = toLabel, additionalAttributes = additionalAttributes}) firstChip otherChips =
  Miso.nodeHtml
    "mdc-chip-set"
    (chipSetCs : chipSetChoiceCs : gridRole : additionalAttributes)
    (Prelude.map (chip selected onChange toLabel) (firstChip : otherChips))

chip :: (Eq a) => Maybe a -> Maybe (a -> msg) -> (a -> String) -> Chip.Chip a msg -> Miso.View msg
chip selected onChange toLabel (Internal.Chip (config_@Internal.Config {Internal.additionalAttributes = additionalAttributes}) value) =
  Miso.div_
    [Miso.class_ "mdc-touch-target-wrapper"]
    [ Miso.nodeHtml
        "mdc-chip"
        ( Maybe.mapMaybe
            id
            [ chipCs,
              chipTouchCs,
              rowRole,
              selectedProp (Just value == selected),
              interactionHandler (fmap ((|>) value) onChange)
            ]
            ++ additionalAttributes
        )
        ( Maybe.mapMaybe
            id
            [ rippleElt,
              leadingIconElt config_,
              primaryActionElt (toLabel value)
            ]
        )
    ]

chipSetCs :: Miso.Attribute msg
chipSetCs =
  Miso.class_ "mdc-chip-set"

chipSetChoiceCs :: Miso.Attribute msg
chipSetChoiceCs =
  Miso.class_ "mdc-chip-set--choice"

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

selectedProp :: Bool -> Maybe (Miso.Attribute msg)
selectedProp selected =
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

interactionHandler :: Maybe msg -> Maybe (Miso.Attribute msg)
interactionHandler msg =
  fmap (Miso.on "MDCChip:interaction" Miso.emptyDecoder << const) msg

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
