{-# LANGUAGE OverloadedStrings #-}

module Material.FormField
  ( Config,
    config,
    setOnClick,
    setLabel,
    setAlignEnd,
    setFor,
    setAttributes,
    formField,
  )
where

import qualified Data.Function
import qualified Data.Maybe as Maybe
import qualified Miso
import qualified Miso.String

(<<) = (Data.Function..)

-- | Configuration of a form field
data Config msg = Config
  { label :: Maybe String,
    for :: Maybe String,
    alignEnd :: Bool,
    additionalAttributes :: [Miso.Attribute msg],
    onClick :: Maybe msg
  }

-- | Specify a form field's label
setLabel :: Maybe String -> Config msg -> Config msg
setLabel label config_ =
  config_ {label = label}

-- | Specify a form field label's HTML5 for attribute
setFor :: Maybe String -> Config msg -> Config msg
setFor for config_ =
  config_ {for = for}

-- | Specify whether the form field's label is positioned after its control
-- This is usefile for, say, checkboxes.
setAlignEnd :: Bool -> Config msg -> Config msg
setAlignEnd alignEnd config_ =
  config_ {alignEnd = alignEnd}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user clicks on the label
setOnClick :: msg -> Config msg -> Config msg
setOnClick onClick config_ =
  config_ {onClick = Just onClick}

-- | Default configuration of a form field
config :: Config msg
config =
  Config
    { label = Nothing,
      for = Nothing,
      alignEnd = False,
      additionalAttributes = [],
      onClick = Nothing
    }

-- | Form field view function
formField :: Config msg -> [Miso.View msg] -> Miso.View msg
formField (config_@Config {additionalAttributes = additionalAttributes}) nodes =
  Miso.nodeHtml
    "mdc-form-field"
    ( Maybe.mapMaybe
        id
        [ rootCs,
          alignEndCs config_
        ]
        ++ additionalAttributes
    )
    (nodes ++ [labelElt config_])

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-form-field")

alignEndCs :: Config msg -> Maybe (Miso.Attribute msg)
alignEndCs (Config {alignEnd = alignEnd}) =
  if alignEnd
    then Just (Miso.class_ "mdc-form-field--align-end")
    else Nothing

forAttr :: Config msg -> Maybe (Miso.Attribute msg)
forAttr (Config {for = for}) =
  fmap (Miso.for_ << Miso.String.toMisoString) (for)

clickHandler :: Config msg -> Maybe (Miso.Attribute msg)
clickHandler (Config {onClick = onClick}) =
  fmap Miso.onClick onClick

labelElt :: Config msg -> Miso.View msg
labelElt (config_@Config {label = label}) =
  Miso.label_
    ( Maybe.mapMaybe
        id
        [ forAttr config_,
          clickHandler config_
        ]
    )
    [Miso.text (Maybe.maybe "" Miso.String.toMisoString label)]
