{-# LANGUAGE OverloadedStrings #-}

module Material.HelperText
  ( Config,
    config,
    setPersistent,
    setAttributes,
    helperText,
    helperLine,
    characterCounter,
  )
where

import Data.Maybe
import Miso.Html
import Miso.String

-- | Configuration of a helper text
data Config msg = Config
  { persistent :: Bool,
    additionalAttributes :: [Attribute msg]
  }

-- | Default configuration of a helper text
config :: Config msg
config =
  Config
    { persistent = False,
      additionalAttributes = []
    }

-- | Specify whether a helper text should be persistent
-- Persistent helper texts always display regardless of whether the input has
-- focus or not.
setPersistent :: Bool -> Config msg -> Config msg
setPersistent persistent config_ =
  config_ {persistent = persistent}

-- | Specify additional attributes
setAttributes :: [Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Helper text view function
-- The helper text is expected to be a direct child of the helper line.
helperText :: Config msg -> String -> View msg
helperText (config_@Config {additionalAttributes = additionalAttributes}) string =
  div_
    ( mapMaybe
        id
        [ helperTextCs,
          persistentCs config_,
          ariaHiddenAttr
        ]
        ++ additionalAttributes
    )
    [text (toMisoString string)]

-- | Helper text line view function
-- The helper line is expected to be the wrapping element of the helper text. It
-- is expected to be a direct sibling of the text field that it belongs to.
helperLine :: [Attribute msg] -> [View msg] -> View msg
helperLine additionalAttributes nodes =
  div_ (helperLineCs : additionalAttributes) nodes

helperTextCs :: Maybe (Attribute msg)
helperTextCs =
  Just (class_ "mdc-text-field-helper-text")

helperLineCs :: Attribute msg
helperLineCs =
  class_ "mdc-text-field-helper-line"

persistentCs :: Config msg -> Maybe (Attribute msg)
persistentCs config_@Config {persistent = persistent} =
  if persistent
    then Just (class_ "mdc-text-field-helper-text--persistent")
    else Nothing

ariaHiddenAttr :: Maybe (Attribute msg)
ariaHiddenAttr =
  Just (textProp "aria-hidden" "true")

-- | Character counter view function
characterCounter :: [Attribute msg] -> View msg
characterCounter additionalAttributes =
  div_ (characterCounterCs : additionalAttributes) []

characterCounterCs :: Attribute msg
characterCounterCs =
  class_ "mdc-text-field-character-counter"
