{-# LANGUAGE OverloadedStrings #-}

module Material.TextField
  ( Config,
    config,
    setOnInput,
    setOnChange,
    setLabel,
    setFullwidth,
    setValue,
    setPlaceholder,
    setDisabled,
    setRequired,
    setValid,
    setMinLength,
    setMaxLength,
    setPattern,
    setType,
    setMin,
    setMax,
    setStep,
    setLeadingIcon,
    setTrailingIcon,
    setAttributes,
    filled,
    outlined,
    Icon,
    icon,
  )
where

import qualified Data.Aeson
import qualified Data.Maybe as Maybe
import qualified Material.Icon as Icon
import qualified Miso
import qualified Miso.Html.Event
import qualified Miso.String

-- | Configuration of a text field
data Config msg = Config
  { label :: Maybe String,
    fullwidth :: Bool,
    value :: Maybe String,
    placeholder :: Maybe String,
    disabled :: Bool,
    required :: Bool,
    valid :: Bool,
    minLength :: Maybe Int,
    maxLength :: Maybe Int,
    pattern :: Maybe String,
    type_ :: Maybe String,
    min :: Maybe Int,
    max :: Maybe Int,
    step :: Maybe Int,
    leadingIcon :: Maybe (Icon msg),
    trailingIcon :: Maybe (Icon msg),
    additionalAttributes :: [Miso.Attribute msg],
    onInput :: Maybe (String -> msg),
    onChange :: Maybe (String -> msg)
  }

-- | Text field trailing or leading icon -
data Icon msg
  = Icon (Miso.View msg)

-- | Default configuration of a text field
config :: Config msg
config =
  Config
    { label = Nothing,
      fullwidth = False,
      value = Nothing,
      placeholder = Nothing,
      disabled = False,
      required = False,
      valid = True,
      minLength = Nothing,
      maxLength = Nothing,
      pattern = Nothing,
      type_ = Nothing,
      Material.TextField.min = Nothing,
      Material.TextField.max = Nothing,
      step = Nothing,
      leadingIcon = Nothing,
      trailingIcon = Nothing,
      additionalAttributes = [],
      onInput = Nothing,
      onChange = Nothing
    }

-- | Specify a text field's label
setLabel :: Maybe String -> Config msg -> Config msg
setLabel label config_ =
  config_ {label = label}

-- | Specify a text field to be fullwidth
setFullwidth :: Bool -> Config msg -> Config msg
setFullwidth fullwidth config_ =
  config_ {fullwidth = fullwidth}

-- | Specify a text field's value
setValue :: Maybe String -> Config msg -> Config msg
setValue value config_ =
  config_ {value = value}

-- | Specify a text field's placeholder
setPlaceholder :: Maybe String -> Config msg -> Config msg
setPlaceholder placeholder config_ =
  config_ {placeholder = placeholder}

-- | Specify a text field to be disabled
-- Disabled text fields cannot be interacted with and have no visual interaction
-- effect.
setDisabled :: Bool -> Config msg -> Config msg
setDisabled disabled config_ =
  config_ {disabled = disabled}

-- | Specify a text field to be required
setRequired :: Bool -> Config msg -> Config msg
setRequired required config_ =
  config_ {required = required}

-- | Specify a text field to be valid
setValid :: Bool -> Config msg -> Config msg
setValid valid config_ =
  config_ {valid = valid}

-- | Specify a text field's minimum length
setMinLength :: Maybe Int -> Config msg -> Config msg
setMinLength minLength config_ =
  config_ {minLength = minLength}

-- | Specify a text field's maximum length
setMaxLength :: Maybe Int -> Config msg -> Config msg
setMaxLength maxLength config_ =
  config_ {maxLength = maxLength}

-- | Specify a text field's pattern
setPattern :: Maybe String -> Config msg -> Config msg
setPattern pattern config_ =
  config_ {pattern = pattern}

-- | Specify a text field's type
setType :: Maybe String -> Config msg -> Config msg
setType type_ config_ =
  config_ {type_ = type_}

-- | Specify a text field's minimum value
setMin :: Maybe Int -> Config msg -> Config msg
setMin min config_ =
  config_ {Material.TextField.min = min}

-- | Specify a text field's maximum value
setMax :: Maybe Int -> Config msg -> Config msg
setMax max config_ =
  config_ {Material.TextField.max = max}

-- | Specify a text field's step value
setStep :: Maybe Int -> Config msg -> Config msg
setStep step config_ =
  config_ {step = step}

-- | Specify a text field's leading icon
setLeadingIcon :: Maybe (Icon msg) -> Config msg -> Config msg
setLeadingIcon leadingIcon config_ =
  config_ {leadingIcon = leadingIcon}

-- | Specify a text field's trailing icon
setTrailingIcon :: Maybe (Icon msg) -> Config msg -> Config msg
setTrailingIcon trailingIcon config_ =
  config_ {trailingIcon = trailingIcon}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user changes the value inside the text field
setOnInput :: (String -> msg) -> Config msg -> Config msg
setOnInput onInput config_ =
  config_ {onInput = Just onInput}

-- | Specify a message when the user confirms a changed value inside the text
-- field
setOnChange :: (String -> msg) -> Config msg -> Config msg
setOnChange onChange config_ =
  config_ {onChange = Just onChange}

-- | Filled text field view function
filled :: Config msg -> Miso.View msg
filled config_ =
  textField False config_

-- | Outlined text field view function
outlined :: Config msg -> Miso.View msg
outlined config_ =
  textField True config_

textField :: Bool -> Config msg -> Miso.View msg
textField outlined_ (config_@Config {additionalAttributes = additionalAttributes, fullwidth = fullwidth}) =
  Miso.nodeHtml
    "mdc-text-field"
    ( Maybe.mapMaybe
        id
        [ rootCs,
          noLabelCs config_,
          filledCs outlined_,
          outlinedCs outlined_,
          fullwidthCs config_,
          disabledCs config_,
          withLeadingIconCs config_,
          withTrailingIconCs config_,
          valueProp config_,
          disabledProp config_,
          requiredProp config_,
          validProp config_,
          patternProp config_,
          minLengthProp config_,
          maxLengthProp config_,
          minProp config_,
          maxProp config_,
          stepProp config_
        ]
        ++ additionalAttributes
    )
    ( Prelude.concat
        [ leadingIconElt config_,
          if fullwidth
            then
              if outlined_
                then
                  [ inputElt config_,
                    notchedOutlineElt config_
                  ]
                else
                  [ inputElt config_,
                    lineRippleElt
                  ]
            else
              if outlined_
                then
                  [ inputElt config_,
                    notchedOutlineElt config_
                  ]
                else
                  [ inputElt config_,
                    labelElt config_,
                    lineRippleElt
                  ],
          trailingIconElt config_
        ]
    )

-- | A text field's icon, either leading or trailing
icon :: [Miso.Attribute msg] -> String -> Icon msg
icon additionalAttributes iconName =
  Icon (Icon.icon (Miso.class_ "mdc-text-field__icon" : additionalAttributes) iconName)

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-text-field")

filledCs :: Bool -> Maybe (Miso.Attribute msg)
filledCs outlined_ =
  if not outlined_
    then Just (Miso.class_ "mdc-text-field--filled")
    else Nothing

outlinedCs :: Bool -> Maybe (Miso.Attribute msg)
outlinedCs outlined_ =
  if outlined_
    then Just (Miso.class_ "mdc-text-field--outlined")
    else Nothing

fullwidthCs :: Config msg -> Maybe (Miso.Attribute msg)
fullwidthCs (Config {fullwidth = fullwidth}) =
  if fullwidth
    then Just (Miso.class_ "mdc-text-field--fullwidth")
    else Nothing

disabledCs :: Config msg -> Maybe (Miso.Attribute msg)
disabledCs (Config {disabled = disabled}) =
  if disabled
    then Just (Miso.class_ "mdc-text-field--disabled")
    else Nothing

withLeadingIconCs :: Config msg -> Maybe (Miso.Attribute msg)
withLeadingIconCs (Config {leadingIcon = leadingIcon}) =
  case leadingIcon of
    Nothing -> Nothing
    _ -> Just (Miso.class_ "mdc-text-field--with-leading-icon")

withTrailingIconCs :: Config msg -> Maybe (Miso.Attribute msg)
withTrailingIconCs (Config {trailingIcon = trailingIcon}) =
  case trailingIcon of
    Nothing -> Nothing
    _ -> Just (Miso.class_ "mdc-text-field--with-trailing-icon")

requiredProp :: Config msg -> Maybe (Miso.Attribute msg)
requiredProp (Config {required = required}) =
  Just (Miso.boolProp "required" required)

validProp :: Config msg -> Maybe (Miso.Attribute msg)
validProp (Config {valid = valid}) =
  Just (Miso.boolProp "valid" valid)

minLengthProp :: Config msg -> Maybe (Miso.Attribute msg)
minLengthProp (Config {minLength = minLength}) =
  Just
    ( Miso.intProp
        "minLength"
        (Maybe.fromMaybe (-1) minLength)
    )

maxLengthProp :: Config msg -> Maybe (Miso.Attribute msg)
maxLengthProp (Config {maxLength = maxLength}) =
  Just
    ( Miso.intProp
        "maxLength"
        (Maybe.fromMaybe (-1) maxLength)
    )

minLengthAttr :: Config msg -> Maybe (Miso.Attribute msg)
minLengthAttr (Config {minLength = minLength}) =
  Maybe.maybe Nothing (\x -> Just (Miso.intProp "minLength" x)) minLength

maxLengthAttr :: Config msg -> Maybe (Miso.Attribute msg)
maxLengthAttr (Config {maxLength = maxLength}) =
  Maybe.maybe Nothing (\x -> Just (Miso.intProp "maxLength" x)) maxLength

minProp :: Config msg -> Maybe (Miso.Attribute msg)
minProp (Config {Material.TextField.min = min}) =
  Just
    ( Miso.textProp
        "min"
        ( Miso.String.toMisoString
            (Maybe.fromMaybe "" (Maybe.maybe Nothing (\x -> Just (show x)) min))
        )
    )

maxProp :: Config msg -> Maybe (Miso.Attribute msg)
maxProp (Config {Material.TextField.max = max}) =
  Just
    ( Miso.textProp
        "max"
        ( Miso.String.toMisoString
            (Maybe.fromMaybe "" (Maybe.maybe Nothing (\x -> Just (show x)) max))
        )
    )

stepProp :: Config msg -> Maybe (Miso.Attribute msg)
stepProp (Config {step = step}) =
  Just
    ( Miso.textProp
        "step"
        ( Miso.String.toMisoString
            (Maybe.fromMaybe "" (Maybe.maybe Nothing (\x -> Just (show x)) step))
        )
    )

valueProp :: Config msg -> Maybe (Miso.Attribute msg)
valueProp (Config {value = value}) =
  Maybe.maybe Nothing (\x -> Just (Miso.textProp "value" (Miso.String.toMisoString x))) value

placeholderAttr :: Config msg -> Maybe (Miso.Attribute msg)
placeholderAttr (Config {placeholder = placeholder}) =
  Maybe.maybe Nothing (\x -> Just (Miso.placeholder_ (Miso.String.toMisoString x))) placeholder

leadingIconElt :: Config msg -> [Miso.View msg]
leadingIconElt (Config {leadingIcon = leadingIcon}) =
  case leadingIcon of
    Nothing ->
      []
    Just (Icon html) ->
      [html]

trailingIconElt :: Config msg -> [Miso.View msg]
trailingIconElt (Config {trailingIcon = trailingIcon}) =
  case trailingIcon of
    Nothing ->
      []
    Just (Icon html) ->
      [html]

inputHandler :: Config msg -> Maybe (Miso.Attribute msg)
inputHandler (Config {onInput = onInput}) =
  Maybe.maybe Nothing (\f -> Just (Miso.Html.Event.onInput (\s -> f (Miso.String.fromMisoString s)))) onInput

changeHandler :: Config msg -> Maybe (Miso.Attribute msg)
changeHandler (Config {onChange = onChange}) =
  Maybe.maybe Nothing (\f -> Just (Miso.Html.Event.onChange (\s -> f (Miso.String.fromMisoString s)))) onChange

inputElt :: Config msg -> Miso.View msg
inputElt config_ =
  Miso.input_
    ( Maybe.mapMaybe
        id
        [ inputCs,
          typeAttr config_,
          ariaLabelAttr config_,
          placeholderAttr config_,
          inputHandler config_,
          changeHandler config_,
          minLengthAttr config_,
          maxLengthAttr config_
        ]
    )

inputCs :: Maybe (Miso.Attribute msg)
inputCs =
  Just (Miso.class_ "mdc-text-field__input")

patternProp :: Config msg -> Maybe (Miso.Attribute msg)
patternProp (Config {pattern = pattern}) =
  Maybe.maybe Nothing (\x -> Just (Miso.textProp "pattern" (Miso.String.toMisoString x))) pattern

typeAttr :: Config msg -> Maybe (Miso.Attribute msg)
typeAttr (Config {type_ = type_}) =
  Maybe.maybe Nothing (\x -> Just (Miso.type_ (Miso.String.toMisoString x))) type_

ariaLabelAttr :: Config msg -> Maybe (Miso.Attribute msg)
ariaLabelAttr (Config {fullwidth = fullwidth, placeholder = placeholder, label = label}) =
  if fullwidth
    then Maybe.maybe Nothing (\x -> Just (Miso.textProp "aria-label" (Miso.String.toMisoString x))) label
    else Nothing

disabledProp :: Config msg -> Maybe (Miso.Attribute msg)
disabledProp (Config {disabled = disabled}) =
  Just (Miso.boolProp "disabled" disabled)

labelElt :: Config msg -> Miso.View msg
labelElt (Config {label = label, value = value}) =
  let floatingLabelCs =
        "mdc-floating-label"

      floatingLabelFloatAboveCs =
        "mdc-floating-label--float-above"
   in case label of
        Just str ->
          Miso.div_
            [ if Maybe.fromMaybe "" value /= ""
                then Miso.class_ (Miso.String.toMisoString (floatingLabelCs ++ " " ++ floatingLabelFloatAboveCs))
                else Miso.class_ (Miso.String.toMisoString floatingLabelCs),
              Miso.textProp
                "foucClassNames"
                (Miso.String.toMisoString (Data.Aeson.encode [floatingLabelFloatAboveCs]))
            ]
            [Miso.text (Miso.String.toMisoString str)]
        Nothing ->
          Miso.text ""

noLabelCs :: Config msg -> Maybe (Miso.Attribute msg)
noLabelCs (Config {label = label}) =
  if label == Nothing
    then Just (Miso.class_ "mdc-text-field--no-label")
    else Nothing

lineRippleElt :: Miso.View msg
lineRippleElt =
  Miso.div_ [Miso.class_ "mdc-line-ripple"] []

notchedOutlineElt :: Config msg -> Miso.View msg
notchedOutlineElt config_ =
  Miso.div_
    [Miso.class_ "mdc-notched-outline"]
    [ notchedOutlineLeadingElt,
      notchedOutlineNotchElt config_,
      notchedOutlineTrailingElt
    ]

notchedOutlineLeadingElt :: Miso.View msg
notchedOutlineLeadingElt =
  Miso.div_ [Miso.class_ "mdc-notched-outline__leading"] []

notchedOutlineTrailingElt :: Miso.View msg
notchedOutlineTrailingElt =
  Miso.div_ [Miso.class_ "mdc-notched-outline__trailing"] []

notchedOutlineNotchElt :: Config msg -> Miso.View msg
notchedOutlineNotchElt config_ =
  Miso.div_ [Miso.class_ "mdc-notched-outline__notch"] [labelElt config_]
