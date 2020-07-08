{-# LANGUAGE OverloadedStrings #-}
module Material.Checkbox
    ( Config(..), config
    , setOnChange
    , State(..), setState
    , setDisabled
    , setTouch
    , setAttributes
    , checkbox
    , checked, unchecked
    , indeterminate
    ) where

import Miso
import Miso.Event.Decoder
import Miso.Svg.Element
import Miso.Svg.Attribute
import Control.Exception
import System.IO.Unsafe
import Data.Maybe
import Material.Checkbox.Internal


{-| Default configuration of a checkbox
-}
config :: Config msg
config =
    Config
        { state = Nothing
        , disabled = False
        , additionalAttributes = []
        , Material.Checkbox.Internal.onChange = Nothing
        , touch = True
        }


{-| Specify a checkbox' state
A checkbox may be in `checked`, `unchecked` or `indeterminate` state.
-}
setState :: Maybe State -> Config msg -> Config msg
setState state config_ =
    config_ { state = state }


{-| Specify whether a checkbox is disabled
Disabled checkboxes cannot be interacted with and have no visual interaction
effect.
-}
setDisabled :: Bool -> Config msg -> Config msg
setDisabled disabled config_ =
    config_ { disabled = disabled }


{-| Specify additional attributes
-}
setAttributes :: [Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
    config_ { additionalAttributes = additionalAttributes }


{-| Specify a message when the user changes a checkbox
-}
setOnChange :: msg -> Config msg -> Config msg
setOnChange onChange config_ =
    config_ { Material.Checkbox.Internal.onChange = Just onChange }


{-| Specify whether touch support is enabled (enabled by default)
Touch support is an accessibility guideline that states that touch targets
should be at least 48 x 48 pixels in size. Use this configuration option to
disable increased touch target size.
**Note:** Checkboxes with touch support will be wrapped in a HTML div element
to prevent potentially overlapping touch targets on adjacent elements.
-}
setTouch :: Bool -> Config msg -> Config msg
setTouch touch config_ =
    config_ { touch = touch }


{-| Unchecked state
-}
unchecked :: State
unchecked =
    Unchecked


{-| Checked state
-}
checked :: State
checked =
    Material.Checkbox.Internal.Checked


{-| Indeterminate state
-}
indeterminate :: State
indeterminate =
    Indeterminate


{-| Checkbox view function
-}
checkbox :: Config msg -> View msg
checkbox config_@Config{ touch=touch, additionalAttributes=additionalAttributes } =
    let
        wrapTouch node =
            if touch then
                div_ [ class_ "mdc-touch-target-wrapper" ] [ node ]

            else
                node
    in
    wrapTouch $
        nodeHtml "mdc-checkbox"
            (mapMaybe id
                [ rootCs
                , touchCs config_
                , checkedProp config_
                , indeterminateProp config_
                , disabledProp config_
                ]
                ++ additionalAttributes
            )
            [ nativeControlElt config_
            , backgroundElt
            ]


rootCs :: Maybe (Attribute msg)
rootCs =
    Just (class_ "mdc-checkbox")


touchCs :: Config msg -> Maybe (Attribute msg)
touchCs Config{ touch=touch } =
    if touch then
        Just (class_ "mdc-checkbox--touch")

    else
        Nothing


checkedProp :: Config msg -> Maybe (Attribute msg)
checkedProp Config{ state=state } =
    Just (boolProp "checked" (state == Just Material.Checkbox.Internal.Checked))


indeterminateProp :: Config msg -> Maybe (Attribute msg)
indeterminateProp Config{ state=state } =
    Just (boolProp "indeterminate" (state == Just Indeterminate))


disabledProp :: Config msg -> Maybe (Attribute msg)
disabledProp Config{ disabled=disabled } =
    Just (boolProp "disabled" disabled)


changeHandler :: Config msg -> Maybe (Attribute msg)
changeHandler Config{ state=state, Material.Checkbox.Internal.onChange=onChange } =
    -- Note: MDCList choses to send a change event to all checkboxes, thus we
    -- have to check here if the state actually changed.
    case onChange of
        Nothing -> Nothing
        Just msg ->
            unsafePerformIO$catch (
                return (
                    Just (
                        on "change" checkedDecoder (\(Miso.Checked isChecked) ->
                                if
                                    (isChecked && state /= Just Material.Checkbox.Internal.Checked)
                                        || (not isChecked && state /= Just Unchecked)
                                then
                                    msg

                                else
                                    error "not applicable"
                            )
                        )
                    )   
                ) ex
                where
                    ex :: SomeException -> IO (Maybe (Attribute msg))
                    ex _ = return Nothing


nativeControlElt :: Config msg -> View msg
nativeControlElt config_ =
    input_
        (mapMaybe id
            [ Just (type_ "checkbox")
            , Just (class_ "mdc-checkbox__native-control")
            , checkedProp config_
            , indeterminateProp config_
            , changeHandler config_
            ]
        )


backgroundElt :: View msg
backgroundElt =
    div_
        [ class_ "mdc-checkbox__background" ]
        [ svg_
            [ class_ "mdc-checkbox__checkmark"
            , viewBox_ "0 0 24 24"
            ]
            [ Miso.Svg.Element.path_
                [ class_ "mdc-checkbox__checkmark-path"
                , fill_ "none"
                , d_ "M1.73,12.91 8.1,19.28 22.79,4.59"
                ]
                []
            ]
        , div_ [ class_ "mdc-checkbox__mixedmark" ] []
        ]