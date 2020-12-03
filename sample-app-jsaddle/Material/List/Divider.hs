{-# LANGUAGE OverloadedStrings #-}
module Material.List.Divider 
    ( Config, config
    , setPadded
    , setInset
    , setAttributes
    , listItem
    , group
    )

{-| Lists are continuous, vertical indexes of text or images.

This module concerns a list's divider elements. If you are looking for the list
container, refer to [Material.List](Material-List), and if you are looking for
the list item, refer to [Material.List.Item](Material-List-Item).


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [List Item Divider](#list-item-divider)
  - [List Group Divider](#list-group-divider)


# Resources

  - [Demo: Lists](https://aforemny.github.io/material-components-web-elm/#lists)
  - [Material Design Guidelines: Lists](https://material.io/design/components/lists.html)
  - [MDC Web: List](https://github.com/material-components/material-components-web/tree/master/packages/mdc-list)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-list#sass-mixins)


# Basic Usage

    import Material.List as List
    import Material.List.Divider as ListDivider
    import Material.List.Item as ListItem

    main =
        List.list List.config
            [ ListItem.listItem ListItem.config
                [ text "Line item" ]
            , ListDivider.listItem ListDivider.config
            , ListItem.listItem ListItem.config
                [ text "Line item" ]
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setPadded
@docs setInset
@docs setAttributes


# List Item Divider

List items may be seperated by a divider. The divider may optionally be _inset_
so that it does not intersect the list item's graphic, or _padded_ so that it
does not intersect the list item's meta.

    List.list List.config
        [ ListItem.listItem ListItem.config
            [ text "List item" ]
        , ListDivider.listItem ListDivider.config
        , ListItem.listItem ListItem.config
            [ text "List item" ]
        ]

@docs listItem


### List Group Divider

Multiple lists within a group may be visually seperated by a list group divider.

    List.group []
        [ List.list List.config
            [ ListItem.listItem ListItem.config [ text "Folder" ]
            , ListItem.listItem ListItem.config [ text "Folder" ]
            ]
        , ListDivider.group []
        , List.list List.config
            [ ListItem.listItem ListItem.config [ text "File" ]
            , ListItem.listItem ListItem.config [ text "File" ]
            ]
        ]

@docs group

-}

import Html  (Html)
import Miso.Attributes  (class)
import Material.List.Item  (ListItem)
import Material.List.Item.Internal as ListItem


{-| Configuration of a list item divider
-}
data Config msg
    = Config
        { inset :: Bool
        , padded :: Bool
        , additionalAttributes :: [Miso.Attribute msg]
        }


{-| Default configuration of a list item divider
-}
config :: Config msg
config =
    Config
        { inset = False
        , padded = False
        , additionalAttributes = []
        }


{-| Specify whether a list divider should be _inset_

Insert list item dividers to not intersect a list item's meta.

-}
setInset :: Bool -> Config msg -> Config msg
setInset inset config_ =
    config_ { inset = inset }


{-| Specify whether a list divider should be _padded_

Padded list item dividers do not intersect a list item's avatar.

-}
setPadded :: Bool -> Config msg -> Config msg
setPadded padded config_ =
    config_ { padded = padded }


{-| Specify additional attributes
-}
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
    config_ { additionalAttributes = additionalAttributes }


{-| List item divider view function
-}
listItem :: Config msg -> ListItem msg
listItem (config_@Config { additionalAttributes }) =
    ListItem.ListItemDivider <|
        Miso.li
            (Maybe.mapMaybe id
                [ listDividerCs
                , separatorRoleAttr
                , insetCs config_
                , paddedCs config_
                ]
                ++ additionalAttributes
            )
            []


listDividerCs :: Maybe (Miso.Attribute msg)
listDividerCs =
    Just (Miso.class_ "mdc-list-divider")


separatorRoleAttr :: Maybe (Miso.Attribute msg)
separatorRoleAttr =
    Just (Miso.Attributes.attribute "role" "separator")


insetCs :: Config msg -> Maybe (Miso.Attribute msg)
insetCs (Config { inset }) =
    if inset then
        Just (Miso.class_ "mdc-list-divider--inset")

    else
        Nothing


paddedCs :: Config msg -> Maybe (Miso.Attribute msg)
paddedCs (Config { padded }) =
    if padded then
        Just (Miso.class_ "mdc-list-divider--padded")

    else
        Nothing


{-| List group divider view function
-}
group :: [Miso.Attribute msg] -> Miso.View msg
group additionalAttributes =
    Miso.hr (Maybe.mapMaybe id [ listDividerCs ] ++ additionalAttributes) []
