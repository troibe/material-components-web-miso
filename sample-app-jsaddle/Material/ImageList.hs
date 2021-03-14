{-# LANGUAGE OverloadedStrings #-}

module Material.ImageList
  ( Config,
    config,
    setMasonry,
    setWithTextProtection,
    setAttributes,
    imageList,
  )
where

import qualified Data.Function
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Material.ImageList.Item (ImageListItem)
import qualified Material.ImageList.Item.Internal as ImageListItem
import qualified Miso
import qualified Miso.String

(|>) = (Data.Function.&)

-- | Configuration of an image list
data Config msg = Config
  { masonry :: Bool,
    withTextProtection :: Bool,
    additionalAttributes :: [Miso.Attribute msg]
  }

-- | Default configuration of an image list
config :: Config msg
config =
  Config
    { masonry = False,
      withTextProtection = False,
      additionalAttributes = []
    }

-- | Specify whether an image list is a _masonry image list_
-- The masonry image list variant presents images vertically arranged into several
-- columns. In this layout, images may be any combination of aspect ratios.
setMasonry :: Bool -> Config msg -> Config msg
setMasonry masonry config_ =
  config_ {masonry = masonry}

-- | Specify whether an image list item's label should display in a scrim on top
-- of the image
-- By default, image list item's labels display below the image.
setWithTextProtection :: Bool -> Config msg -> Config msg
setWithTextProtection withTextProtection config_ =
  config_ {withTextProtection = withTextProtection}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Image list view function
imageList :: Config msg -> [ImageListItem.ImageListItem msg] -> Miso.View msg
imageList (config_@Config {additionalAttributes = additionalAttributes}) listItems =
  Miso.nodeHtml
    "mdc-image-list"
    ( Maybe.mapMaybe
        id
        [ rootCs,
          masonryCs config_,
          withTextProtectionCs config_
        ]
        ++ additionalAttributes
    )
    (Prelude.map (listItemElt config_) listItems)

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-image-list")

masonryCs :: Config msg -> Maybe (Miso.Attribute msg)
masonryCs (Config {masonry = masonry}) =
  if masonry
    then Just (Miso.class_ "mdc-image-list--masonry")
    else Nothing

withTextProtectionCs :: Config msg -> Maybe (Miso.Attribute msg)
withTextProtectionCs (Config {withTextProtection = withTextProtection}) =
  if withTextProtection
    then Just (Miso.class_ "mdc-image-list--with-text-protection")
    else Nothing

listItemElt :: Config msg -> ImageListItem.ImageListItem msg -> Miso.View msg
listItemElt (config_@Config {masonry = masonry}) (listItem@(ImageListItem.ImageListItem (ImageListItem.Config {ImageListItem.href = href, ImageListItem.additionalAttributes = additionalAttributes}))) =
  let inner =
        [ if masonry
            then imageElt masonry listItem
            else imageAspectContainerElt masonry listItem,
          supportingElt listItem
        ]
   in Miso.nodeHtml
        "mdc-image-list-item"
        (Miso.class_ "mdc-image-list__item" : additionalAttributes)
        ( href
            |> fmap (\href_ -> [Miso.a_ [Miso.href_ (Miso.String.toMisoString href_)] inner])
            |> Maybe.fromMaybe inner
        )

imageAspectContainerElt :: Bool -> ImageListItem.ImageListItem msg -> Miso.View msg
imageAspectContainerElt masonry (listItem@(ImageListItem.ImageListItem (ImageListItem.Config {ImageListItem.href = href}))) =
  Miso.div_
    ( Maybe.mapMaybe
        id
        [ Just (Miso.class_ "mdc-image-list__image-aspect-container"),
          fmap (\_ -> Miso.class_ "mdc-ripple-surface") href
        ]
    )
    [imageElt masonry listItem]

imageElt :: Bool -> ImageListItem.ImageListItem msg -> Miso.View msg
imageElt masonry (ImageListItem.ImageListItem (ImageListItem.Config {ImageListItem.href = href, ImageListItem.image = image})) =
  let img =
        Miso.img_
          [ Miso.class_ "mdc-image-list__image",
            Miso.src_ (Miso.String.toMisoString image)
          ]
   in if masonry
        then
          if href /= Nothing
            then Miso.div_ [Miso.class_ "mdc-ripple-surface"] [img]
            else img
        else
          Miso.div_
            [ Miso.class_ "mdc-image-list__image",
              Miso.style_ $ Map.singleton "background-image" (Miso.String.toMisoString ("url('" ++ image ++ "')"))
            ]
            []

supportingElt :: ImageListItem.ImageListItem msg -> Miso.View msg
supportingElt (ImageListItem.ImageListItem (ImageListItem.Config {ImageListItem.label = label})) =
  case label of
    Just string ->
      Miso.div_
        [Miso.class_ "mdc-image-list__supporting"]
        [Miso.span_ [Miso.class_ "mdc-image-list__label"] [Miso.text (Miso.String.toMisoString string)]]
    Nothing ->
      Miso.text ""
