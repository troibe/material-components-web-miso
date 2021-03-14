{-# LANGUAGE OverloadedStrings #-}

module Material.Dialog
  ( Config,
    config,
    setOnClose,
    setOpen,
    setAttributes,
    dialog,
    Content,
    dialogContent,
  )
where

import qualified Data.List as List
import Data.Maybe
import Miso.Event.Decoder
import Miso.Html
import Miso.Html.Event
import Miso.String

-- | Configuration of a dialog
data Config msg = Config
  { open :: Bool,
    additionalAttributes :: [Attribute msg],
    onClose :: Maybe msg
  }

-- | Default configuration of a dialog
config :: Config msg
config =
  Config
    { open = False,
      additionalAttributes = [],
      onClose = Nothing
    }

-- | Specify whether a dialog is open
setOpen :: Bool -> Config msg -> Config msg
setOpen open config_ =
  config_ {open = open}

-- | Specify additional attributes
setAttributes :: [Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user closes the dialog
setOnClose :: msg -> Config msg -> Config msg
setOnClose onClose config_ =
  config_ {onClose = Just onClose}

-- | Dialog content
data Content msg = Content
  { title :: Maybe String,
    content :: [View msg],
    actions :: [View msg]
  }

dialogContent :: Maybe String -> [View msg] -> [View msg] -> Content msg
dialogContent t c a =
  Content
    { title = t,
      content = c,
      actions = a
    }

-- | Dialog view function
dialog :: Config msg -> Content msg -> View msg
dialog (config_@Config {additionalAttributes = additionalAttributes}) content =
  nodeHtml
    "mdc-dialog"
    ( mapMaybe
        id
        [ rootCs,
          openProp config_,
          roleAttr,
          ariaModalAttr,
          closeHandler config_
        ]
        ++ additionalAttributes
    )
    [ containerElt content,
      scrimElt
    ]

rootCs :: Maybe (Attribute msg)
rootCs =
  Just (class_ "mdc-dialog")

openProp :: Config msg -> Maybe (Attribute msg)
openProp (config_@Config {open = open}) =
  Just (boolProp "open" open)

roleAttr :: Maybe (Attribute msg)
roleAttr =
  Just (textProp "role" "alertdialog")

ariaModalAttr :: Maybe (Attribute msg)
ariaModalAttr =
  Just (boolProp "aria-modal" True)

closeHandler :: Config msg -> Maybe (Attribute msg)
closeHandler (config_@Config {onClose = onClose}) = case onClose of
  Nothing -> Nothing
  Just x -> Just (on "MDCDialog:close" emptyDecoder (const x))

containerElt :: Content msg -> View msg
containerElt content =
  div_ [class_ "mdc-dialog__container"] [surfaceElt content]

surfaceElt :: Content msg -> View msg
surfaceElt content =
  div_
    [class_ "mdc-dialog__surface"]
    ( mapMaybe
        id
        [ titleElt content,
          contentElt content,
          actionsElt content
        ]
    )

titleElt :: Content msg -> Maybe (View msg)
titleElt (Content {title = title}) =
  case title of
    Just title_ ->
      Just (div_ [class_ "mdc-dialog__title"] [text (toMisoString title_)])
    Nothing ->
      Nothing

contentElt :: Content msg -> Maybe (View msg)
contentElt (Content {content = content}) =
  Just (div_ [class_ "mdc-dialog__content"] content)

actionsElt :: Content msg -> Maybe (View msg)
actionsElt (Content {actions = actions}) =
  if List.null actions
    then Nothing
    else Just (div_ [class_ "mdc-dialog__actions"] actions)

scrimElt :: View msg
scrimElt =
  div_ [class_ "mdc-dialog__scrim"] []
