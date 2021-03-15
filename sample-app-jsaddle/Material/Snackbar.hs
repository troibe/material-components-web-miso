{-# LANGUAGE OverloadedStrings #-}

module Material.Snackbar
  ( Config,
    config,
    setCloseOnEscape,
    setAttributes,
    snackbar,
    Queue,
    initialQueue,
    MessageId,
    close,
    addMessage,
    message,
    Message,
    setActionButton,
    setOnActionButtonClick,
    setActionIcon,
    setOnActionIconClick,
    setLeading,
    setStacked,
    setTimeoutMs,
    Icon,
    icon,
    customIcon,
    svgIcon,
  )
where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Function
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple
import qualified Miso
import qualified Miso.String
import qualified Miso.Svg as Svg
import qualified Miso.Svg.Attribute

(|>) = (Data.Function.&)

(<|) = (Data.Function..)

-- | Queue of messages
data Queue msg = Queue
  { messages :: [(MessageId, Message msg)],
    nextMessageId :: MessageId
  }
  deriving (Eq)

-- | Message identifier type
data MessageId
  = MessageId Int
  deriving (Eq, Show)

inc :: MessageId -> MessageId
inc (MessageId messageId) =
  MessageId (messageId + 1)

-- | Initial empty queue
initialQueue :: Queue msg
initialQueue =
  Queue
    { messages = [],
      nextMessageId = MessageId 0
    }

-- | Hide the currently showing message
close :: MessageId -> Queue msg -> Queue msg
close messageId queue@Queue {messages = messages} =
  queue
    { messages =
        case messages of
          [] ->
            []
          (currentMessageId, _) : otherMessages ->
            if currentMessageId == messageId
              then otherMessages
              else messages
    }

-- | Adds a message to the queue
addMessage :: Message msg -> Queue msg -> Queue msg
addMessage message_ queue@Queue {messages = messages, nextMessageId = nextMessageId} =
  queue
    { messages = messages ++ [(nextMessageId, message_)],
      nextMessageId = inc nextMessageId
    }

-- | Configuration of a snackbar
data Config msg = Config
  { closeOnEscape :: Bool,
    additionalAttributes :: [Miso.Attribute msg],
    onClosed :: MessageId -> msg
  }

-- | Default configuration of a snackbar
config :: (MessageId -> msg) -> Config msg
config onClosed =
  Config
    { closeOnEscape = False,
      additionalAttributes = [],
      onClosed = onClosed
    }

-- | Specify whether the snackbar's messages should close when the user presses
-- escape
setCloseOnEscape :: Bool -> Config msg -> Config msg
setCloseOnEscape closeOnEscape config_ =
  config_ {closeOnEscape = closeOnEscape}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Snackbar view function
snackbar :: Config msg -> Queue msg -> Miso.View msg
snackbar (config_@Config {additionalAttributes = additionalAttributes}) (queue@Queue {messages = messages, nextMessageId = nextMessageId}) =
  let (currentMessageId, currentMessage) =
        Maybe.listToMaybe messages
          |> fmap (Bifunctor.second Just)
          |> Maybe.fromMaybe (MessageId (-1), Nothing)
   in Miso.nodeHtml
        "mdc-snackbar"
        ( Maybe.mapMaybe
            id
            [ rootCs,
              closeOnEscapeProp config_,
              leadingCs currentMessage,
              stackedCs currentMessage,
              messageIdProp currentMessageId,
              timeoutMsProp currentMessage,
              closedHandler currentMessageId config_
            ]
            ++ additionalAttributes
        )
        [surfaceElt currentMessageId (Maybe.fromMaybe (message "") currentMessage)]

-- | Snackbar message
data Message msg = Message
  { label :: String,
    actionButton :: Maybe String,
    onActionButtonClick :: Maybe (MessageId -> msg),
    actionIcon :: Maybe (Icon msg),
    onActionIconClick :: Maybe (MessageId -> msg),
    leading :: Bool,
    stacked :: Bool,
    timeoutMs :: Maybe Int
  }

instance Eq (Message msg) where
  Message
    { label = aLabel,
      actionButton = aActionButton,
      leading = aLeading,
      stacked = aStacked,
      timeoutMs = aTimeoutMs
    }
    == Message
      { label = bLabel,
        actionButton = bActionButton,
        leading = bLeading,
        stacked = bStacked,
        timeoutMs = bTimeoutMs
      } =
      aLabel == bLabel
        && aActionButton == bActionButton
        && aLeading == bLeading
        && aStacked == bStacked
        && aTimeoutMs == bTimeoutMs

-- | Specify a message's action button label
setActionButton :: Maybe String -> Message msg -> Message msg
setActionButton actionButton message_ =
  message_ {actionButton = actionButton}

-- | Specify a message when the user clicks on a message's action button
setOnActionButtonClick :: (MessageId -> msg) -> Message msg -> Message msg
setOnActionButtonClick onActionButtonClick message_ =
  message_ {onActionButtonClick = Just onActionButtonClick}

-- | Specify a message's action icon
setActionIcon :: Maybe (Icon msg) -> Message msg -> Message msg
setActionIcon actionIcon message_ =
  message_ {actionIcon = actionIcon}

-- | Specify a message when the user clicks on a message's action icon
setOnActionIconClick :: (MessageId -> msg) -> Message msg -> Message msg
setOnActionIconClick onActionIconClick message_ =
  message_ {onActionIconClick = Just onActionIconClick}

-- | Specify whether a message should display _leading_
-- Messages are by default centered within the viewport. On larger screens, they
-- can optionally be displyed on the _leading_ edge of the screen. To display a
-- message as leading, set its `setLeading` configuration option to `True`.
setLeading :: Bool -> Message msg -> Message msg
setLeading leading message_ =
  message_ {leading = leading}

-- | Specify whether a message should be stacked
-- Stacked messages display their label above their action button or icon. This
-- works best for messages with a long label.
setStacked :: Bool -> Message msg -> Message msg
setStacked stacked message_ =
  message_ {stacked = stacked}

-- | Specify a message's timeout in milliseconds
setTimeoutMs :: Maybe Int -> Message msg -> Message msg
setTimeoutMs timeoutMs message_ =
  message_ {timeoutMs = timeoutMs}

-- | Default snackbar message (empty label)
message :: String -> Message msg
message label =
  Message
    { label = label,
      actionButton = Nothing,
      onActionButtonClick = Nothing,
      actionIcon = Nothing,
      onActionIconClick = Nothing,
      leading = False,
      stacked = False,
      timeoutMs = Just 5000
    }

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-snackbar")

closeOnEscapeProp :: Config msg -> Maybe (Miso.Attribute msg)
closeOnEscapeProp (Config {closeOnEscape = closeOnEscape}) =
  Just (Miso.boolProp "closeOnEscape" closeOnEscape)

leadingCs :: Maybe (Message msg) -> Maybe (Miso.Attribute msg)
leadingCs message_ =
  Maybe.maybe
    Nothing
    ( \(Message {leading = leading}) ->
        if leading
          then Just (Miso.class_ "mdc-snackbar--leading")
          else Nothing
    )
    message_

stackedCs :: Maybe (Message msg) -> Maybe (Miso.Attribute msg)
stackedCs message_ =
  Maybe.maybe
    Nothing
    ( \(Message {stacked = stacked}) ->
        if stacked
          then Just (Miso.class_ "mdc-snackbar--stacked")
          else Nothing
    )
    message_

messageIdProp :: MessageId -> Maybe (Miso.Attribute msg)
messageIdProp (MessageId messageId) =
  Just (Miso.intProp "messageId" messageId)

clamp :: Ord a => a -> a -> a -> a
clamp low high = min high . max low

timeoutMsProp :: Maybe (Message msg) -> Maybe (Miso.Attribute msg)
timeoutMsProp message_ =
  let normalizedTimeoutMs =
        message_
          |> Maybe.maybe
            Nothing
            (\(Message {timeoutMs = timeoutMs}) -> fmap (clamp 4000 10000) timeoutMs)
          |> Maybe.fromMaybe indefiniteTimeout

      indefiniteTimeout =
        -1
   in Just (Miso.intProp "timeoutMs" normalizedTimeoutMs)

closedHandler :: MessageId -> Config msg -> Maybe (Miso.Attribute msg)
closedHandler messageId (Config {onClosed = onClosed}) =
  Just (Miso.on "MDCSnackbar:closed" Miso.emptyDecoder (\_ -> onClosed messageId))

ariaStatusRoleAttr :: Miso.Attribute msg
ariaStatusRoleAttr =
  Miso.textProp "aria-role" "status"

ariaPoliteLiveAttr :: Miso.Attribute msg
ariaPoliteLiveAttr =
  Miso.textProp "aria-live" "polite"

surfaceElt :: MessageId -> Message msg -> Miso.View msg
surfaceElt messageId message_ =
  Miso.div_
    [Miso.class_ "mdc-snackbar__surface"]
    [ labelElt message_,
      actionsElt messageId message_
    ]

labelElt :: Message msg -> Miso.View msg
labelElt (Message {label = label}) =
  Miso.div_
    [Miso.class_ "mdc-snackbar__label", ariaStatusRoleAttr, ariaPoliteLiveAttr]
    [Miso.text (Miso.String.toMisoString label)]

actionsElt :: MessageId -> Message msg -> Miso.View msg
actionsElt messageId message_ =
  Miso.div_
    [Miso.class_ "mdc-snackbar__actions"]
    ( Maybe.mapMaybe
        id
        [ actionButtonElt messageId message_,
          actionIconElt messageId message_
        ]
    )

actionButtonElt :: MessageId -> Message msg -> Maybe (Miso.View msg)
actionButtonElt messageId (message_@Message {actionButton = actionButton}) =
  fmap
    ( \actionButtonLabel ->
        Miso.button_
          ( Maybe.mapMaybe
              id
              [ actionButtonCs,
                actionButtonClickHandler messageId message_
              ]
          )
          [Miso.text (Miso.String.toMisoString actionButtonLabel)]
    )
    actionButton

actionButtonCs :: Maybe (Miso.Attribute msg)
actionButtonCs =
  Just (Miso.class_ "mdc-button mdc-snackbar__action")

actionButtonClickHandler :: MessageId -> Message msg -> Maybe (Miso.Attribute msg)
actionButtonClickHandler messageId (Message {onActionButtonClick = onActionButtonClick}) =
  fmap (Miso.onClick <| (|>) messageId) onActionButtonClick

actionIconElt :: MessageId -> Message msg -> Maybe (Miso.View msg)
actionIconElt messageId (message_@Message {actionIcon = actionIcon}) =
  case actionIcon of
    Just (Icon {node = node, attributes = attributes, nodes = nodes}) ->
      Just
        ( node
            ( Maybe.mapMaybe
                id
                [ Just (Miso.class_ "mdc-icon-button"),
                  Just (Miso.class_ "mdc-snackbar__dismiss"),
                  actionIconClickHandler messageId message_
                ]
                ++ attributes
            )
            nodes
        )
    Just (SvgIcon {node = node, attributes = attributes, nodes = nodes}) ->
      Just
        ( node
            ( Maybe.mapMaybe
                id
                [ Just (Miso.Svg.Attribute.class_' "mdc-icon-button"),
                  Just (Miso.Svg.Attribute.class_' "mdc-snackbar__dismiss"),
                  actionIconClickHandler messageId message_
                ]
                ++ attributes
            )
            nodes
        )
    Nothing ->
      Nothing

actionIconClickHandler :: MessageId -> Message msg -> Maybe (Miso.Attribute msg)
actionIconClickHandler messageId (Message {onActionIconClick = onActionIconClick}) =
  fmap (Miso.onClick <| (|>) messageId) onActionIconClick

-- | Icon type
data Icon msg
  = Icon
      { node :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg,
        attributes :: [Miso.Attribute msg],
        nodes :: [Miso.View msg]
      }
  | SvgIcon
      { node :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg,
        attributes :: [Miso.Attribute msg],
        nodes :: [Miso.View msg]
      }

-- | Material Icon
--    Snackbar.message "Something happened"
--        |> Snackbar.setActionIcon
--            (Just (Snackbar.icon "favorite"))
icon :: String -> Icon msg
icon iconName =
  customIcon Miso.i_ [Miso.class_ "material-icons"] [Miso.text (Miso.String.toMisoString iconName)]

-- | Custom icon
--    Snackbar.message "Something happened"
--        |> Snackbar.setActionIcon
--            (Just
--                (Snackbar.customIcon Miso.i
--                    [ Miso.class_ "fab fa-font-awesome" ]
--                    []
--                )
--            )
customIcon ::
  ([Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg) ->
  [Miso.Attribute msg] ->
  [Miso.View msg] ->
  Icon msg
customIcon node attributes nodes =
  Icon {node = node, attributes = attributes, nodes = nodes}

-- | SVG icon
--    Snackbar.message "Something happened"
--        |> Snackbar.setActionIcon
--            (Just
--                (Snackbar.svgIcon
--                    [ Svg.Attributes.viewBox "…" ]
--                    [-- …
--                    ]
--                )
--            )
svgIcon :: [Miso.Attribute msg] -> [Miso.View msg] -> Icon msg
svgIcon attributes nodes =
  SvgIcon {node = Svg.svg_, attributes = attributes, nodes = nodes}
