-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main where

import           Miso
import           Miso.String

#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Material.Button as MB
import Material.Icon as MI
import Material.IconButton as MIB
import Material.Theme as MT
import Material.HelperText as MHT
import Material.Elevation as ME
import Material.Dialog as MD
import Material.Card as MC
import Material.Typography as MTY

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  | Closed
  deriving (Show, Eq)

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f = do
  bString <- B.readFile "material-components-web-elm.min.js"
  jSaddle <- JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) (JSaddle.jsaddleAppWithJs (B.append (JSaddle.jsaddleJs False) bString))
  Warp.runSettings (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) jSaddle
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = 0                    -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World") >> pure NoOp
updateModel Closed _ = noEff 0

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ []
  [ MB.text (MB.setOnClick AddOne$MB.config) "+"
  , MHT.helperText (MHT.setPersistent True$MHT.config) (show x)
  , MB.text (MB.setOnClick SubtractOne$MB.config) "-"
  , br_ []
  , MI.icon [primary] "thumb_up"
  , br_ []
  , MIB.iconButton (MIB.setAttributes [ME.z10]$MIB.config) "thumb_down"
  , MD.dialog (MD.setOnClose Closed$MD.setOpen (x/=0)$MD.config) (MD.dialogContent (Just "Test") [ Miso.text "Discard draft?" ]
    [ MB.text (MB.setOnClick Closed$MB.config) "Cancel"
    , MB.text (MB.setOnClick Closed$MB.config) "Discard"
    ]
  )
  , MC.card ( MC.setAttributes
                [ style_ $ M.singleton "margin" "48px 0"
                , style_ $ M.singleton "width" "350px"
                ]
              $ MC.config )
    MC.Content
      { blocks = 
        [ MC.Block $
          div_
            [ style_ $ M.singleton "padding" "1rem" ]
            [ h2_ 
              [ MTY.headline6
              , style_ $ M.singleton "margin" "0"
              ]
              [ Miso.text "Title" ]
            , h3_ 
              [ MTY.subtitle2
              , MT.textSecondaryOnBackground
              , style_ $ M.singleton "margin" "0"
              ]
              [ Miso.text "Subtitle" ]
            ]
        , MC.Block $
          div_ []
            [ p_ 
              [ MTY.body2
              , MT.textSecondaryOnBackground
              , style_ $ M.singleton "padding" "0 1rem 0.5rem 1rem"
              ]
              [ Miso.text "Lorem ipsum..."] ]
        ]
      , actions =
        Just $
          MC.cardActions [ MC.button MB.config "Visit" ] [MC.icon MIB.config "favorite"] 
      }
  , link_
    [ rel_ "stylesheet"
    , href_ "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css"
    ]
  , script_
    [ src_ "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.js"
    ] ""
  , link_
    [ rel_ "stylesheet"
    , href_ "https://fonts.googleapis.com/icon?family=Material+Icons"
    ]
 ]
