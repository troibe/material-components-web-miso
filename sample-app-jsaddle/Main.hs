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
import Material.Button as MB
import Material.Icon as MI
import Material.IconButton as MIB
import Material.Theme as MT
import Material.HelperText as MHT
import Material.Elevation as ME
import Material.Dialog as MD

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
  Warp.runSettings (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
    JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) JSaddle.jsaddleApp
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
  , MD.dialog (MD.setOpen True$MD.config) (MD.dialogContent (Just "Test") [] [])
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
