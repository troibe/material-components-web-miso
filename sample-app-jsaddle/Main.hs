{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell language pragma
module Main where

#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import qualified Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import Material.Button as MB
import Material.Card as MC
import Material.Checkbox as MCB
import Material.Chip.Action as Chip.Action
import Material.Chip.Choice as Chip.Choice
import Material.Chip.Filter as Chip.Filter
import Material.Chip.Input as Chip.Input
import Material.ChipSet.Action as ChipSet.Action
import Material.ChipSet.Choice as ChipSet.Choice
import Material.ChipSet.Filter as ChipSet.Filter
import Material.ChipSet.Input as ChipSet.Input
import Material.DataTable as DataTable
import Material.Dialog as MD
import Material.Drawer.Dismissible as MDD
import Material.Drawer.Permanent as MDP
import Material.Elevation as ME
import Material.Fab as Fab
import Material.FormField as FormField
import Material.HelperText as MHT
import Material.Icon as MI
import Material.IconButton as MIB
import Material.IconToggle as IconToggle
import Material.ImageList as ImageList
import Material.ImageList.Item as ImageListItem
import Material.LayoutGrid as LayoutGrid
import Material.LinearProgress as MLP
import Material.List as List
import Material.List.Item as ListItem
import Material.Menu as Menu
import Material.Radio as Radio
import Material.Ripple as Ripple
import Material.Select as Select
import Material.Select.Item as SelectItem
import Material.Slider as Slider
import Material.Snackbar as Snackbar
import Material.Switch as Switch
import Material.Tab as Tab
import Material.TabBar as TabBar
import Material.TextField as MTF
import Material.Theme as MT
import Material.TopAppBar as MTAB
import Material.Typography as MTY
import Miso
import Miso.String

(|>) = (Data.Function.&)

-- | Type synonym for an application model
data Model = Model
  { counter :: Int,
    queue :: Snackbar.Queue Action,
    switchState :: Bool,
    tabState :: Int,
    sliderState :: Float,
    menuState :: Bool,
    chipSetChoiceState :: Maybe String,
    chipSetFilterState :: (Bool, Bool),
    chipSetInputState :: [String],
    iconToggleState :: Bool,
    selectedItem :: Maybe String
  }
  deriving (Eq)

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  | Closed
  | SetActivated String
  | SnackbarClosed Snackbar.MessageId
  | PressSwitch
  | TabClicked Int
  | SliderChanged Float
  | ItemSelected String
  | MenuOpened
  | MenuClosed
  | ActionChipClicked String
  | ColorChanged String
  | ChipClicked String
  | InputChipDeleted String
  | IconToggleClicked
  deriving (Show, Eq)

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f = do
  bString <- B.readFile "material-components-web-elm.min.js"
  jSaddle <- JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) (JSaddle.jsaddleAppWithJs (B.append (JSaddle.jsaddleJs False) bString))
  Warp.runSettings (Warp.setPort 8081 (Warp.setTimeout 3600 Warp.defaultSettings)) jSaddle
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

extendedEvents :: M.Map MisoString Bool
extendedEvents =
  defaultEvents
    |> M.insert "MDCDialog:close" True
    |> M.insert "MDCDrawer:close" True
    |> M.insert "MDCList:action" True
    |> M.insert "MDCSnackbar:closed" True
    |> M.insert "MDCTab:interacted" True
    |> M.insert "MDCSlider:input" True
    |> M.insert "MDCMenuSurface:close" True
    |> M.insert "MDCChip:interaction" True
    |> M.insert "MDCIconButtonToggle:change" True

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model =
      Model
        { counter = 0,
          queue = Snackbar.initialQueue,
          switchState = False,
          tabState = 0,
          sliderState = 10.0,
          menuState = False,
          chipSetChoiceState = Just "Red",
          chipSetFilterState = (False, False),
          chipSetInputState = ["Chip One", "Chip Two"],
          iconToggleState = True,
          selectedItem = Just "Third"
        } -- initial model
    update = updateModel -- update function
    view = viewModel -- view function
    events = extendedEvents -- default delegated events and MDCDialog:close
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = Off -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m@Model {counter = counter} = noEff m {counter = counter + 1}
updateModel SubtractOne m@Model {counter = counter} = noEff m {counter = counter - 1}
updateModel NoOp m = noEff m
updateModel SayHelloWorld m =
  m <# do
    liftIO (putStrLn "Hello World!") >> pure NoOp
updateModel (ItemSelected item) m =
  m {selectedItem = (Just item)} <# do
    liftIO (putStrLn item) >> pure NoOp
updateModel (SetActivated item) m@Model {queue = queue} =
  let message =
        Snackbar.message item
          |> Snackbar.setActionIcon (Just (Snackbar.icon "close"))
          |> Snackbar.setOnActionIconClick SnackbarClosed
      newQueue = Snackbar.addMessage message queue
   in m {queue = newQueue} <# do
        liftIO (putStrLn item) >> pure NoOp
updateModel (SnackbarClosed messageId) m@Model {queue = queue} =
  let newQueue = Snackbar.close messageId queue
   in m {queue = newQueue} <# do
        liftIO (putStrLn $ show messageId) >> pure NoOp
updateModel PressSwitch m@Model {switchState = switchState} = noEff m {switchState = not switchState}
updateModel (TabClicked tabId) m = noEff m {tabState = tabId}
updateModel (SliderChanged value) m = noEff m {sliderState = value}
updateModel (MenuOpened) m = noEff m {menuState = True}
updateModel (MenuClosed) m = noEff m {menuState = False}
updateModel (ActionChipClicked chip) m =
  m <# do
    liftIO (putStrLn chip) >> pure NoOp
updateModel (ColorChanged chip) m = noEff m {chipSetChoiceState = Just chip}
updateModel (ChipClicked chip) m@Model {chipSetFilterState = (filterTops, filterShoes)} =
  case chip of
    "Tops" -> noEff m {chipSetFilterState = (not filterTops, filterShoes)}
    "Shoes" -> noEff m {chipSetFilterState = (filterTops, not filterShoes)}
    _ -> noEff m {chipSetFilterState = (filterTops, filterShoes)}
updateModel (InputChipDeleted inputChip) m@Model {chipSetInputState = inputChips} = noEff m {chipSetInputState = L.filter ((/=) inputChip) inputChips}
updateModel (IconToggleClicked) m@Model {iconToggleState = iconToggleState} = noEff m {iconToggleState = not iconToggleState}
updateModel Closed m = noEff m {counter = 0}

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@Model {counter = counter, switchState = switchState, sliderState = sliderState} =
  div_
    [ style_ $ M.singleton "display" "-ms-flexbox",
      style_ $ M.singleton "display" "flex",
      style_ $ M.singleton "height" "100vh"
    ]
    [ MDP.drawer
        MDP.config
        [ MDP.content
            []
            [ MDP.header
                []
                [ h3_ [MDP.title] [Miso.text "Mail"],
                  h6_ [MDP.subtitle] [Miso.text "email@material.io"]
                ]
            ]
        ],
      div_
        [MDD.appContent]
        [ MTAB.regular
            MTAB.config
            [ MTAB.row
                []
                [ MTAB.section
                    [MTAB.alignStart]
                    [ MIB.iconButton
                        (MIB.setAttributes [MTAB.navigationIcon] $ MIB.config)
                        "menu",
                      span_ [MTAB.title] [Miso.text "Title"]
                    ]
                ]
            ],
          div_
            [ style_ $ M.singleton "padding-left" "18px",
              style_ $ M.singleton "padding-right" "18px",
              style_ $ M.singleton "overflow" "auto",
              style_ $ M.singleton "height" "100%",
              style_ $ M.singleton "box-sizing" "border-box",
              MTAB.fixedAdjust,
              MDD.appContent
            ]
            [ MB.text (MB.setOnClick AddOne $ MB.config) "+",
              MHT.helperText (MHT.setPersistent True $ MHT.config) (show counter),
              MB.text (MB.setOnClick SubtractOne $ MB.config) "-",
              br_ [],
              MI.icon [MT.primary] "thumb_up",
              br_ [],
              MIB.iconButton (MIB.config) "thumb_down",
              MD.dialog
                (MD.setOnClose Closed $ MD.setOpen (counter /= 0) $ MD.config)
                ( MD.dialogContent
                    (Just "Test")
                    [Miso.text "Discard draft?"]
                    [ MB.text (MB.setOnClick Closed $ MB.config) "Cancel",
                      MB.text (MB.setOnClick Closed $ MB.config) "Discard"
                    ]
                ),
              br_ [],
              MLP.indeterminate MLP.config,
              br_ [],
              MCB.checkbox (MCB.setOnChange SayHelloWorld $ MCB.config),
              br_ [],
              MCB.checkbox (MCB.config),
              br_ [],
              Radio.radio
                ( Radio.setOnChange SayHelloWorld $
                    Radio.setChecked False $
                      Radio.config
                ),
              br_ [],
              Switch.switch (Switch.setOnChange PressSwitch $ Switch.setChecked switchState $ Switch.config),
              br_ [],
              MTF.filled (MTF.setLabel (Just "Hi") $ MTF.config),
              br_ [],
              activatedItemList m,
              br_ [],
              myImageList,
              br_ [],
              myRipple,
              br_ [],
              mySnackbar m,
              br_ [],
              myTabBar m,
              br_ [],
              MHT.helperText (MHT.config |> MHT.setPersistent True) (show sliderState),
              mySlider m,
              br_ [],
              myMenu m,
              br_ [],
              mySelect m,
              br_ [],
              myFormField,
              br_ [],
              myActionChipSet,
              br_ [],
              myChoiceChipSet m,
              br_ [],
              myFilterChipSet m,
              br_ [],
              myInputChipSet,
              br_ [],
              myDataTable,
              br_ [],
              myLayoutGrid,
              br_ [],
              myIconToggle m,
              br_ [],
              MC.card
                ( MC.setAttributes
                    [ style_ $ M.singleton "margin" "48px 0",
                      style_ $ M.singleton "width" "350px",
                      ME.z10
                    ]
                    $ MC.config
                )
                MC.Content
                  { blocks =
                      [ MC.Block $
                          div_
                            [style_ $ M.singleton "padding" "1rem"]
                            [ h2_
                                [ MTY.headline6,
                                  style_ $ M.singleton "margin" "0"
                                ]
                                [Miso.text "Title"],
                              h3_
                                [ MTY.subtitle2,
                                  MT.textSecondaryOnBackground,
                                  style_ $ M.singleton "margin" "0"
                                ]
                                [Miso.text "Subtitle"]
                            ],
                        MC.Block $
                          div_
                            []
                            [ p_
                                [ MTY.body2,
                                  MT.textSecondaryOnBackground,
                                  style_ $ M.singleton "padding" "0 1rem 0.5rem 1rem",
                                  style_ $ M.singleton "margin" "0"
                                ]
                                [Miso.text "Lorem ipsum..."]
                            ]
                      ],
                    actions =
                      Just $
                        MC.cardActions [MC.button MB.config "Visit"] [MC.icon MIB.config "favorite"]
                  }
            ],
          Fab.fab
            ( Fab.setOnClick SayHelloWorld $
                Fab.setAttributes
                  [ style_ $ M.singleton "position" "fixed",
                    style_ $ M.singleton "bottom" "2rem",
                    style_ $ M.singleton "right" "2rem"
                  ]
                  $ Fab.config
            )
            "favorite"
        ],
      link_
        [ rel_ "stylesheet",
          href_ "https://unpkg.com/material-components-web@6.0.0/dist/material-components-web.min.css"
        ],
      script_
        [ src_ "https://unpkg.com/material-components-web@6.0.0/dist/material-components-web.min.js"
        ]
        "",
      link_
        [ rel_ "stylesheet",
          href_ "https://fonts.googleapis.com/icon?family=Material+Icons"
        ]
    ]

demoList =
  [ style_ $ M.singleton "border" "1px solid rgba(0,0,0,.1)"
  ]

activatedItemList :: Model -> View Action
activatedItemList model =
  let listItem (icon, label) =
        ListItem.listItem
          ( ListItem.config
              |> ListItem.setSelected
                ( if "Star" == label
                    then Just ListItem.activated
                    else Nothing
                )
              |> ListItem.setOnClick (SetActivated label)
          )
          [ListItem.graphic [] [MI.icon [] icon], Miso.text $ ms label]
   in List.list
        (List.config |> List.setAttributes demoList)
        (listItem ("inbox", "Inbox" :: String))
        ( L.map
            listItem
            [ ("star", "Star" :: String),
              ("send", "Sent" :: String),
              ("drafts", "Drafts" :: String)
            ]
        )

myImageList :: View Action
myImageList =
  ImageList.imageList
    ( ImageList.config
        |> ImageList.setWithTextProtection True
    )
    [ ImageListItem.imageListItem
        ( ImageListItem.config
            |> ImageListItem.setLabel (Just "Photo")
            |> ImageListItem.setAttributes
              [ style_ $ M.singleton "width" "200px",
                style_ $ M.singleton "height" "200px",
                style_ $ M.singleton "margin" "2px"
              ]
        )
        "https://picsum.photos/200/200"
    ]

myRipple :: View Action
myRipple =
  span_
    [ style_ $ M.singleton "padding" "12px",
      style_ $ M.singleton "position" "relative"
    ]
    [ Miso.text "🙌",
      Ripple.unbounded Ripple.config
    ]

mySnackbar :: Model -> View Action
mySnackbar Model {queue = queue} =
  Snackbar.snackbar
    (Snackbar.config (\x -> SnackbarClosed x))
    queue

myTabBar :: Model -> View Action
myTabBar Model {tabState = tabState} =
  TabBar.tabBar
    TabBar.config
    [ Tab.tab
        ( Tab.config
            |> Tab.setActive (tabState == 0)
            |> Tab.setOnClick (TabClicked 0)
        )
        "Tab 1"
        Nothing,
      Tab.tab
        ( Tab.config
            |> Tab.setActive (tabState == 1)
            |> Tab.setOnClick (TabClicked 1)
        )
        "Tab 2"
        Nothing,
      Tab.tab
        ( Tab.config
            |> Tab.setActive (tabState == 2)
            |> Tab.setOnClick (TabClicked 2)
        )
        "Tab 3"
        Nothing
    ]

mySlider :: Model -> View Action
mySlider Model {sliderState = sliderState} =
  Slider.slider
    ( Slider.config
        |> Slider.setValue (Just sliderState)
        |> Slider.setOnInput SliderChanged
    )

myMenu :: Model -> View Action
myMenu Model {menuState = menuState} =
  Miso.div_
    [Menu.surfaceAnchor]
    [ MB.text
        (MB.config |> MB.setOnClick MenuOpened)
        "Open menu",
      Menu.menu
        ( Menu.config
            |> Menu.setOpen menuState
            |> Menu.setOnClose MenuClosed
        )
        [ List.list
            (List.config |> List.setWrapFocus True)
            ( ListItem.listItem
                (ListItem.config |> (ListItem.setOnClick NoOp))
                [Miso.text "Menu item"]
            )
            [ ListItem.listItem
                (ListItem.config |> (ListItem.setOnClick NoOp))
                [Miso.text "Menu item"]
            ]
        ]
    ]

myFormField :: View Action
myFormField =
  FormField.formField
    ( FormField.config
        |> FormField.setLabel (Just "My checkbox")
    )
    [MCB.checkbox MCB.config]

myActionChipSet :: View Action
myActionChipSet =
  ChipSet.Action.chipSet
    []
    ( Chip.Action.chip
        ( Chip.Action.config
            |> Chip.Action.setOnClick (ActionChipClicked "Chip One")
        )
        "Chip One"
    )
    [ Chip.Action.chip
        ( Chip.Action.config
            |> Chip.Action.setOnClick (ActionChipClicked "Chip Two")
        )
        "Chip Two"
    ]

myChoiceChipSet :: Model -> View Action
myChoiceChipSet Model {chipSetChoiceState = chipSetChoiceState} =
  ChipSet.Choice.chipSet
    ( ChipSet.Choice.config
        id
        |> ChipSet.Choice.setSelected chipSetChoiceState
        |> ChipSet.Choice.setOnChange ColorChanged
    )
    (Chip.Choice.chip Chip.Choice.config "Red")
    [ Chip.Choice.chip Chip.Choice.config "Blue"
    ]

myFilterChipSet :: Model -> View Action
myFilterChipSet Model {chipSetFilterState = (filterTops, filterShoes)} =
  ChipSet.Filter.chipSet
    []
    ( Chip.Filter.chip
        ( Chip.Filter.config
            |> Chip.Filter.setSelected filterTops
            |> Chip.Filter.setOnChange
              (ChipClicked "Tops")
        )
        "Tops"
    )
    [ Chip.Filter.chip
        ( Chip.Filter.config
            |> Chip.Filter.setSelected filterShoes
            |> Chip.Filter.setOnChange
              (ChipClicked "Shoes")
        )
        "Shoes"
    ]

myInputChipSet :: View Action
myInputChipSet =
  ChipSet.Input.chipSet
    "myInputChipSet"
    []
    ( "Chip One",
      Chip.Input.chip
        ( Chip.Input.config
            |> Chip.Input.setOnDelete (InputChipDeleted "Chip One")
        )
        "Chip One"
    )
    [ ( "Chip Two",
        Chip.Input.chip
          ( Chip.Input.config
              |> Chip.Input.setOnDelete (InputChipDeleted "Chip Two")
          )
          "Chip Two"
      )
    ]

myDataTable :: View Action
myDataTable =
  DataTable.dataTable
    DataTable.config
    [ DataTable.row
        []
        [DataTable.cell [] [Miso.text "Desert"]]
    ]
    [ DataTable.row
        []
        [DataTable.cell [] [Miso.text "Frozen yogurt"]]
    ]

myLayoutGrid :: View Action
myLayoutGrid =
  LayoutGrid.layoutGrid
    []
    [ LayoutGrid.inner
        []
        [ LayoutGrid.cell [] [MHT.helperText (MHT.setPersistent True $ MHT.config) "Test1"],
          LayoutGrid.cell [] [MHT.helperText (MHT.setPersistent True $ MHT.config) "Test2"],
          LayoutGrid.cell [] [MHT.helperText (MHT.setPersistent True $ MHT.config) "Test3"]
        ]
    ]

myIconToggle :: Model -> View Action
myIconToggle Model {iconToggleState = iconToggleState} =
  IconToggle.iconToggle
    ( IconToggle.config
        |> IconToggle.setOn iconToggleState
        |> IconToggle.setOnChange IconToggleClicked
    )
    (IconToggle.icon "favorite_outlined")
    (IconToggle.icon "favorite")

mySelectItem :: String -> SelectItem String Action
mySelectItem text =
  SelectItem.selectItem
    (SelectItem.config text)
    [Miso.text $ Miso.String.toMisoString text]

mySelect :: Model -> View Action
mySelect Model {selectedItem = selectedItem} =
  Select.outlined
    (Select.setLabel (Just "Choose wisely") $ Select.setOnChange ItemSelected $ Select.setSelected selectedItem Select.config)
    (mySelectItem "First")
    [ mySelectItem "Second",
      mySelectItem "Third",
      mySelectItem "Fourth"
    ]
