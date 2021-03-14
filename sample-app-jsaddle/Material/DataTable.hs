{-# LANGUAGE OverloadedStrings #-}

module Material.DataTable
  ( Config,
    config,
    setLabel,
    setAttributes,
    dataTable,
    Row,
    row,
    selected,
    Cell,
    cell,
    numericCell,
    checkboxCell,
  )
where

import qualified Data.Function
import qualified Data.Maybe as Maybe
import qualified Material.Checkbox as Checkbox
import qualified Material.Checkbox.Internal as Checkbox.Internal
import qualified Miso
import qualified Miso.String

(|>) = (Data.Function.&)

(<|) = (Data.Function.$)

(<<) = (Data.Function..)

-- | Configuration of a data table
data Config msg = Config
  { label :: Maybe String,
    additionalAttributes :: [Miso.Attribute msg]
  }

-- | Default configuration of a data table
config :: Config msg
config =
  Config
    { label = Nothing,
      additionalAttributes = []
    }

-- | Specify the data table's HTML5 aria-label attribute
setLabel :: Maybe String -> Config msg -> Config msg
setLabel label config_ =
  config_ {label = label}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Data table view function
dataTable ::
  Config msg ->
  [Row msg] ->
  [Row msg] ->
  Miso.View msg
dataTable (config_@Config {additionalAttributes = additionalAttributes}) thead tbody =
  Miso.nodeHtml
    "mdc-data-table"
    (dataTableCs : additionalAttributes)
    [ Miso.table_
        ( Maybe.mapMaybe
            id
            [ dataTableTableCs,
              ariaLabelAttr config_
            ]
        )
        [ Miso.thead_ [] (Prelude.map headerRow thead),
          Miso.tbody_ [dataTableContentCs] (Prelude.map bodyRow tbody)
        ]
    ]

dataTableCs :: Miso.Attribute msg
dataTableCs =
  Miso.class_ "mdc-data-table"

dataTableTableCs :: Maybe (Miso.Attribute msg)
dataTableTableCs =
  Just (Miso.class_ "mdc-data-table__table")

dataTableContentCs :: Miso.Attribute msg
dataTableContentCs =
  Miso.class_ "mdc-data-table__content"

ariaLabelAttr :: Config msg -> Maybe (Miso.Attribute msg)
ariaLabelAttr (Config {label = label}) =
  fmap (Miso.textProp "aria-label" << Miso.String.toMisoString) label

-- | Row type
data Row msg = Row {attributes :: [Miso.Attribute msg], nodes :: [Cell msg]}

-- | Row view function
row :: [Miso.Attribute msg] -> [Cell msg] -> Row msg
row attributes nodes =
  Row {attributes = attributes, nodes = nodes}

-- | Attribute to mark a row as selected
-- This has no effect on a header row.
-- Note that this is a list of attributes because it actually sets two HTML
-- attributes at once.
selected :: [Miso.Attribute msg]
selected =
  [ dataTableRowSelectedCs,
    Miso.textProp "aria-selected" "true"
  ]

dataTableRowSelectedCs :: Miso.Attribute msg
dataTableRowSelectedCs =
  Miso.class_ "mdc-data-table__row--selected"

headerRow :: Row msg -> Miso.View msg
headerRow (Row {attributes = attributes, nodes = nodes}) =
  Miso.tr_ (dataTableHeaderRowCs : attributes) (Prelude.map headerCell nodes)

dataTableHeaderRowCs :: Miso.Attribute msg
dataTableHeaderRowCs =
  Miso.class_ "mdc-data-table__header-row"

bodyRow :: Row msg -> Miso.View msg
bodyRow (Row {attributes = attributes, nodes = nodes}) =
  Miso.tr_ (dataTableRowCs : attributes) (Prelude.map bodyCell nodes)

dataTableRowCs :: Miso.Attribute msg
dataTableRowCs =
  Miso.class_ "mdc-data-table__row"

headerCell :: Cell msg -> Miso.View msg
headerCell cell_ =
  case cell_ of
    Cell {numeric = numeric, cellAttributes = attributes, cellNodes = nodes} ->
      Miso.th_
        ( Maybe.mapMaybe
            id
            [ dataTableHeaderCellCs,
              columnHeaderRoleAttr,
              colScopeAttr,
              dataTableHeaderCellNumericCs numeric
            ]
            ++ attributes
        )
        nodes
    CheckboxCell {checkBoxCellAttributes = attributes, config_ = config_} ->
      Miso.th_
        ( Maybe.mapMaybe
            id
            [ dataTableHeaderCellCs,
              columnHeaderRoleAttr,
              colScopeAttr,
              dataTableHeaderCellCheckboxCs
            ]
            ++ attributes
        )
        [ Checkbox.checkbox
            ( case config_ of
                config__@Checkbox.Internal.Config {Checkbox.Internal.additionalAttributes = additionalAttributes} ->
                  config__
                    { Checkbox.Internal.additionalAttributes =
                        Miso.class_ "mdc-data-table__row-checkbox" :
                        additionalAttributes
                    }
            )
        ]

dataTableHeaderCellCs :: Maybe (Miso.Attribute msg)
dataTableHeaderCellCs =
  Just (Miso.class_ "mdc-data-table__header-cell")

columnHeaderRoleAttr :: Maybe (Miso.Attribute msg)
columnHeaderRoleAttr =
  Just (Miso.textProp "role" "columnheader")

colScopeAttr :: Maybe (Miso.Attribute msg)
colScopeAttr =
  Just (Miso.textProp "scope" "col")

dataTableHeaderCellNumericCs :: Bool -> Maybe (Miso.Attribute msg)
dataTableHeaderCellNumericCs numeric =
  if numeric
    then Just (Miso.class_ "mdc-data-table__header-cell--numeric")
    else Nothing

dataTableHeaderCellCheckboxCs :: Maybe (Miso.Attribute msg)
dataTableHeaderCellCheckboxCs =
  Just (Miso.class_ "mdc-data-table__header-cell--checkbox")

bodyCell :: Cell msg -> Miso.View msg
bodyCell cell_ =
  case cell_ of
    Cell {numeric = numeric, cellAttributes = attributes, cellNodes = nodes} ->
      Miso.td_
        ( Maybe.mapMaybe
            id
            [ dataTableCellCs,
              dataTableCellNumericCs numeric
            ]
            ++ attributes
        )
        nodes
    CheckboxCell {checkBoxCellAttributes = attributes, config_ = config_} ->
      Miso.td_
        ( Maybe.mapMaybe
            id
            [ dataTableCellCs,
              dataTableCellCheckboxCs
            ]
            ++ attributes
        )
        [ Checkbox.checkbox
            ( case config_ of
                config__@Checkbox.Internal.Config {Checkbox.Internal.additionalAttributes = additionalAttributes} ->
                  config__
                    { Checkbox.Internal.additionalAttributes =
                        Miso.class_ "mdc-data-table__row-checkbox" :
                        additionalAttributes
                    }
            )
        ]

-- | Cell type
data Cell msg
  = Cell
      { numeric :: Bool,
        cellAttributes :: [Miso.Attribute msg],
        cellNodes :: [Miso.View msg]
      }
  | CheckboxCell
      { config_ :: Checkbox.Config msg,
        checkBoxCellAttributes :: [Miso.Attribute msg]
      }

-- | Data table cell
cell :: [Miso.Attribute msg] -> [Miso.View msg] -> Cell msg
cell attributes nodes =
  Cell {numeric = False, cellAttributes = attributes, cellNodes = nodes}

-- | Numeric data table cell (right-aligned contents)
numericCell :: [Miso.Attribute msg] -> [Miso.View msg] -> Cell msg
numericCell attributes nodes =
  Cell {numeric = True, cellAttributes = attributes, cellNodes = nodes}

-- | Data table cell that contians a checkbox
checkboxCell :: [Miso.Attribute msg] -> Checkbox.Config msg -> Cell msg
checkboxCell attributes config_ =
  CheckboxCell {checkBoxCellAttributes = attributes, config_ = config_}

dataTableCellCs :: Maybe (Miso.Attribute msg)
dataTableCellCs =
  Just (Miso.class_ "mdc-data-table__cell")

dataTableCellNumericCs :: Bool -> Maybe (Miso.Attribute msg)
dataTableCellNumericCs numeric =
  if numeric
    then Just (Miso.class_ "mdc-data-table__cell--numeric")
    else Nothing

dataTableCellCheckboxCs :: Maybe (Miso.Attribute msg)
dataTableCellCheckboxCs =
  Just (Miso.class_ "mdc-data-table__cell--checkbox")
