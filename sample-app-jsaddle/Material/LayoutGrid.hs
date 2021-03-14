{-# LANGUAGE OverloadedStrings #-}

module Material.LayoutGrid
  ( layoutGrid,
    cell,
    inner,
    span1,
    span2,
    span3,
    span4,
    span5,
    span6,
    span7,
    span8,
    span9,
    span10,
    span11,
    span12,
    alignTop,
    alignMiddle,
    alignBottom,
    alignLeft,
    alignRight,
    span1Desktop,
    span2Desktop,
    span3Desktop,
    span4Desktop,
    span5Desktop,
    span6Desktop,
    span7Desktop,
    span8Desktop,
    span9Desktop,
    span10Desktop,
    span11Desktop,
    span12Desktop,
    span1Tablet,
    span2Tablet,
    span3Tablet,
    span4Tablet,
    span5Tablet,
    span6Tablet,
    span7Tablet,
    span8Tablet,
    span1Phone,
    span2Phone,
    span3Phone,
    span4Phone,
  )
where

import qualified Data.Map as Map
import qualified Miso
import qualified Miso.String
import Prelude hiding (span)

-- | Layout grid view function
layoutGrid :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg
layoutGrid attributes nodes =
  Miso.nodeHtml
    "mdc-layout-grid"
    (Miso.class_ "mdc-layout-grid" : Miso.style_ (Map.singleton "display" "block") : attributes)
    nodes

-- | Layout grid cell view function
cell :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg
cell attributes nodes =
  Miso.div_ (Miso.class_ "mdc-layout-grid__cell" : attributes) nodes

-- | Layout grid inner view function
-- It is mandatory to wrap `cell`s within `inner`. This has to do with nesting
-- layout grids, but it is mandatory for flat layout grids as well.
inner :: [Miso.Attribute msg] -> [Miso.View msg] -> Miso.View msg
inner attributes nodes =
  Miso.div_ (Miso.class_ "mdc-layout-grid__inner" : attributes) nodes

-- | Aligns a cell to the bottom
alignBottom :: Miso.Attribute msg
alignBottom =
  Miso.class_ "mdc-layout-grid__cell--align-bottom"

-- | Aligns the layout grid to the left
alignLeft :: Miso.Attribute msg
alignLeft =
  Miso.class_ "mdc-layout-grid--align-left"

-- | Aligns the layout grid to the right
alignRight :: Miso.Attribute msg
alignRight =
  Miso.class_ "mdc-layout-grid--align-right"

-- | Aligns a cell to the middle
alignMiddle :: Miso.Attribute msg
alignMiddle =
  Miso.class_ "mdc-layout-grid__cell--align-middle"

-- | Aligns a cell to the top
alignTop :: Miso.Attribute msg
alignTop =
  Miso.class_ "mdc-layout-grid__cell--align-top"

span :: Int -> Miso.Attribute msg
span n =
  Miso.class_ (Miso.String.toMisoString ("mdc-layout-grid__cell--span-" ++ show n))

spanDesktop :: Int -> Miso.Attribute msg
spanDesktop n =
  Miso.class_ (Miso.String.toMisoString ("mdc-layout-grid__cell--span-" ++ show n ++ "-desktop"))

spanTablet :: Int -> Miso.Attribute msg
spanTablet n =
  Miso.class_ (Miso.String.toMisoString ("mdc-layout-grid__cell--span-" ++ show n ++ "-tablet"))

spanPhone :: Int -> Miso.Attribute msg
spanPhone n =
  Miso.class_ (Miso.String.toMisoString ("mdc-layout-grid__cell--span-" ++ show n ++ "-phone"))

-- | Change a cell to span one column
span1 :: Miso.Attribute msg
span1 =
  span 1

-- | Change a cell to span two columns
span2 :: Miso.Attribute msg
span2 =
  span 2

-- | Change a cell to span three columns
span3 :: Miso.Attribute msg
span3 =
  span 3

-- | Change a cell to span four columns
span4 :: Miso.Attribute msg
span4 =
  span 4

-- | Change a cell to span five columns
span5 :: Miso.Attribute msg
span5 =
  span 5

-- | Change a cell to span six columns
span6 :: Miso.Attribute msg
span6 =
  span 6

-- | Change a cell to span seven columns
span7 :: Miso.Attribute msg
span7 =
  span 7

-- | Change a cell to span eight columns
span8 :: Miso.Attribute msg
span8 =
  span 8

-- | Change a cell to span nine columns
span9 :: Miso.Attribute msg
span9 =
  span 9

-- | Change a cell to span ten columns
span10 :: Miso.Attribute msg
span10 =
  span 10

-- | Change a cell to span eleven columns
span11 :: Miso.Attribute msg
span11 =
  span 11

-- | Change a cell to span twelve columns
span12 :: Miso.Attribute msg
span12 =
  span 12

-- | Change a cell to span one column (desktop only)
span1Desktop :: Miso.Attribute msg
span1Desktop =
  spanDesktop 1

-- | Change a cell to span two columns (desktop only)
span2Desktop :: Miso.Attribute msg
span2Desktop =
  spanDesktop 2

-- | Change a cell to span three columns (desktop only)
span3Desktop :: Miso.Attribute msg
span3Desktop =
  spanDesktop 3

-- | Change a cell to span four columns (desktop only)
span4Desktop :: Miso.Attribute msg
span4Desktop =
  spanDesktop 4

-- | Change a cell to span five columns (desktop only)
span5Desktop :: Miso.Attribute msg
span5Desktop =
  spanDesktop 5

-- | Change a cell to span six columns (desktop only)
span6Desktop :: Miso.Attribute msg
span6Desktop =
  spanDesktop 6

-- | Change a cell to span seven columns (desktop only)
span7Desktop :: Miso.Attribute msg
span7Desktop =
  spanDesktop 7

-- | Change a cell to span eight columns (desktop only)
span8Desktop :: Miso.Attribute msg
span8Desktop =
  spanDesktop 8

-- | Change a cell to span nine columns (desktop only)
span9Desktop :: Miso.Attribute msg
span9Desktop =
  spanDesktop 9

-- | Change a cell to span ten columns (desktop only)
span10Desktop :: Miso.Attribute msg
span10Desktop =
  spanDesktop 10

-- | Change a cell to span eleven columns (desktop only)
span11Desktop :: Miso.Attribute msg
span11Desktop =
  spanDesktop 11

-- | Change a cell to span twelve columns (desktop only)
span12Desktop :: Miso.Attribute msg
span12Desktop =
  spanDesktop 12

-- | Change a cell to span one column (tablet only)
span1Tablet :: Miso.Attribute msg
span1Tablet =
  spanTablet 1

-- | Change a cell to span two columns (tablet only)
span2Tablet :: Miso.Attribute msg
span2Tablet =
  spanTablet 2

-- | Change a cell to span three columns (tablet only)
span3Tablet :: Miso.Attribute msg
span3Tablet =
  spanTablet 3

-- | Change a cell to span four columns (tablet only)
span4Tablet :: Miso.Attribute msg
span4Tablet =
  spanTablet 4

-- | Change a cell to span five columns (tablet only)
span5Tablet :: Miso.Attribute msg
span5Tablet =
  spanTablet 5

-- | Change a cell to span six columns (tablet only)
span6Tablet :: Miso.Attribute msg
span6Tablet =
  spanTablet 6

-- | Change a cell to span seven columns (tablet only)
span7Tablet :: Miso.Attribute msg
span7Tablet =
  spanTablet 7

-- | Change a cell to span eight columns (tablet only)
span8Tablet :: Miso.Attribute msg
span8Tablet =
  spanTablet 8

-- | Change a cell to span one column (phone only)
span1Phone :: Miso.Attribute msg
span1Phone =
  spanPhone 1

-- | Change a cell to span two columns (phone only)
span2Phone :: Miso.Attribute msg
span2Phone =
  spanPhone 2

-- | Change a cell to span three columns (phone only)
span3Phone :: Miso.Attribute msg
span3Phone =
  spanPhone 3

-- | Change a cell to span four columns (phone only)
span4Phone :: Miso.Attribute msg
span4Phone =
  spanPhone 4
