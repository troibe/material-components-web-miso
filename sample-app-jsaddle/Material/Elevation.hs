{-# LANGUAGE OverloadedStrings #-}

module Material.Elevation
  ( z0,
    z1,
    z2,
    z3,
    z4,
    z5,
    z6,
    z7,
    z8,
    z9,
    z10,
    z11,
    z12,
    z13,
    z14,
    z15,
    z16,
    z17,
    z18,
    z19,
    z20,
    z21,
    z22,
    z23,
    z24,
  )
where

import Miso.Html
import Miso.String

-- | 0dp elevation (no elevation)
z0 :: Attribute msg
z0 =
  z 0

-- | 1dp elevation
z1 :: Attribute msg
z1 =
  z 1

-- | 2dp elevation
z2 :: Attribute msg
z2 =
  z 2

-- | 3dp elevation
z3 :: Attribute msg
z3 =
  z 3

-- | 4dp elevation
z4 :: Attribute msg
z4 =
  z 4

-- | 5dp elevation
z5 :: Attribute msg
z5 =
  z 5

-- | 6dp elevation
z6 :: Attribute msg
z6 =
  z 6

-- | 7dp elevation
z7 :: Attribute msg
z7 =
  z 7

-- | 8dp elevation
z8 :: Attribute msg
z8 =
  z 8

-- | 9dp elevation
z9 :: Attribute msg
z9 =
  z 9

-- | 10dp elevation
z10 :: Attribute msg
z10 =
  z 10

-- | 11dp elevation
z11 :: Attribute msg
z11 =
  z 11

-- | 12dp elevation
z12 :: Attribute msg
z12 =
  z 12

-- | 13dp elevation
z13 :: Attribute msg
z13 =
  z 13

-- | 14dp elevation
z14 :: Attribute msg
z14 =
  z 14

-- | 15dp elevation
z15 :: Attribute msg
z15 =
  z 15

-- | 16dp elevation
z16 :: Attribute msg
z16 =
  z 16

-- | 17dp elevation
z17 :: Attribute msg
z17 =
  z 17

-- | 18dp elevation
z18 :: Attribute msg
z18 =
  z 18

-- | 19dp elevation
z19 :: Attribute msg
z19 =
  z 19

-- | 20dp elevation
z20 :: Attribute msg
z20 =
  z 20

-- | 21dp elevation
z21 :: Attribute msg
z21 =
  z 21

-- | 22dp elevation
z22 :: Attribute msg
z22 =
  z 22

-- | 23dp elevation
z23 :: Attribute msg
z23 =
  z 23

-- | 24dp elevation
z24 :: Attribute msg
z24 =
  z 24

z :: Int -> Attribute msg
z n =
  class_ (toMisoString ("mdc-elevation--z" ++ show n))
