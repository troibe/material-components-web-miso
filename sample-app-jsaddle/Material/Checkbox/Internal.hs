module Material.Checkbox.Internal (Config (..), State (..)) where

import Miso

data Config msg = Config
  { state :: Maybe State,
    disabled :: Bool,
    additionalAttributes :: [Attribute msg],
    onChange :: Maybe msg,
    touch :: Bool
  }

data State
  = Unchecked
  | Checked
  | Indeterminate
  deriving (Eq)
