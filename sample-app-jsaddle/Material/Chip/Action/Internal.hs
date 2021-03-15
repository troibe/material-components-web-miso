{-# LANGUAGE OverloadedStrings #-}

module Material.Chip.Action.Internal (Chip (..), Config (..), Icon (..)) where

import qualified Data.Void as Void
import qualified Miso

data Config msg = Config
  { icon :: Maybe Icon,
    additionalAttributes :: [Miso.Attribute msg],
    onClick :: Maybe msg
  }

data Chip msg
  = Chip (Config msg) String

data Icon
  = Icon
      { node :: [Miso.Attribute Void.Void] -> [Miso.View Void.Void] -> Miso.View Void.Void,
        attributes :: [Miso.Attribute Void.Void],
        nodes :: [Miso.View Void.Void]
      }
  | SvgIcon
      { node :: [Miso.Attribute Void.Void] -> [Miso.View Void.Void] -> Miso.View Void.Void,
        attributes :: [Miso.Attribute Void.Void],
        nodes :: [Miso.View Void.Void]
      }
