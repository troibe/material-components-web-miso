{-# LANGUAGE OverloadedStrings #-}

module Material.Tab.Internal
  ( Config (..),
    Content (..),
    Icon (..),
    Tab (..),
  )
where

import Data.Void as Void
import qualified Miso

data Config msg = Config
  { active :: Bool,
    additionalAttributes :: [Miso.Attribute msg],
    onClick :: Maybe msg,
    content :: Content
  }

data Content = Content
  { label :: String,
    icon :: Maybe Icon
  }

data Tab msg
  = Tab (Config msg)

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
