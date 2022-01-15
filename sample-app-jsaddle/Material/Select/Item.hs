{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Material.Select.Item
  ( Config,
    config,
    InitialConfig,
    setDisabled,
    setAttributes,
    SelectItem,
    selectItem,
  )
where

import Material.Select.Item.Internal (Config (..), SelectItem (..))
import qualified Miso

-- | Configuration of a select item

-- data Config a msg =
--    Material.Select.Item.Internal.Config a msg

data InitialConfig a = InitialConfig
  {value :: a}

-- | Default configuration of a select item
config :: a -> Config a msg
config value =
  Config
    { Material.Select.Item.Internal.value = value,
      disabled = False,
      additionalAttributes = []
    }

-- | Specify whether a select item should be disabled
--
-- Disabled select items cannot be interacted with and have not visual interaction
-- effect.
setDisabled :: Bool -> Config a msg -> Config a msg
setDisabled disabled config_ =
  config_ {disabled = disabled}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config a msg -> Config a msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Select item type

-- data SelectItem a msg =
--    Material.Select.Item.Internal.SelectItem a msg

-- | Select item constructor
selectItem :: Config a msg -> [Miso.View msg] -> SelectItem a msg
selectItem =
  SelectItem
