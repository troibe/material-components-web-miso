{-# LANGUAGE OverloadedStrings #-}

module Material.Theme
  ( primary,
    secondary,
    background,
    onPrimary,
    onSecondary,
    onSurface,
    primaryBg,
    secondaryBg,
    surface,
    textPrimaryOnBackground,
    textSecondaryOnBackground,
    textHintOnBackground,
    textDisabledOnBackground,
    textIconOnBackground,
    textPrimaryOnLight,
    textSecondaryOnLight,
    textHintOnLight,
    textDisabledOnLight,
    textIconOnLight,
    textPrimaryOnDark,
    textSecondaryOnDark,
    textHintOnDark,
    textDisabledOnDark,
    textIconOnDark,
  )
where

import Miso.Html

-- | Sets the text color to the theme primary color
primary :: Attribute msg
primary =
  class_ "mdc-theme--primary"

-- | Sets the text color to the theme secondary color
secondary :: Attribute msg
secondary =
  class_ "mdc-theme--secondary"

-- | Sets the background color to the theme background color
background :: Attribute msg
background =
  class_ "mdc-theme--background"

-- | Sets the surface color to the theme surface color
surface :: Attribute msg
surface =
  class_ "mdc-theme--surface"

-- | Sets the text color to the theme on-primary color
-- The theme's on-primary color is a text color that works best on a primary color
-- background.
onPrimary :: Attribute msg
onPrimary =
  class_ "mdc-theme--on-primary"

-- | Sets the text color to the theme on-secondary color
-- The theme's on-secondary color is a text color that works best on a secondary
-- color background.
onSecondary :: Attribute msg
onSecondary =
  class_ "mdc-theme--on-secondary"

-- | Sets the text color to the theme on-surface color
-- The theme's on-surface color is a text color that works best on a surface
-- color background.
onSurface :: Attribute msg
onSurface =
  class_ "mdc-theme--on-surface"

-- | Sets the background color to the theme primary color
primaryBg :: Attribute msg
primaryBg =
  class_ "mdc-theme--primary-bg"

-- | Sets the background color to the theme secondary color
secondaryBg :: Attribute msg
secondaryBg =
  class_ "mdc-theme--secondary-bg"

-- | Sets text to a suitable color for the primary text style on top of light
-- background
textPrimaryOnLight :: Attribute msg
textPrimaryOnLight =
  class_ "mdc-theme--text-primary-on-light"

-- | Sets text to a suitable color for the secondary text style on top of light
-- background
textSecondaryOnLight :: Attribute msg
textSecondaryOnLight =
  class_ "mdc-theme--text-secondary-on-light"

-- | Sets text to a suitable color for the hint text style on top of light
-- background
textHintOnLight :: Attribute msg
textHintOnLight =
  class_ "mdc-theme--text-hint-on-light"

-- | Sets text to a suitable color for the disabled text style on top of light
-- background
textDisabledOnLight :: Attribute msg
textDisabledOnLight =
  class_ "mdc-theme--text-disabled-on-light"

-- | Sets text to a suitable color for the icon text style on top of light
-- background
textIconOnLight :: Attribute msg
textIconOnLight =
  class_ "mdc-theme--text-icon-on-light"

-- | Sets text to a suitable color for the primary text style on top of dark
-- background
textPrimaryOnDark :: Attribute msg
textPrimaryOnDark =
  class_ "mdc-theme--text-primary-on-dark"

-- | Sets text to a suitable color for the secondary text style on top of dark
-- background
textSecondaryOnDark :: Attribute msg
textSecondaryOnDark =
  class_ "mdc-theme--text-secondary-on-dark"

-- | Sets text to a suitable color for the hint text style on top of dark
-- background
textHintOnDark :: Attribute msg
textHintOnDark =
  class_ "mdc-theme--text-hint-on-dark"

-- | Sets text to a suitable color for the disabled text style on top of dark
-- background
textDisabledOnDark :: Attribute msg
textDisabledOnDark =
  class_ "mdc-theme--text-disabled-on-dark"

-- | Sets text to a suitable color for the icon text style on top of dark
-- background
textIconOnDark :: Attribute msg
textIconOnDark =
  class_ "mdc-theme--text-icon-on-dark"

-- | Sets text to a suitable color for the primary text style on top of the
-- background color
textPrimaryOnBackground :: Attribute msg
textPrimaryOnBackground =
  class_ "mdc-theme--text-primary-on-background"

-- | Sets text to a suitable color for the secondary text style on top of the
-- background color
textSecondaryOnBackground :: Attribute msg
textSecondaryOnBackground =
  class_ "mdc-theme--text-secondary-on-background"

-- | Sets text to a suitable color for the hint text style on top of the
-- background color
textHintOnBackground :: Attribute msg
textHintOnBackground =
  class_ "mdc-theme--text-hint-on-background"

-- | Sets text to a suitable color for the disabled text style on top of the
-- background color
textDisabledOnBackground :: Attribute msg
textDisabledOnBackground =
  class_ "mdc-theme--text-disabled-on-background"

-- | Sets text to a suitable color for the icon text style on top of the
-- background color
textIconOnBackground :: Attribute msg
textIconOnBackground =
  class_ "mdc-theme--text-icon-on-background"
