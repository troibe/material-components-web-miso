{-# LANGUAGE OverloadedStrings #-}

module Material.Typography
  ( headline1,
    headline2,
    headline3,
    headline4,
    headline5,
    headline6,
    body1,
    body2,
    subtitle1,
    subtitle2,
    caption,
    button,
    overline,
    typography,
  )
where

import Miso

-- | Sets the font to Roboto
typography :: Attribute msg
typography =
  class_ "mdc-typography"

-- | Sets font properties as Headline 1
headline1 :: Attribute msg
headline1 =
  class_ "mdc-typography--headline1"

-- | Sets font properties as Headline 2
headline2 :: Attribute msg
headline2 =
  class_ "mdc-typography--headline2"

-- | Sets font properties as Headline 3
headline3 :: Attribute msg
headline3 =
  class_ "mdc-typography--headline3"

-- | Sets font properties as Headline 4
headline4 :: Attribute msg
headline4 =
  class_ "mdc-typography--headline4"

-- | Sets font properties as Headline 5
headline5 :: Attribute msg
headline5 =
  class_ "mdc-typography--headline5"

-- | Sets font properties as Headline 6
headline6 :: Attribute msg
headline6 =
  class_ "mdc-typography--headline6"

-- | Sets font properties as Subtitle 1
subtitle1 :: Attribute msg
subtitle1 =
  class_ "mdc-typography--subtitle1"

-- | Sets font properties as Subtitle 2
subtitle2 :: Attribute msg
subtitle2 =
  class_ "mdc-typography--subtitle2"

-- | Sets font properties as Body 1
body1 :: Attribute msg
body1 =
  class_ "mdc-typography--body1"

-- | Sets font properties as Body 2
body2 :: Attribute msg
body2 =
  class_ "mdc-typography--body2"

-- | Sets font properties as Caption
caption :: Attribute msg
caption =
  class_ "mdc-typography--caption"

-- | Sets font properties as Button
button :: Attribute msg
button =
  class_ "mdc-typography--button"

-- | Sets font properties as Overline
overline :: Attribute msg
overline =
  class_ "mdc-typography--overline"
