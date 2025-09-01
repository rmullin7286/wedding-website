module Wedding.Html.Attributes 
  ( dataBsToggle_
  , dataBsTarget_
  , ariaControls_
  , ariaExpanded_
  , ariaLabel_
  ) where

import Data.Text (Text)
import Lucid.Base (Attribute, makeAttribute)

-- Bootstrap data attributes
dataBsToggle_ :: Text -> Attribute
dataBsToggle_ = makeAttribute "data-bs-toggle"

dataBsTarget_ :: Text -> Attribute  
dataBsTarget_ = makeAttribute "data-bs-target"

-- ARIA attributes for accessibility
ariaControls_ :: Text -> Attribute
ariaControls_ = makeAttribute "aria-controls"

ariaExpanded_ :: Text -> Attribute
ariaExpanded_ = makeAttribute "aria-expanded"

ariaLabel_ :: Text -> Attribute
ariaLabel_ = makeAttribute "aria-label"