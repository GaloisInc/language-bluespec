-- Unfortunately, the (<>) function from the Prelude has a different fixity than
-- the (<>) function from the pretty library, so we avoid re-exporting (<>) from
-- the Prelude to avoid fixity clashes.
module Language.Bluespec.Prelude
  ( module Prelude
  ) where

import Prelude hiding ((<>))
