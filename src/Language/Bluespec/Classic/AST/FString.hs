-- This corresponds to src/comp/FStringCompat.hs in bsc.
module Language.Bluespec.Classic.AST.FString
  ( FString
  , getFString
  , mkFString
  , tmpFString
  ) where

import Data.String (IsString(..))
import qualified Data.Text as T

import Language.Bluespec.Classic.AST.Pretty
import Language.Bluespec.Classic.AST.SString
import Language.Bluespec.Prelude

-- wrapper to make SStrings look like FStrings

newtype FString = FString SString
  deriving (Eq, Ord)

instance IsString FString where
  fromString = FString . T.pack

instance Show FString where
    show (FString s) = show s

instance PPrint FString where
    pPrint _ _ x = text (show x)

getFString :: FString -> String
getFString = toString

mkFString :: String -> FString
mkFString s = fromString s

tmpFString :: Int -> String -> FString
tmpFString _ = fromString

toString :: FString -> String
toString (FString s) = T.unpack s
