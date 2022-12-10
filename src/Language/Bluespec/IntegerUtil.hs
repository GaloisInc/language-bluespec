-- This corresponds to src/comp/IntegerUtil.hs in bsc.
module Language.Bluespec.IntegerUtil
  ( integerFormat
  , integerFormatPref
  , integerToString
  ) where

import Language.Bluespec.Prelude

integerFormat :: Integer -> Integer -> Integer -> String
integerFormat width base value =
        if value < 0 then
                '-' : integerFormat width base (-value)
        else
                let s = integerToString (fromInteger base) value
                    l = length s
                    w = fromInteger width
                    pad = if l < w then replicate (w-l) '0' else ""
                in  pad ++ s

integerFormatPref :: Integer -> Integer -> Integer -> String
integerFormatPref width  2 value = "0b" ++ integerFormat width  2 value
integerFormatPref width  8 value = "0o" ++ integerFormat width  8 value
integerFormatPref width 10 value =         integerFormat width 10 value
integerFormatPref width 16 value = "0x" ++ integerFormat width 16 value
-- otherwise, use decimal
integerFormatPref width  _ value = integerFormatPref width 10 value

integerToString :: Int -> Integer -> String
integerToString b i | b < 2      =  error "integerToString: base must be >= 2"
                    | i < 0      =  '-' : showIntBase (toInteger b) (negate i) ""
                    | otherwise  =  showIntBase (toInteger b) i ""

-- mostly duplicates the function in the Prelude from the Haskell 98 Report
showIntBase :: Integer -> Integer -> String -> String
showIntBase b n r | n < 0 = error "Numeric.showInt: can't show negative numbers"
                  | otherwise =
                      let (n',d) = quotRem n b
                          r'     = digit d : r
                          digit d' | d' < 10     =  toEnum (fromEnum '0' + fromIntegral d')
                                   | otherwise   =  toEnum (fromEnum 'A' + fromIntegral d' - 10)
                      in  if n' == 0 then r' else showIntBase b n' r'
