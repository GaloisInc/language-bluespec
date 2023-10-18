{-# LANGUAGE FlexibleInstances #-}
-- This corresponds to src/comp/Util.hs in bsc.
module Language.Bluespec.Util
  ( dbgLevel

  , unconsOrErr

  , fromJustOrErr

  , quote
  , doubleQuote
  , ToString(..)
  ) where

import Data.Char (intToDigit)

import Language.Bluespec.Prelude

-- =====
-- Configurable traces
-- (currently only used in Id and XRef)

dbgLevel :: Int
dbgLevel = -1

-- =====
-- List utilities

unconsOrErr :: String -> [elem] -> (elem, [elem])
unconsOrErr _   (elt:rest) = (elt, rest)
unconsOrErr err []         = error err

-- =====
-- List/Maybe utilities

fromJustOrErr :: String -> Maybe value -> value
fromJustOrErr err Nothing  = error err
fromJustOrErr _   (Just v) = v

-- =====
-- String utilities

quote :: String -> String
quote s = "`" ++ s ++ "'"

doubleQuote :: String -> String
doubleQuote s = "\"" ++ s ++ "\""

-- =====
-- ToString class

class ToString a where
    to_string :: a -> String
    itos :: a -> String

instance ToString Int where
    itos a = show a
    to_string a = error ("to_string applied to nonsymbol (Int) "
                            ++ show a)

instance ToString Integer where
    itos a = show a
    to_string a = error ("to_string applied to nonsymbol (Integer) "
                            ++ show a)

instance ToString Char where
    itos a = show a
    to_string a = case a of
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        '\a' -> "\\a"
        '\\' -> "\\\\"
        '"' -> "\\\""        -- backslash double-quote
        _ | n < 0 ||
            n > 0x100 -> error $ "quoting a character value " ++ show n
        _ | n < 0x20 || n >= 0x7F ->
            [ '\\', intToDigit highest, intToDigit middle, intToDigit lowest ]
        _ -> [a]
      where
        n = fromEnum a
        (top2, lowest) = quotRem n 8
        (highest, middle) = quotRem top2 8

instance ToString String where
    itos a = error $ "itos applied to string " ++ show a
    to_string a = concatMap to_string a
