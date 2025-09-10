{-|
Tokenizer Usage Instructions
===========================

Build:
  ghc -package text -o tokenizer tokenizer.hs

Run on a file:
  ./tokenizer names.txt

Save output to a file:
  ./tokenizer names.txt > tokenized_output.tsv

The output is a TSV file with columns: line, column, token type, lexeme.
-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Main
-- Description : Simple, reasonably robust tokenizer for plain text.
--
-- What it does
--   • Splits input into tokens with line/column positions.
--   • Token kinds: Word, Number, Punct, Symbol.
--   • Words can include internal apostrophes/hyphens (e.g., don't, well-being).
--   • Numbers: integers or decimals (e.g., 42, 3.1415).
--   • Whitespace is skipped but line/column are tracked.
--
-- How to build
--   ghc -O2 Main.hs
--   # or with cabal/stack as you prefer (requires the 'text' package).
--
-- How to run
--   ./Main path/to/file.txt
--   # or pipe from stdin
--   cat file.txt | ./Main
--
-- Output format (TSV)
--   line\tcol\tkind\tlexeme
--
-- You can adapt TOKEN RULES near the bottom if you want different behavior.

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import System.Environment (getArgs)
import Data.Char (isSpace, isDigit, isLetter)
import Data.Maybe (fromMaybe)

-- Token types

data TokKind = WordTok | NumberTok | PunctTok | SymbolTok
  deriving (Eq)

instance Show TokKind where
  show WordTok   = "WORD"
  show NumberTok = "NUMBER"
  show PunctTok  = "PUNCT"
  show SymbolTok = "SYMBOL"

data Token = Token
  { tKind :: TokKind
  , tLex  :: T.Text
  , tLine :: !Int
  , tCol  :: !Int
  } deriving (Eq)

instance Show Token where
  -- TSV: line\tcol\tkind\tlexeme
  show (Token k lx ln cl) = show ln ++ "\t" ++ show cl ++ "\t" ++ show k ++ "\t" ++ escapeTabs (T.unpack lx)
    where
      escapeTabs [] = []
      escapeTabs ('\t':xs) = '\\':'t':escapeTabs xs
      escapeTabs ('\n':xs) = '\\':'n':escapeTabs xs
      escapeTabs (x:xs) = x : escapeTabs xs

-- Scanner entry point

tokenize :: T.Text -> [Token]
tokenize = go 1 1
  where
    go :: Int -> Int -> T.Text -> [Token]
    go ln col txt = case T.uncons txt of
      Nothing         -> []
      Just (c, rest)
        -> if c == '\n' then go (ln + 1) 1 rest
           else if isSpace c then let (spaces, r) = T.span (\ch -> ch /= '\n' && isSpace ch) rest; n = 1 + T.length spaces
                                 in go ln (col + n) r
           else if isLetter c then
             let (lexeme, r) = takeWord (T.cons c rest)
                 tok = Token WordTok lexeme ln col
             in tok : go ln (col + T.length lexeme) r
           else if isDigit c then
             let (lexeme, r) = takeNumber (T.cons c rest)
                 tok = Token NumberTok lexeme ln col
             in tok : go ln (col + T.length lexeme) r
           else if isPunct c then let tok = Token PunctTok (T.singleton c) ln col
                                   in tok : go ln (col + 1) rest
           else if isSymbol c then let tok = Token SymbolTok (T.singleton c) ln col
                                    in tok : go ln (col + 1) rest
           else let tok = Token SymbolTok (T.singleton c) ln col
                in tok : go ln (col + 1) rest

-- TOKEN RULES ---------------------------------------------------------------
-- You can tailor these to your needs.

-- Words: letters, with optional internal '-' or '\'' characters.
-- We allow digits inside words only if you want alnum words; here we keep digits out
-- so things like "R2D2" will be split as WORD "R" NUMBER "2" WORD "D" NUMBER "2".
-- If you prefer alnum words, set allowDigitInWord = True.

allowDigitInWord :: Bool
allowDigitInWord = False

takeWord :: T.Text -> (T.Text, T.Text)
takeWord t0 =
  let (chunk, rest) = spanWithIdx 0 t0 step
      -- disallow trailing '-' or '\'' at the end of a word
      trimmed = T.dropEnd (countTrailing (\x -> x == '-' || x == '\'') chunk) chunk
      dropped = T.drop (T.length chunk - T.length trimmed) rest
  in (trimmed, dropped)
  where
    step i ch
      | isLetter ch = True
      | ch == '-' || ch == '\'' = i > 0  -- only internal hyphen/apostrophe
      | allowDigitInWord && isDigit ch = True
      | otherwise = False

-- Numbers: digits with optional single decimal point followed by digits.
-- Examples: 42, 3.14, 007

takeNumber :: T.Text -> (T.Text, T.Text)
takeNumber t0 =
  let (intPart, r1) = T.span isDigit t0
  in case T.uncons r1 of
       Just ('.', r2) ->
         let (frac, r3) = T.span isDigit r2
         in if T.null frac
              then (intPart, r1)   -- lone dot → stop before '.'
              else (intPart <> "." <> frac, r3)
       _ -> (intPart, r1)

-- Classification helpers ----------------------------------------------------

-- Basic sets for punctuation/symbols. Adjust as needed.

punctSet :: [Char]
punctSet = ",[].,;:!?" -- common sentence punctuation

symbolSet :: [Char]
symbolSet = "()[]{}\"`~@#$%^&*_+=|\\/<>" -- misc symbols (includes quotes, slashes, etc.)

isPunct :: Char -> Bool
isPunct c = c `elem` punctSet || c `elem` ['–','—'] -- en/em dashes as punctuation

isSymbol :: Char -> Bool
isSymbol c = c `elem` symbolSet

-- Utility: span with index-aware predicate

spanWithIdx :: Int -> T.Text -> (Int -> Char -> Bool) -> (T.Text, T.Text)
spanWithIdx start txt p = go start 0 txt
  where
    go i n t = case T.uncons t of
      Just (ch, r) | p (i + n) ch -> go i (n + 1) r
      _                           -> (T.take n txt, T.drop n txt)

-- Count trailing chars that match a predicate

countTrailing :: (Char -> Bool) -> T.Text -> Int
countTrailing p = T.length . T.takeWhile p . T.reverse

-- Main ----------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    (fp:_) -> TIO.readFile fp
    _      -> TIO.getContents
  mapM_ (putStrLn . show) (tokenize input)
