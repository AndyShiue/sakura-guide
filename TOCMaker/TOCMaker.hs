module Main where

import System.IO

import Data.Maybe

import Text.Regex.Posix

import Text.Parsec

data Header = Header
              String -- title
              Int -- header level, i.e. the `x` in `<hx>` tags in HTML, or the number of trailing hashes before title in markdown

instance Show Header where
  show (Header title lv) = "(Header " ++ title ++ " " ++ show lv ++ ")"

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStr "Input the path of your markdown file: "
          input <- getLine
          putStr "Input your webpage: "
          webpage <- getLine
          content <- readFile input
          putStr "Output to: "
          out <- getLine
          let ls = lines content
          let headerStrs = filter (\str -> str =~ "#+.*") ls
          writeFile out . unlines . map (headerToLink webpage) . map (\h -> Header (fromJust . trimHeader $ h) (leadingHashCount h)) $ headerStrs

headerToLink :: String -> Header -> String
headerToLink webpage (Header title lv) = replicate (2 * lv) ' ' ++ "- [" ++ title ++ "](" ++ webpage ++ "#" ++ map spaceToDash title ++ ")"

spaceToDash :: Char -> Char
spaceToDash c = if c == ' ' then '-' else c

leadingHashCount :: String -> Int
leadingHashCount = length . takeWhile (\c -> c == '#')

trimHeader :: String -> Maybe String
trimHeader header = removeLeadingHashesAndSpaces header >>= return . reverse >>= removeLeadingHashesAndSpaces >>= return . reverse

removeLeadingHashesAndSpaces :: String -> Maybe String
removeLeadingHashesAndSpaces str = case removeLeadingHashesAndSpaces' str of
                                       Left _ -> Nothing
                                       Right val -> Just val
  where removeLeadingHashesAndSpaces' str = parse (do many $ oneOf "#"
                                                      spaces
                                                      remained <- many anyToken
                                                      return remained) "" str
