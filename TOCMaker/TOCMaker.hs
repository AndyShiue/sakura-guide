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
          content <- readFile input
          putStr "Output to: "
          out <- getLine
          let ls = lines content
          let headerStrs = filter (\str -> str =~ "#+.*") ls
          writeFile out . unlines . map headerToLink . map (\h -> Header (fromJust . trimHeader $ h) (trailingHashCount h)) $ headerStrs

contentUrl :: String
contentUrl = "https://github.com/AndyShiue/sakura-guide/blob/master/sakura-guide.md"

headerToLink :: Header -> String
headerToLink (Header title lv) = replicate (2 * lv) ' ' ++ "- [" ++ title ++ "](" ++ contentUrl ++ "#" ++ title ++ ")"

trailingHashCount :: String -> Int
trailingHashCount = length . takeWhile (\c -> c == '#')

trimHeader :: String -> Maybe String
trimHeader header = removeTrailingHashesAndSpaces header >>= return . reverse >>= removeTrailingHashesAndSpaces >>= return . reverse

removeTrailingHashesAndSpaces :: String -> Maybe String
removeTrailingHashesAndSpaces str = case removeTrailingHashesAndSpaces' str of
                                         Left _ -> Nothing
                                         Right val -> Just val
  where removeTrailingHashesAndSpaces' str = parse (do many $ oneOf "#"
                                                       spaces
                                                       remained <- many anyToken
                                                       return remained) "" str
