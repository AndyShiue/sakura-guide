module TOCMaker where

import Data.Maybe
import Data.Either.Unwrap
import Control.Monad

import Text.Regex.Posix

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator

data Header = Header
              String -- title
              Int -- header level, i.e. the `x` in `<hx>` tags in HTML, or the number of trailing hashes before title in markdown

instance Show Header where
  show (Header title lv) = "(Header " ++ title ++ " " ++ show lv ++ ")"

main :: IO ()
main = do content <- readFile "../落櫻散華抄.md"
          let ls = lines content
          let headerStrs = filter (\str -> str =~ "#+.*") ls
          writeFile "../TOC.md" . unlines . map headerToLink . map (\h -> Header (fromJust . trimHeader $ h) (trailingHashCount h)) $ headerStrs

headerToLink :: Header -> String
headerToLink (Header title lv) = replicate (2*lv) ' ' ++ "- [" ++ title ++ "](" ++ "https://github.com/AndyShiue/sakura-guide/blob/master/sakura-guide.md#" ++ title ++ ")"

trailingHashCount :: String -> Int
trailingHashCount = length . takeWhile (\c -> c == '#')

trimHeader :: String -> Maybe String
trimHeader = fmap reverse . join . fmap removeTrailingHashesAndSpaces . fmap reverse . removeTrailingHashesAndSpaces

removeTrailingHashesAndSpaces :: String -> Maybe String
removeTrailingHashesAndSpaces str = case removeTrailingHashesAndSpaces' str of
                                         Left err -> Nothing
                                         Right val -> Just val
  where removeTrailingHashesAndSpaces' str = parse (do many $ oneOf "#"
                                                       spaces
                                                       remained <- many anyToken
                                                       return remained) "" str
