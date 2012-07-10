module Publication 
( PubType (..)
, Publication (..)
, parsePublications
) where

import Text.BibTeX.Entry
import Text.BibTeX.Parse
import Text.Parsec.Prim
import Data.Char (toLower)

data PubType = Conference 
             | Journal 
             | Book 
             | PhDThesis 
             | MastersThesis 
             | Misc
             deriving (Eq,Show)

data Publication = Publication 
  { pubType     :: PubType
  , title       :: String
  , authors     :: [String]
  , year        :: String
  , publishedIn :: String
  , abstract    :: String
  , link        :: String
  , bibtex      :: T
  } deriving (Show)

bibPubType :: T -> PubType
bibPubType e = case map toLower (entryType e) of
  "inproceedings" -> Conference
  "article"       -> Journal
  "book"          -> Book
  "phdthesis"     -> PhDThesis
  "mastersthesis" -> MastersThesis
  _               -> Misc

bibField :: String -> T -> String
bibField k e = case lookup k (fields (lowerCaseFieldNames e)) of
  Just v -> v
  Nothing -> error $ "No field named '" ++ k ++ "'"

bibAuthors :: T -> [String]
bibAuthors = map flipName . splitAuthorList . bibField "author"

bibTitle :: T -> String
bibTitle = bibField "title"

bibYear :: T -> String
bibYear = bibField "year"

bibToPub :: T -> Publication
bibToPub e = Publication tp ti au yr "" "" "" e
  where tp = bibPubType e
        ti = bibTitle e
        au = bibAuthors e
        yr = bibYear e

parsePublications :: String -> [Publication]
parsePublications s = case parse (skippingLeadingSpace file) "BibTeX" s of
  Left err -> error (show err)
  Right bibs -> map bibToPub bibs
