module Publication 
( PubType (..)
, Publication (..)
, parsePublications
, publicationToHtml
, publicationsToHtml
) where

import Text.BibTeX.Entry
import Text.BibTeX.Parse
import Text.Parsec.Prim
import Text.LaTeX.Character (toUnicodeString)
import Text.Blaze.Html5 (Html, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Char (toLower)
import Data.List (intercalate)

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
  Just v -> toUnicodeString v
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

fullStop :: String -> String
fullStop s = s ++ "."

htmlTitle :: String -> Html
htmlTitle = H.i . toHtml . fullStop

htmlAuthors :: [String] -> Html
htmlAuthors = toHtml . fullStop . intercalate ", "

publicationToHtml :: Publication -> Html
publicationToHtml (Publication _ ti au yr _ _ _ _) = do
  htmlAuthors au
  htmlTitle ti
  toHtml (fullStop yr)

publicationsToHtml :: [Publication] -> Html
publicationsToHtml ps = do
  H.ul $ mapM_ (H.li . publicationToHtml) ps

