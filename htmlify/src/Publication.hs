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
--import qualified Text.Blaze.Html5.Attributes as A
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
  , series      :: String
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

bibToPub :: T -> Publication
bibToPub e = Publication tp ti au yr pb sr ab "" e
  where tp = bibPubType e
        ti = bibField "title" e
        au = bibAuthors e
        yr = bibField "year" e
        pb = bibField "booktitle" e
        sr = bibField "series" e
        ab = bibField "abstract" e

parsePublications :: String -> [Publication]
parsePublications s = case parse (skippingLeadingSpace file) "BibTeX" s of
  Left err -> error (show err)
  Right bibs -> map bibToPub bibs

fullStop :: String -> String
fullStop = flip (++) "."

htmlTitle :: String -> Html
htmlTitle = H.i . toHtml . fullStop

htmlAuthors :: [String] -> Html
htmlAuthors = toHtml . fullStop . intercalate ", "

htmlProceedings :: String -> String -> Html
htmlProceedings pb sr = toHtml (fullStop conf)
  where conf = "In " ++ pb ++ " (" ++ sr ++ ")"

publicationToHtml :: Publication -> Html
publicationToHtml (Publication Conference ti au yr pb sr _ _ _) = do
  htmlAuthors au
  htmlTitle ti
  toHtml (fullStop yr)
  htmlProceedings pb sr
publicationToHtml _ = error "Unsupported publication type"

publicationsToHtml :: [Publication] -> Html
publicationsToHtml ps = do
  H.ul $ mapM_ (H.li . publicationToHtml) ps

