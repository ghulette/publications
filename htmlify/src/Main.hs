import Publication
import Text.Blaze.Html.Renderer.Pretty

main :: IO ()
main = do
  bib <- getContents
  let pubs = parsePublications bib
  if null pubs then return () else do
    let html = publicationsToHtml pubs
    putStrLn $ renderHtml html
