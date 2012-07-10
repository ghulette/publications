import Publication

main :: IO ()
main = do
  bibStr <- getContents
  let papers = parsePublications bibStr
  mapM_ print papers
