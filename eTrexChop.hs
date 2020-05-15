
-- Please see REFERENCES.md file in same directory as this file
--
-- DEPENDENCIES:
-- 1) cabal update
-- 2) cabal install xml

import System.Environment
import System.IO
import Text.XML.Light
import Data.Time.Clock
import Data.Time.Format
-- import Debug.Trace

-- https://stackoverflow.com/questions/4174372/haskell-date-parsing-and-formatting
getTimeOfPoint :: Element -> UTCTime
getTimeOfPoint e = do
   let theElementsOfPoint = onlyElems (elContent e)
   let theTime = theElementsOfPoint !! 1
   let theContent = elContent theTime
   let theText = onlyText theContent
   let theDate = cdData (theText !! 0)
   let dateString = theDate
   let timeFromString = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" dateString :: UTCTime
   timeFromString


-- https://en.wikibooks.org/wiki/Haskell/Pattern_matching
trimTrkSegInternal :: UTCTime -> [Content] -> bool -> Int -> Int -> [Content]
trimTrkSegInternal baseTime [] goForward offsetInSeconds amountToTrimInSeconds =
   []
trimTrkSegInternal baseTime (tsc:xs) goForward offsetInSeconds amountToTrimInSeconds = do
   let trkSegElems = onlyElems (tsc : [])
   let curTrackPoint = trkSegElems !! 0
   let pointTime = getTimeOfPoint curTrackPoint
   let timeDiffNominal = diffUTCTime pointTime baseTime
   let secondsPassed = nominalDiffTimeToSeconds timeDiffNominal

   -- This probably isn't the best idea to chop the number off like that
   -- I'm not going to be picky right now
   let intSecondsPassed = floor secondsPassed
   if intSecondsPassed <= amountToTrimInSeconds then
     trimTrkSegInternal
       baseTime xs goForward offsetInSeconds amountToTrimInSeconds
     -- trace ("testA: " ++
     -- show(intSecondsPassed) ++ " " ++ show(amountToTrimInSeconds))
     --  (trimTrkSegInternal
     --    baseTime xs goForward offsetInSeconds amountToTrimInSeconds)
   else
     tsc :
       trimTrkSegInternal
       baseTime xs goForward offsetInSeconds amountToTrimInSeconds
     -- trace "testB"
     --   (tsc :
     --     trimTrkSegInternal
     --     baseTime xs goForward offsetInSeconds amountToTrimInSeconds)

trimTrkSeg :: [Content] -> bool -> Int -> Int -> [Content]
-- trimTrkSeg :: [Content] -> bool -> Int -> Int -> IO ()
trimTrkSeg trkSegContent goForward offsetInSeconds amountToTrimInSeconds = do
   let trkSegElems = onlyElems trkSegContent

   let firstTrkPt1 = trkSegElems !! 0
   let firstPt1Date = getTimeOfPoint firstTrkPt1
   trimTrkSegInternal firstPt1Date trkSegContent goForward offsetInSeconds amountToTrimInSeconds



main :: IO() 
main = do
  theArgs <- getArgs
  let argsLength = length theArgs

  --if (argsLength /= 4) then do
  --  putStrLn "Usage: ./eTrexChop [fileName] [direction] [offsetInSeconds] [secondsToTrim]"
  --  putStrLn ""
  --  putStrLn "Examples:"
  --  putStrLn ""
  --  putStrLn "-- Trim 10 minutes off the front of the file and "
  --  putStrLn "-- send output to mmChopped.gpx"
  --  putStrLn "./eTrexChop ./mountMagazine.gpx + 0 600 > ./mmChopped.gpx"
  --  putStrLn ""
  --  putStrLn "-- Trim 3 minutes going backward from the end of the file"
  --  putStrLn "-- and send output to stdout"
  --  putStrLn "./eTrexChop beaverLake.gpx - 0 180"
  --  putStrLn ""
  if (argsLength /= 2) then do
    putStrLn "Usage: ./eTrexChop [fileName] [secondsToTrimFromBeginning]"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn ""
    putStrLn "-- Trim 10 minutes off the front of the file and "
    putStrLn "-- send output to mmChopped.gpx"
    putStrLn "./eTrexChop ./mountMagazine.gpx 600 > ./mmChopped.gpx"
    putStrLn ""
    putStrLn "-- Trim 3 minutes off the front of the file "
    putStrLn "-- and send output to stdout"
    putStrLn "./eTrexChop beaverLake.gpx 180"
    putStrLn ""
  else do
    --let fileName = theArgs !! 0
    --let direction = theArgs !! 1
    --let offsetInSecondsStr = theArgs !! 2
    --let secondsToTrimStr = theArgs !! 3
    let fileName = theArgs !! 0
    let direction = "+"
    let offsetInSecondsStr = "0"
    let secondsToTrimStr = theArgs !! 1

    let offsetInSeconds = read offsetInSecondsStr::Int
    let secondsToTrim = read secondsToTrimStr::Int

    theFile <- readFile fileName
    -- putStr theFile

    -- Okay, Yeah with Haskell being recursive and all I should be
    -- be able to parse the XML myself but I'm too lazy (no-pun intended)
    -- See [1] for where I got the code
    let contents = parseXML theFile
    --let xmlContent = contents !! 0
    --print xmlContent 
    -- let xmlElem = Elem xmlContent
    --let xmlText = Text xmlContent
    --let xmlCRef = CRef xmlContent
    
    --let gpxContent = contents !! 1
    --putStrLn ""
    let theElements = onlyElems contents

    let header = theElements !! 0
    putStrLn (showElement header)
   
    let gpx = theElements !! 1
    let gpxContentList = elContent gpx
    -- let gpxElems = onlyElems gpxContentList
    -- let metaData = gpxElems !! 0
    -- let trk = gpxElems !! 1

    -- putStrLn ""

    let gpxName = elName gpx
    let gpxAttribs = (elAttribs gpx)
    
    let gpxContent = (elContent gpx)

    let trkContent = gpxContentList !! 1
    let trkElems = onlyElems (trkContent : [])

    let trkElement = trkElems !! 0

    let trkElemName = elName trkElement
    let trkElemAttribs = elAttribs trkElement

    
    let trkContent = (elContent trkElement)
    let nameContent = trkContent !! 0
    let extensionsContent = trkContent !! 1
    let trkSegContent = trkContent !! 2

    let trkSegElements = onlyElems (trkSegContent : [])
    let trkSegElement = trkSegElements !! 0

    let trkSegName = elName trkSegElement
    let trkSegAttribs = elAttribs trkSegElement
    let trkSegContent = elContent trkSegElement
    let trkSegELLine = elLine trkSegElement

    let trimmedTrackSegContent = trimTrkSeg trkSegContent True 0 secondsToTrim

    let newTrkSegElement = Element trkSegName trkSegAttribs trimmedTrackSegContent trkSegELLine
    let newTrkSegContent = Elem newTrkSegElement
    
    let trkElemContent = nameContent : extensionsContent : newTrkSegContent : []
    
    let trkELLLine = elLine trkElement
    let newTrkElement = Element trkElemName trkElemAttribs trkElemContent trkELLLine
    let trkContent = Elem newTrkElement

    let newGPX_content = (gpxContentList !! 0) : trkContent : []


    let gpxELLine = (elLine gpx)

    let newGPX = Element gpxName gpxAttribs newGPX_content gpxELLine

    -- print ""
    putStrLn (showElement newGPX)

    -- https://stackoverflow.com/questions/51509949/what-do-haskell-data-constructors-construct
    -- https://stackoverflow.com/questions/10115623/accessing-members-of-a-custom-data-type-in-haskell
--    let attribList = (elAttribs header)

  --  putStrLn "*************"
 --   print attribList
 --   let item1 = attribList !! 0
 --   let item2 = attribList !! 1
--    let item3 = attribList !! 2

  --  let item1Key = attrKey item1
    
    --let modifiedAttribList = (Attr item1Key "7.0") : item2 : item3 : []
   -- putStrLn "*************"
     
    --let clonedHeader = Element (elName header) modifiedAttribList (elContent header) (elLine header)



    -- putStrLn (showElement clonedHeader)

    -- let gpx = theElements !! 1
    
    -- putStrLn (showElement gpx)

   --let gpx = theElements !! 1
  -- print header
   -- putStrLn ""


    
    
