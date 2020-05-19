-- Please see REFERENCES.md file in same directory as this file
--
-- DEPENDENCIES:
-- 1) cabal update
-- 2) cabal install xml
-- 3) cabal install time

import System.Environment
import System.IO
import Text.XML.Light
import Data.Time.Clock
import Data.Time.Format
import Debug.Trace



-- https://stackoverflow.com/questions/4174372/haskell-date-parsing-and-formatting
getTimeOfPoint :: Element -> UTCTime
getTimeOfPoint e = do
   let theElementsOfPoint = onlyElems (elContent e)
   let theTime = theElementsOfPoint !! 1
   let theContent = elContent theTime
   let theText = onlyText theContent
   let theDate = cdData (theText !! 0)
   let dateString = theDate
   let timeFromString =
         parseTimeOrError
           True
           defaultTimeLocale
           "%Y-%m-%dT%H:%M:%SZ"
           dateString :: UTCTime
   timeFromString


-- https://en.wikibooks.org/wiki/Haskell/Pattern_matching
trimTrkSegInternal :: UTCTime -> [Content] -> Bool -> Int -> Int -> [Content]
trimTrkSegInternal baseTime [] goForward offsetInSeconds amountToTrimInSeconds =
   []
trimTrkSegInternal
   baseTime (tsc:xs) goForward offsetInSeconds amountToTrimInSeconds = do

   let trkSegElems = onlyElems (tsc : [])
   let curTrackPoint = trkSegElems !! 0
   let pointTime = getTimeOfPoint curTrackPoint
   let timeDiffNominal = diffUTCTime pointTime baseTime
   let secondsPassed = nominalDiffTimeToSeconds timeDiffNominal

   -- This probably isn't the best idea to chop the number off like that
   -- I'm not going to be picky right now
   let intSecondsPassed =
         if goForward then
             floor secondsPassed
         else
             (0 - (floor secondsPassed))
   if intSecondsPassed <= amountToTrimInSeconds then
     trimTrkSegInternal
        baseTime xs goForward offsetInSeconds amountToTrimInSeconds
     --trace ("testA: " ++
     --show(intSecondsPassed) ++ " " ++ show(amountToTrimInSeconds))
     -- (trimTrkSegInternal
     --   baseTime xs goForward offsetInSeconds amountToTrimInSeconds)
   else
     tsc :
        trimTrkSegInternal
        baseTime xs goForward offsetInSeconds amountToTrimInSeconds
     --trace "testB"
     --  (tsc :
     --    trimTrkSegInternal
     --    baseTime xs goForward offsetInSeconds amountToTrimInSeconds)



trimTrkSeg :: [Content] -> Bool -> Int -> Int -> [Content]
trimTrkSeg trkSegContent goForward offsetInSeconds amountToTrimInSeconds = do
   let trkSegElems =
         if goForward then do
             onlyElems trkSegContent
         else do
             reverse (onlyElems trkSegContent)

   let startingTrkPt1 = trkSegElems !! 0
   let startingPt1Date = getTimeOfPoint startingTrkPt1

   let trimmedList =
         trimTrkSegInternal
         startingPt1Date
         trkSegContent
         goForward
         offsetInSeconds
         amountToTrimInSeconds
   if goForward then
       trimmedList
   else
       trimmedList
      -- I'm not sure why I'm commenting this out...
      -- seems I need to reverse the list back...
      -- reverse trimmedList



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
  if (argsLength /= 3) then do
    putStrLn "Usage: ./eTrexChop [fileName] [direction] [secondsToTrim]"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn ""
    putStrLn "-- Trim 10 minutes off the front of the file and "
    putStrLn "-- send output to mmChopped.gpx"
    putStrLn "./eTrexChop ./mountMagazine.gpx + 600 > ./mmChopped.gpx"
    putStrLn ""
    putStrLn "-- Trim 3 minutes off the end of the file going backwards"
    putStrLn "-- and send output to stdout"
    putStrLn "./eTrexChop beaverLake.gpx - 180"
    putStrLn ""
  else do
    --let fileName = theArgs !! 0
    --let direction = theArgs !! 1
    --let offsetInSecondsStr = theArgs !! 2
    --let secondsToTrimStr = theArgs !! 3
    let fileName = theArgs !! 0
    let direction = "+"
    let offsetInSecondsStr = "0"
    let direction = theArgs !! 1

    let boolDirection =
         if (direction == "-") then
           False
         else
           True

    let secondsToTrimStr = theArgs !! 2

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

    let trimmedTrackSegContent =
          trimTrkSeg trkSegContent boolDirection 0 secondsToTrim

    let newTrkSegElement =
         Element trkSegName trkSegAttribs trimmedTrackSegContent trkSegELLine
    let newTrkSegContent = Elem newTrkSegElement
    let trkElemContent = nameContent : extensionsContent : newTrkSegContent : []
    let trkELLLine = elLine trkElement
    let newTrkElement =
         Element trkElemName trkElemAttribs trkElemContent trkELLLine
    let trkContent = Elem newTrkElement
    let newGPX_content = (gpxContentList !! 0) : trkContent : []
    let gpxELLine = (elLine gpx)
    let newGPX = Element gpxName gpxAttribs newGPX_content gpxELLine

    putStrLn (showElement newGPX)

    -- https://stackoverflow.com/questions/51509949/what-do-haskell-data-constructors-construct
    -- https://stackoverflow.com/questions/10115623/accessing-members-of-a-custom-data-type-in-haskell
