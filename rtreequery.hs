module Main where

import System
import System.CPUTime
import Data.List
import Data.Ord
import Text.Printf
import RTree

main :: IO ()
main = do
    (fileName:_) <- getArgs
    
    start <- getCPUTime
    s <- readFile fileName
    let rects = map strToRect $ lines s
        sortedRects = sortBy (\r -> comparing hVal r) rects
        root = buildRTreeFromRects sortedRects
    end <- getCPUTime
    
    let diff = (fromIntegral (end - start)) / (10^6)
    printf "%s: %d rectangles processed in %0.3f microseconds\n"
        fileName (length rects) (diff :: Double)
    
    queryLoop root

{- Repeatedly accepts a user string representing a rectangle
 and queries the RTree starting at root. Terminates with EOF
 -}
queryLoop :: RTree -> IO ()
queryLoop root = do
    input <- getLine
    if (input == "\EOT")
        then return ()
        else do
            start <- getCPUTime
            let r = strToRect input
                matches = queryRTree root r
            end <- getCPUTime
            let diff = (fromIntegral (end - start)) / (10^6)
            printf "found %d matches in %0.1f microseconds\n"
                (length matches) (diff :: Double)
            mapM_ printRectCorners (take 4 matches)
            queryLoop root
            
printRectCorners :: Rect -> IO ()
printRectCorners r =
	printf "\t%d,%d,%d,%d,%d,%d,%d,%d\n" x1 y1 x2 y2 x3 y3 x4 y4
	where
		x1 = xCoord $ llPoint r
		y1 = yCoord $ llPoint r
		x2 = x1
		y2 = y3
		x3 = xCoord $ urPoint r
		y3 = yCoord $ urPoint r
		x4 = x3
		y4 = y1
		
		