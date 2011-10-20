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
            --mapM_ (return show) (take 4 matches)
            queryLoop root