module Main where

import System
import System.CPUTime
import Char
import Data.List
import Data.Bits
import Data.List.Split
import Data.Ord
import Text.Printf

--Data Types
data Point = Point {
    xCoord :: Int,
    yCoord :: Int
} deriving Show

data Rect = Rect {
    llPoint :: Point,
    urPoint :: Point,
    hVal :: Int
} deriving Show

data RTree =
    Inner {
        children :: [RTree],
        mbr :: Rect,
        lhv :: Int
    }
    | Leaf {
        rects :: [Rect],
        mbr :: Rect,
        lhv :: Int
    } deriving Show
        
--"Constants"
--our max coordinate is 2^16, so we need an order 16 Hilbert curve
hilbertOrder :: Int
hilbertOrder = 16

--max capacity of a Leaf node
c_l :: Int
c_l = 5

--max capacity of an Inner node
c_n :: Int
c_n = 5


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
        rectsCount = length rects
    printf "%s: %d rectangles processed in %0.3f microseconds\n"
        fileName rectsCount (diff :: Double)
    
    queryLoop root



{-
 -}        
buildInnerNodes :: [RTree] -> [RTree]
buildInnerNodes nodes
    | (length nodes == 1) = nodes
    | otherwise = buildInnerNodes $ map (\ns -> Inner {
            children = ns,
            mbr = foldr1 findMbr (map mbr ns),
            lhv = maximum $ map lhv ns
        }) (splitList c_n nodes)

{-
 -} 
buildRTreeFromRects :: [Rect] -> RTree
buildRTreeFromRects sortedRects =
    head $ buildInnerNodes leaves
    where
        groupedRects = splitList c_l sortedRects
        leaves = map (\rs -> Leaf { 
                    rects = rs,
                    mbr = foldr1 findMbr rs,
                    lhv = maximum $ map hVal rs
                }) groupedRects


{-True if two Rects intersect 
 -}
checkIntersection :: Rect -> Rect -> Bool
checkIntersection r1 r2 =
    (xCoord $ llPoint r1) <= (xCoord $ urPoint r2) &&
    (xCoord $ urPoint r1) >= (xCoord $ llPoint r2) &&
    (yCoord $ urPoint r1) >= (yCoord $ llPoint r2) &&
    (yCoord $ llPoint r1) <= (yCoord $ urPoint r2)


{- Take two Rects and return their MBR (Minimum Bounding Rectangle)
   as another Rect
 -}
findMbr :: Rect -> Rect -> Rect
findMbr r1 r2 =
    Rect { llPoint = Point { xCoord = xLow, yCoord = yLow},
	       urPoint = Point { xCoord = xHi, yCoord = yHi },
	       hVal = 0
	     }
    where
        xLow = minimum [(xCoord $ llPoint r1), (xCoord $ llPoint r2)]
        yLow = minimum [(yCoord $ llPoint r1), (yCoord $ llPoint r2)]
        xHi = maximum [(xCoord $ urPoint r1), (xCoord $ urPoint r2)]
        yHi = maximum [(yCoord $ urPoint r1), (yCoord $ urPoint r2)]


{- Calculates the distance to (x,y) along a hilbert curve of order 'd'

 Borrowed from Bryan's blog at:
 http://www.serpentine.com/blog/2007/01/11/
	two-dimensional-spatial-hashing-with-space-filling-curves/
 -}
hilbertDistance :: (Bits a, Ord a) => Int -> (a,a) -> a
hilbertDistance d (x,y)
    | x < 0 || x >= 1 `shiftL` d = error "x bounds"
    | y < 0 || y >= 1 `shiftL` d = error "y bounds"
    | otherwise = dist (1 `shiftL` (d - 1)) (1 `shiftL` ((d - 1) * 2)) 0 x y
    where dist 0 _ result _ _ = result
          dist side area result x y =
              case (compare x side, compare y side) of
              (LT, LT) -> step result y x
              (LT, _)  -> step (result + area) x (y - side)
              (_, LT)  -> step (result + area * 3) (side - y - 1)
                          (side * 2 - x - 1)
              (_, _)   -> step (result + area * 2) (x - side) (y - side)
              where step = dist (side `shiftR` 1) (area `shiftR` 2)


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
            

{- Implements the RTree search algorithm: at non-leaves, check if the MBR overlaps
 with our search window and descend if so. At leaves, check for intersection and
 return the matches
 -}
queryRTree :: RTree -> Rect -> [Rect] 
queryRTree l@Leaf{ rects = rs } searchRect = filter (\r -> checkIntersection searchRect r) rs 
queryRTree n@Inner { children = childNodes, mbr = myMbr } searchRect =
    if checkIntersection searchRect myMbr
        then concatMap (\child -> queryRTree child searchRect) childNodes
        else []


{- Split a list of as into a list of list of as of max size n
 -}
splitList :: Int -> [a] -> [[a]]
splitList n xs
   | length xs <= n = [xs]
   | otherwise = [take n xs] ++ splitList n (drop n xs)


{- Convert a String of the form "x1,y1,x2,y2,x3,y3,x4,y4" to a Rect
 -}
strToRect :: String -> Rect
strToRect line =
	Rect { llPoint = Point { xCoord = xLow, yCoord = yLow},
	       urPoint = Point { xCoord = xHi, yCoord = yHi },
	       hVal = hilbertDistance hilbertOrder (xCenter,yCenter)
	     }
	where [x1,y1,x2,y2,x3,y3,x4,y4] = splitOn "," line
	      xCoords = toIntList [x1,x2,x3,x4]
	      yCoords = toIntList [y1,y2,y3,y4]
	      xLow = minimum xCoords
	      yLow = minimum yCoords
	      xHi = maximum xCoords
	      yHi = maximum yCoords
	      xCenter = (xLow + xHi) `div` 2
	      yCenter = (yLow + yHi) `div` 2
	      
	      
--The magic is in the type
toIntList :: [String] -> [Int]
toIntList = map read


instance Eq Rect where
    r1 == r2    = (llPoint r1) == (llPoint r2) && (urPoint r1) == (urPoint r2)

instance Eq Point where
    p1 == p2    = (xCoord p1) == (xCoord p2) && (yCoord p1) == (yCoord p2)