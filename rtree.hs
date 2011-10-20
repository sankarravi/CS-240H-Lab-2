module RTree where

import System
import Char
import Data.List
import Data.Bits
import Data.List.Split
import Data.Ord

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
        

--our max coordinate is 2^16, so we need an order 16 Hilbert curve
hilbertOrder :: Int
hilbertOrder = 16

--max capacity of a Leaf node
c_l :: Int
c_l = 2

--max capacity of an Inner node
c_n :: Int
c_n = 2


main :: IO ()
main = do
    (fileName:_) <- getArgs
    s <- readFile fileName
    --let root = buildRTreeFromString
    putStrLn "hi"


buildRTreeFromString :: String -> RTree
buildRTreeFromString fileContents =
    let rects = map strToRect $ lines fileContents
        sortedRects = sortBy (\r -> comparing hVal r) rects
        --rectCount = length sortedRects
    in
        buildRTreeFromRects sortedRects

 
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
	      

{-True if two Rects intersect 
 -}
checkIntersection :: Rect -> Rect -> Bool
checkIntersection r1 r2 =
    (xCoord $ llPoint r1) < (xCoord $ urPoint r2) &&
    (xCoord $ urPoint r1) > (xCoord $ llPoint r2) &&
    (yCoord $ urPoint r1) < (yCoord $ llPoint r2) &&
    (yCoord $ llPoint r1) > (yCoord $ urPoint r2)


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


toIntList :: [String] -> [Int]
toIntList = map read


{-
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

instance Eq Rect where
    r1 == r2    = (llPoint r1) == (llPoint r2) && (urPoint r1) == (urPoint r2)

instance Eq Point where
    p1 == p2    = (xCoord p1) == (xCoord p2) && (yCoord p1) == (yCoord p2)