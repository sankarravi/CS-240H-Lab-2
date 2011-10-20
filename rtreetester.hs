module RTreeTester where

import Test.QuickCheck
import RTree

--Construct a rect inside r1, find their MBR, and verify that it is equal to r1
boundingBox :: Rect -> Bool
boundingBox r1 = 
    r1 == mbr
	where
		containedRect = Rect {
		        llPoint = Point { xCoord = xLow, yCoord = yLow},
	            urPoint = Point { xCoord = xHi, yCoord = yHi },
	            hVal = 0
	    }
	    xLow = (xCoord $ llPoint r1) + (l `div` 4)
	    xHi = (xCoord $ urPoint r1) + (l `div` 2)
	    yLow = (yCoord $ llPoint r1) + (w `div` 4)
	    yHi = (yCoord $ urPoint r1) + (w `div` 2)
		l = (xCoord $ urPoint r1) - (xCoord $ llPoint r1)
		w = (yCoord $ urPoint r1) - (yCoord $ llPoint r1)
		mbr = findMbr r1 containedRect


--Construct a rect inside r1 and verify that it intersects with r1
verifyIntersection :: Rect -> Bool
verifyIntersection r1 =
    checkIntersection r1 containedRect
    where
		containedRect = Rect {
		        llPoint = Point { xCoord = xLow, yCoord = yLow},
	            urPoint = Point { xCoord = xHi, yCoord = yHi },
	            hVal = 0
	    }
	    xLow = (xCoord $ llPoint r1) + (l `div` 4)
	    xHi = (xCoord $ urPoint r1) + (l `div` 2)
	    yLow = (yCoord $ llPoint r1) + (w `div` 4)
	    yHi = (yCoord $ urPoint r1) + (w `div` 2)
		l = (xCoord $ urPoint r1) - (xCoord $ llPoint r1)
		w = (yCoord $ urPoint r1) - (yCoord $ llPoint r1)


--I wanted to have xLow, yLow, l, and w be positive ints < 65536,
--but I wasn't able to make that work	
instance Arbitrary Rect where
	arbitrary = do
		xLow <- arbitrary
		yLow <- arbitrary
		l <- arbitrary
		w <- arbitrary
		let
			xLowAbs = abs xLow
			yLowAbs = abs yLow
			lAbs = abs l
			wAbs = abs w
			xHiAbs = if xLowAbs + lAbs < 65536 then xLowAbs + lAbs else 65535
			yHiAbs = if yLowAbs + wAbs < 65536 then yLowAbs + wAbs else 65535
		return $ Rect { llPoint = Point { xCoord = xLowAbs, yCoord = yLowAbs},
			urPoint = Point { xCoord = xHiAbs, yCoord = yHiAbs },
			hVal = 0
	     }

{-positiveCoord :: (Arbitrary a) => Int -> Gen (Positive a)
positiveCoord = liftM Int arbitrary suchthat-}
