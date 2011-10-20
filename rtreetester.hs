module RTreeTester where

import Test.QuickCheck
import RTree

boundingBox :: Rect -> Bool
boundingBox r1 = 
    r1 == mbr
	where
	    mbr = r1
		{-containedRect = Rect {
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
		mbr = findMbr r1 r1-}
	
	
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
			xHiAbs = xLowAbs + lAbs
			yHiAbs = yLowAbs + wAbs
			--xHiAbs = if xLowAbs + lAbs < 65536 then xLowAbs + lAbs else 65535
			--yHiAbs = if yLowAbs + wAbs < 65536 then yLowAbs + wAbs else 65535
		return $ Rect { llPoint = Point { xCoord = xLowAbs, yCoord = yLowAbs},
			urPoint = Point { xCoord = xHiAbs, yCoord = yHiAbs },
			hVal = 0
	     }

{-positiveCoord :: (Arbitrary a) => Int -> Gen (Positive a)
positiveCoord = liftM Int arbitrary suchthat-}
