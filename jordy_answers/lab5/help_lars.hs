import System.Random

-- create random integer in interval
genInt :: Int -> Int -> IO Int
genInt a b = getStdRandom (randomR (a,b))

getInterval :: Int -> Int -> IO()
getInterval x y = do
	a <- genInt x y
	b <- genInt x y
	let intervalA = returnInterval a
	print intervalA


returnInterval :: Int -> [Int]
returnInterval x =
	case x of
		0 -> [0..2]
		1 -> [3..5]
		2 -> [6..8]

main = getInterval 0 2
