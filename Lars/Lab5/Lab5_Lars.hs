module Lab5 where
import Data.List
import Test.QuickCheck
import System.Random
import Lecture5

-- Check if the number of empty blocks in a grid is equal to the required 
-- number.
checkNumberOfEmptyBlocks :: Grid -> [Int] -> [Int] -> Int -> Int -> Bool
checkNumberOfEmptyBlocks g [6] [] emptyBlocks count = if count == emptyBlocks then True else False
checkNumberOfEmptyBlocks g y [] emptyBlocks count = checkNumberOfEmptyBlocks g (tail y) [0,3,6] emptyBlocks count
checkNumberOfEmptyBlocks g y x emptyBlocks count = do
    let xval = head x
    let yval = head y
    let cur_grid = g !! yval
    if ((cur_grid !! xval) /= 0)
        then checkNumberOfEmptyBlocks g y (tail x) emptyBlocks count
    else checkNumberOfEmptyBlocks g y (tail x) emptyBlocks (count + 1)
  

-- Empty the required amount of blocks, possible for emptying the
-- same block twice, thats why we need the function above
emptytheblock :: Grid -> Int -> Int -> IO Grid
emptytheblock g i 0 = return g
emptytheblock g 0 tries = return g
emptytheblock g i tries = do
    intervala <- getInterval
    intervalb <- getInterval
    let [r] = initNode g
    if (uniqueSol r) then emptytheblock (changeBlock g intervala intervalb intervalb) (i-1) 10          
    else emptytheblock g i (tries - 1)

startEmptyBlocks :: Int -> Grid -> IO Grid
startEmptyBlocks emptyBlocks g = emptytheblock g emptyBlocks 10
 

-- create random integer in interval (each interval represents a block)
genInt :: IO Int
genInt = getStdRandom (randomR (0,2))

getInterval :: IO [Int]
getInterval = do
    a <- genInt
    b <- genInt
    return (returnInterval a)


returnInterval :: Int -> [Int]
returnInterval x =
    case x of
        0 -> [0..2]
        1 -> [3..5]
        2 -> [6..8]

changeElem n newElem (x:xs)
    |   n == 0 = newElem:xs
    |   otherwise = x:changeElem (n-1) newElem xs

-- Change all values in a block to zero (thank you Jordy)
changeBlock :: Grid -> [Int] -> [Int] -> [Int] -> Grid
changeBlock gr [2] [] track = gr
changeBlock gr [5] [] track = gr
changeBlock gr [8] [] track = gr
changeBlock gr y [] track = changeBlock gr (tail y) track track
changeBlock gr yvals xvals track = do
    let xval = head xvals
    let yval = head yvals
    let cur_grid = gr !! yval
    if ((cur_grid !! xval) /= 0)
        then 
            let new_grid = (changeElem yval (changeElem xval 0 cur_grid) gr)
            in changeBlock new_grid yvals (tail xvals) track
    else changeBlock gr yvals (tail xvals) track

-- Keep on trying untill we found a sudoku with the required empty blocks
-- or untill we reached the maximum tries. 
main :: Int -> Int -> IO ()
main emptyBlocks maxTries = 
        if maxTries > 0 then
            do
              [(s, c)] <- rsolveNs [emptyN]
              grid <- startEmptyBlocks emptyBlocks (sud2grid s)
              let [r] = initNode grid
              if not (uniqueSol r) 
                then do
                    print ("No unique solution, trying agan " ++ show maxTries ++ ".")
                    Lab5.main emptyBlocks (maxTries - 1)
                else if not (checkNumberOfEmptyBlocks grid [0,3,6] [0,3,6] emptyBlocks 0)
                 then do 
                    print ("Not enough empty blocks, trying agan " ++ show maxTries ++ ".")
                    Lab5.main emptyBlocks (maxTries - 1)
                else
                    showNode r
        else
            print "No solvable sudoku created in max tries"
          
