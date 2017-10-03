import Data.List
import System.Random
import Lecture5

-- Assignment 1 (Time: 2 hours)
blocksNrc :: [[Int]]
blocksNrc = [[2..4],[6..8]]

bln :: Int -> [Int]
bln x = concat $ filter (elem x) blocksNrc

extendNodeNrc :: Node -> Constraint -> [Node]
extendNodeNrc (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         pruneNrc (r,c,v) constraints) | v <- vs ]

pruneNrc :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
pruneNrc _ [] = []
pruneNrc (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | sameblockNrc (r,c) (x,y) = 
        (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | otherwise = (x,y,zs) : pruneNrc (r,c,v) rest

sameblockNrc :: (Row,Column) -> (Row,Column) -> Bool
sameblockNrc (r,c) (x,y) = bln r == bln x && bln c == bln y 

solveNrc :: [Node] -> [Node]
solveNrc = search succNodeNrc solved 

succNodeNrc :: Node -> [Node]
succNodeNrc(s,[]) = []
succNodeNrc (s,p:ps) = extendNodeNrc (s,ps) p 

solveAndShowNrc :: Grid -> IO[()]
solveAndShowNrc gr = solveShowNrc (initNode gr)

solveShowNrc :: [Node] -> IO[()]
solveShowNrc = sequence . fmap showNode . solveNrc

nrc1 :: Grid
nrc1 = [[0,0,0,3,0,0,0,0,0],
        [0,0,0,7,0,0,3,0,0],
        [2,0,0,0,0,0,0,0,8],
        [0,0,6,0,0,5,0,0,0],
        [0,9,1,6,0,0,0,0,0],
        [3,0,0,0,7,1,2,0,0],
        [0,0,0,0,0,0,0,3,1],
        [0,8,0,0,4,0,0,0,0],
        [0,0,2,0,0,0,0,0,0]]

--	 Answer nrc1:
--	+-------+-------+-------+
--	| 4 7 8 | 3 9 2 | 6 1 5 |
--	| 6 1 9 | 7 5 8 | 3 2 4 |
--	| 2 3 5 | 4 1 6 | 9 7 8 |
--	+-------+-------+-------+
--	| 7 2 6 | 8 3 5 | 1 4 9 |
--	| 8 9 1 | 6 2 4 | 7 5 3 |
--	| 3 5 4 | 9 7 1 | 2 8 6 |
--	+-------+-------+-------+
--	| 5 6 7 | 2 8 9 | 4 3 1 |
--	| 9 8 3 | 1 4 7 | 5 6 2 |
--	| 1 4 2 | 5 6 3 | 8 9 7 |
--	+-------+-------+-------+
assignment1 :: IO[()]
assignment1 = (solveAndShowNrc nrc1)