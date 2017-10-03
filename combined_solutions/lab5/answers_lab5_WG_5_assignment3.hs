import Lecture5

myyvals = [0..8]
myxvals = [0..8]

-- Check if there is only a single solution (take causes the program to not search the entire tree)
singleSol:: Grid -> Bool
singleSol gr = if (length( take 2 (solveNs (initNode gr)))) == 1
	then True
	else False

-- Reverse of singleSol
multiOrNoSol :: Grid -> Bool
multiOrNoSol gr = not (singleSol gr)

-- set the list element at position n to 0
changeElem n newElem (x:xs)
	| n == 0 = newElem:xs
	| otherwise = x:changeElem (n-1) newElem xs

-- Given a grid, check how many non-zero digits there are, substitute each digit by 0 (one by one)
-- and check if multiple solutions aris
minimal :: Grid -> Bool
minimal gr = do
	let n = singleSol gr
	if n
		then do
			let num_vals = countNonZero gr myxvals myyvals 0
			(solutionLoop gr num_vals)
		else False

-- Check for every number of the n non-zero values if changing it to 0 creates multiple solutions
-- a.k.a check for a minimal sudoku
solutionLoop :: Grid -> Int -> Bool
solutionLoop gr 0 = True
solutionLoop gr n = do
	let cur_grid = loopGrid gr myxvals myyvals n
	if (multiOrNoSol cur_grid)
		then solutionLoop gr (n-1)
		else False

-- loop through the grid, and count how many non-zero values there are
countNonZero :: Grid -> [Int] -> [Int] -> Int -> Int
countNonZero gr [8] [] n = n
countNonZero gr y [] n = countNonZero gr (tail y) [0..8] n
countNonZero gr yvals xvals n = do
	let xval = head xvals
	let yval = head yvals
	let cur_grid = gr !! yval
	let cur_num = cur_grid !! xval
	if ((cur_grid !! xval) /= 0)
		then countNonZero gr yvals (tail xvals) (n+1)
		else countNonZero gr yvals (tail xvals) n

-- loop through the grid, and change the nth non 0 value to 0
loopGrid :: Grid -> [Int] -> [Int] -> Int -> Grid
loopGrid gr _ _ 0 = gr
loopGrid gr [8] [] n = gr
loopGrid gr y [] n = loopGrid gr (tail y) [0..8] n
loopGrid gr yvals xvals n = do
	let xval = head xvals
	let yval = head yvals
	let cur_grid = gr !! yval
	-- if we find a number other then 0 and its the number we look for, substitute it by 0, else continue with same grid
	-- New row = (changeElem xval 0 cur_grid)
	-- new grid is therefore = (changeElem yval (changeElem xval 0 cur_grid) gr)
	if (((cur_grid !! xval) /= 0) && (n == 1))
		then loopGrid (changeElem yval (changeElem xval 0 cur_grid) gr) yvals (tail xvals) 0
		else if ((cur_grid !! xval) /= 0)
			then loopGrid gr yvals (tail xvals) (n-1)
			else loopGrid gr yvals (tail xvals) n

main = print (minimal example0)
