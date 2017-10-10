--test1 results 
--True - (0.14 secs, 132077608 bytes)

--test2 results
--True - (0.02 secs, 14605512 bytes)

import Data.List
import System.Random
import Assignment2
import Lecture5

tester:: [Lecture5.Node] -> Bool
tester[] = True
tester((x,xs): xxs) = Lecture5.consistent x && tester xxs


sudokus = [Lecture5.example1, Lecture5.example2, Lecture5.example3, Lecture5.example4]

test1 = 
  all (\sudoku -> tester(Lecture5.solveNs (Lecture5.initNode sudoku))) sudokus

test2 = 
  all (\sudoku -> tester(Assignment2.solveNs (Assignment2.initNode sudoku))) sudokus

