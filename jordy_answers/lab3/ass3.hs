import Lecture3

contradiction :: Form -> Bool
contradiction a = not (satisfiable a)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)


--main = print (contradiction (head (parse "^(1 -1)")))
main = print (tautology (head (parse "|(1 -1)")))