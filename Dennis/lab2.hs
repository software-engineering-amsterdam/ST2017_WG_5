-- Recognizing triangles ---------------- 70 min
--I've defined some trangles and used these to check my function
--The list tests contrains quadruples with int, int, int, shape
--In Test every triangle is checked using the all command

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  
    | a^2 + b^2 == c^2 = Rectangular
    | a == b && b == c && c == a = Equilateral
    | (a == b || b == c || a == c) && (a+b > c) && (b+c > a) && (a+c>b) = Isosceles
    | (a+b > c) && (b+c > a) && (a+c>b) = Other
    | otherwise = NoTriangle

test:: Bool
test = all (\(a,b,c,shape) -> triangle a b c == shape) tests

tests :: [(Integer, Integer, Integer, Shape)]
tests = [(3,4,5,Rectangular),(5,12,13, Rectangular),(3,3,3, Equilateral), 
    (6,6,6,Equilateral), (1,1,1000,NoTriangle),(1,1,-1,NoTriangle), (5,5,3,Isosceles)]

main = print(test)