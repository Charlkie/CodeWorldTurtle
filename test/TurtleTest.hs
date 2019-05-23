-- >>> round' 0
-- 0.0
--
-- >>> round' 1.845 
-- 1.84
-- 
-- >>> round' 1.8459 -- carry test
-- 1.85
--
-- >>> round' (-1.8459)
-- -1.84
--
-- >>> roundedPoints [[]]
-- [[]]
--
-- >>> roundedPoints [[(1.845, 0), (0, -1.8459)], [(0,0)]]
-- [[(1.84,0.0),(0.0,-1.85)],[(0.0,0.0)]]
--
-- >>> triangleVertices 0
-- [(0,0), (0,0), (0,0)]
--
-- >>> pointsCommon [(1.5,0.0),(-1.5,0.0),(0.0,2.598)] [[(0.0,0.0),(-1.5,1.8e-16),(-2.2e-16,2.59),(1.5,4.4e-16),(6.69e-16,-4.03e-16)]] 
-- True
--
-- >>> pointsCommon [[(10,5)], [(15,3)]] [(10,5)]
-- False
--
-- >>> triangleVertices 1
-- [(0.5,0.0),(-0.5,0.0),(0.0,0.87)]
--
-- >>> triangleVertices 3
-- [(1.5,0.0),(-1.5,0.0),(0.0,2.6)]

-- >>> squareVertices 0
-- [(0.0,0.0),(-0.0,0.0),(0.0,0.0),(-0.0,0.0)]
--
-- >>> squareVertices 1
-- [(0.5,0.0),(-0.5,0.0),(0.5,1.0),(-0.5,1.0)]
--
-- >>> squareVertices 3
-- [(1.5,0.0),(-1.5,0.0),(1.5,3.0),(-1.5,3.0)]
--
-- >>> lolwtf [5]
-- [45]

module Main where

import Turtle
import Testing
import CodeWorld

-- | The list of tests to run.
tests :: [Test]
tests = [
          triangleTest1,
          polygonTest1, 
          polygonTest2,
          interpretLSystemTest1,
          interpretLSystemTest2,
          interpretLSystemTest3,
          sierpinskiTest1,
          sierpinskiTest2,
          lSystemCommandsTest
        ]

-- | The 'round'' functions rounds down from third decimal
round' :: Double -> Double
round' number = b a 
  where
    a = (round (number*100))
    b integer = (fromInteger integer)/100

-- The 'roundedPoints' function returns the list of rounded points
roundedPoints :: [[Point]] -> [[Point]]
roundedPoints [] = []
roundedPoints (x:xs) = map (\(a,b) -> (round' a,round' b)) x : roundedPoints xs

-- | The 'pointsCommon' function checks whether all the points belonging to 
--   each list inside A is common the corresponding elements of list is B
pointsCommon :: [[Point]] -> [[Point]] -> Bool
pointsCommon a b 
  | elem False (allAinB a b) = False
  | otherwise = True
    where        
      allAinB [] [] = []
      allAinB _ []  = []
      allAinB [] _  = []
      allAinB (x:xs) (y:ys) 
        | elem False (map (\e -> elem e y) x) = False : allAinB xs ys
        | otherwise = True : allAinB xs ys

triangleVertices :: Double -> [Point]
triangleVertices l = [
                      (l/2,0), (-l/2,0),
                      (0, sqrt ((l**2)-((l/2)**2)))
                      ]

squareVertices :: Double -> [Point]
squareVertices l = [
                      (l/2,0), (-l/2,0),
                      (l/2,l), (-l/2,l)
                    ]
-- | The 'seirpSideLenth' function returns the number of points on a 
--   a side of a sierpinski triangle of a given depth.
seirpSideLenth :: Integer -> Double
seirpSideLenth 1 = 3
seirpSideLenth depth = (2*seirpSideLenth (depth-1)) -1

-- | The 'sierpinskiPoints' function returns the coordinated of every vertex of
--   ever triangle within a sierpinski triangle
sierpinskiPoints :: Integer -> Double -> [Point]
sierpinskiPoints 0 l = [(0,0), (0,l), (l/2/(tan (pi/6)),l/2)]
sierpinskiPoints depth l = series (seirpSideLenth depth) 
  (seirpSideLenth depth) 0 ((l/2)/(tan (pi/6)))
  where
    series _ 0 _ _ = []
    series p c i x = sp ((p-1)-(0.5*(p-c))) ((0.5)*(p-c)) (x*i) 
      ++ series p (c-1) (i+1) x
    sp c x i
        | c == x     = [(i,c)]
        | otherwise = (i,c) : sp (c-1) x i

-- | The polygonPoints functions returns a rounded list of points created by 
-- | the polygon function
polygonPoints :: Int -> Double -> [[Point]]
polygonPoints n l = roundedPoints 
  (getPoints initialState True (Turtle.polygon n l))

-- | The triangle functions returns a rounded list of points created by 
-- | the triangle function
trianglePoints :: Double -> [[Point]]
trianglePoints l  = roundedPoints (getPoints initialState True (triangle l))

-- | The sierpinski functions returns a rounded list of points created by 
-- | the sierpinski function
sierpinskiPointsTest :: Int -> Double -> [[Point]]
sierpinskiPointsTest d l = roundedPoints 
  (getPoints initialState True (sierpinski d l))

-- | The myAssertEqual checks whether two lists of list of points will create
-- | the same shape
myAssertEqual :: [Char] -> [[Point]] -> [[Point]] -> TestResult
myAssertEqual string x y
  | pointsCommon x y == True = OK
  | otherwise = Fail string

triangleTest1 :: Test
triangleTest1 = Test "Triangle == Triangle"
  (myAssertEqual "'triangle' did not produce a valid triangle shape" 
  (roundedPoints [triangleVertices 1]) (trianglePoints 1))  
  
triangleTest2 :: Test
triangleTest2 = Test "Triangle == Triangle"
  (myAssertEqual "'triangle 3' failed to produce a valid triangle shape"
  (roundedPoints [triangleVertices 3]) (trianglePoints 3)) 

polygonTest1 :: Test
polygonTest1 = Test "Polygon 3 == Triangle"
  (myAssertEqual "'polygon' did not produce a valid polygon shape" 
  (roundedPoints [triangleVertices 1]) (polygonPoints 3 1))

polygonTest2 :: Test
polygonTest2 = Test "Polygon 4 == Square"
  (myAssertEqual "'polygon' did not produce a valid polygon shape" 
  (roundedPoints [squareVertices 1]) (polygonPoints 4 1)) 

interpretLSystemTest1 :: Test
interpretLSystemTest1 = Test "Interpret Sierpinski 0"
  (assertEqual (interpretLSystem sierpinskiSystem 0)
    "F-G-G")

interpretLSystemTest2 :: Test
interpretLSystemTest2 = Test "Interpret Sierpinski 1"
  (assertEqual (interpretLSystem sierpinskiSystem 1) 
    "F-G+F+G-F-GG-GG")

interpretLSystemTest3 :: Test
interpretLSystemTest3 = Test "Interpret Sierpinski 3"
  (assertEqual (interpretLSystem sierpinskiSystem 2)
    "F-G+F+G-F-GG+F-G+F+G-F+GG-F-G+F+G-F-GGGG-GGGG")

sierpinskiTest1 :: Test
sierpinskiTest1 = Test "Sierpinski 0"
  (myAssertEqual 
  "'Sierpinski' did not produce a valid Sierpinski shape" 
  (roundedPoints [sierpinskiPoints 0 1])
  (sierpinskiPointsTest 0 1))

sierpinskiTest2 :: Test
sierpinskiTest2 = Test "Sierpinski 2"
  (myAssertEqual 
  "'Sierpinski' did not produce a valid Sierpinski shape" 
  (roundedPoints [sierpinskiPoints 2 1])
  (sierpinskiPointsTest 2 1))

lSystemCommandsTest :: Test
lSystemCommandsTest = Test "LSystemsCommand == SierpinskiArrowHead"
  (myAssertEqual 
  "'lSystemsCommand' did not produce a valid SierpinskiArrowHead shape" 
  ([lSystemCommandsPoints])
  (roundedPoints (getPoints initialState True lSystemCommands)))

lSystemCommandsPoints :: [Point]
lSystemCommandsPoints = [
         (-5.0, -8.0), (-5.0, -7.5), (-5.0, -6.5), (-5.0, -6.0), (-5.0, -5.0),
         (-5.0, -4.5), (-5.0, -3.5), (-5.0, -3.0), (-5.0, -2.0), (-5.0, -1.5), 
         (-5.0, -0.5), (-5.0, 0.0), (-5.0, 1.0), (-5.0, 1.5), (-5.0, 2.5), 
         (-5.0, 3.0), (-5.0, 4.0), (-5.0, 4.5), (-5.0, 5.5), (-5.0, 6.0), 
         (-5.0, 7.0), (-5.0, 7.5), (-4.57, -7.25), (-4.57, -6.75), 
         (-4.57, -5.75), (-4.57, -5.25), (-4.57, -4.25), (-4.57, -3.75), 
         (-4.57, -2.75), (-4.57, -2.25), (-4.57, -1.25), (-4.57, -0.75), 
         (-4.57, 0.25), (-4.57, 0.75), (-4.57, 1.75), (-4.57, 2.25), 
         (-4.57, 3.25), (-4.57, 3.75), (-4.57, 4.75), (-4.57, 5.25), 
         (-4.57, 6.25), (-4.57, 6.75), (-4.57, 7.75), (-4.13, -7.5), 
         (-4.13, -6.5), (-4.13, -5.0), (-4.13, -4.5), (-4.13, -3.5), 
         (-4.13, -3.0), (-4.13, -1.5), (-4.13, -0.5), (-4.13, 1.0), 
         (-4.13, 1.5), (-4.13, 2.5), (-4.13, 3.0), (-4.13, 4.5), 
         (-4.13, 5.5), (-4.13, 7.0), (-4.13, 7.5), (-3.7, -7.25), 
         (-3.7, -6.75), (-3.7, -5.25), (-3.7, -2.75), (-3.7, -1.25), 
         (-3.7, -0.75), (-3.7, 0.75), (-3.7, 3.25), (-3.7, 4.75), 
         (-3.7, 5.25), (-3.7, 6.75), (-3.27, -6.5), (-3.27, -6.0), 
         (-3.27, -5.0), (-3.27, -3.0), (-3.27, -2.0), (-3.27, -1.5), 
         (-3.27, 1.0), (-3.27, 1.5), (-3.27, 2.5), (-3.27, 3.0), 
         (-3.27, 5.5), (-3.27, 6.0), (-3.27, 7.0), (-2.83, -6.75), 
         (-2.83, -5.75), (-2.83, -5.25), (-2.83, -2.75), (-2.83, -2.25), 
         (-2.83, -1.25), (-2.83, 1.75), (-2.83, 2.25), (-2.83, 5.25), 
         (-2.83, 6.25), (-2.83, 6.75), (-2.4, -6.5), (-2.4, -6.0), 
         (-2.4, -2.0), (-2.4, -1.5), (-2.4, 1.5), (-2.4, 2.5), (-2.4, 5.5), 
         (-2.4, 6.0), (-1.97, -5.75), (-1.97, -2.25), (-1.97, 1.75), 
         (-1.97, 2.25), (-1.97, 6.25), (-1.54, -6.0), (-1.54, -5.0), 
         (-1.54, -4.5), (-1.54, -3.5), (-1.54, -3.0), (-1.54, -2.0), 
         (-1.54, 2.5), (-1.54, 3.0), (-1.54, 4.0), (-1.54, 4.5), (-1.54, 5.5),
         (-1.54, 6.0), (-1.1, -5.75), (-1.1, -5.25), (-1.1, -4.25), 
         (-1.1, -3.75), (-1.1, -2.75), (-1.1, -2.25), (-1.1, 2.25), 
         (-1.1, 3.25), (-1.1, 3.75), (-1.1, 4.75), (-1.1, 5.25), 
         (-0.67, -5.0), (-0.67, -4.5), (-0.67, -3.5), (-0.67, -3.0), 
         (-0.67, 2.5), (-0.67, 3.0), (-0.67, 4.5), (-0.67, 5.5), 
         (-0.24, -5.25), (-0.24, -2.75), (-0.24, 3.25), (-0.24, 4.75), 
         (-0.24, 5.25), (0.2, -5.0), (0.2, -4.5), (0.2, -3.5), (0.2, -3.0), 
         (0.2, 3.0), (0.2, 4.0), (0.2, 4.5), (0.63, -4.25), (0.63, -3.75), 
         (0.63, 3.25), (0.63, 3.75), (0.63, 4.75), (1.06, -4.5), (1.06, -3.5), 
         (1.06, 4.0), (1.06, 4.5), (1.5, -4.25), (1.5, -3.75), (1.5, 3.75), 
         (1.93, -3.5), (1.93, -3.0), (1.93, -2.0), (1.93, -1.5), (1.93, -0.5), 
         (1.93, 0.0), (1.93, 1.0), (1.93, 1.5), (1.93, 2.5), (1.93, 3.0), 
         (1.93, 4.0), (2.36, -3.75), (2.36, -2.75), (2.36, -2.25), (2.36, -1.25), 
         (2.36, -0.75), (2.36, 0.25), (2.36, 0.75), (2.36, 1.75), (2.36, 2.25), 
         (2.36, 3.25), (2.36, 3.75), (2.79, -3.5), (2.79, -3.0), (2.79, -1.5), 
         (2.79, -0.5), (2.79, 1.0), (2.79, 1.5), (2.79, 2.5), (2.79, 3.0), 
         (3.23, -2.75), (3.23, -1.25), (3.23, -0.75), (3.23, 0.75), (3.23, 3.25), 
         (3.66, -3.0), (3.66, -2.0), (3.66, -1.5), (3.66, 1.0), (3.66, 1.5), 
         (3.66, 2.5), (3.66, 3.0), (4.09, -2.75), (4.09, -2.25), (4.09, -1.25), 
         (4.09, 1.75), (4.09, 2.25), (4.53, -2.0), (4.53, -1.5), (4.53, 1.5), 
         (4.53, 2.5), (4.96, -2.25), (4.96, 1.75), (4.96, 2.25), (5.39, -2.0), 
         (5.39, -1.5), (5.39, -0.5), (5.39, 0.0), (5.39, 1.0), (5.39, 1.5), 
         (5.83, -1.25), (5.83, -0.75), (5.83, 0.25), (5.83, 0.75), (5.83, 1.75), 
         (6.26, -1.5), (6.26, -0.5), (6.26, 1.0), (6.26, 1.5), (6.69, -1.25), 
         (6.69, -0.75), (6.69, 0.75), (7.12, -0.5), (7.12, 0.0), (7.12, 1.0), 
         (7.56, -0.75), (7.56, 0.25), (7.56, 0.75), (7.99, -0.5), (7.99, 0.0), 
         (8.42, 0.25), (8.86, 0.0)]

main :: IO ()
main = runTests tests
