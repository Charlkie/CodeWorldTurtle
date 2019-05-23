module Turtle where

import CodeWorld

type Radians = Double

-- | The commands that we can send to our turtle.
data TurtleCommand
  = Forward Double -- ^ Drive forward the given number of units,
                   -- drawing if the pen is down.
  | Turn Radians -- ^ Turn the turtle. Positive values are
                 -- anticlockwise; negative values are clockwise.
  | PenUp -- ^ Lift the pen, so that future 'Forward' instructions do
          -- not draw.
  | PenDown -- ^ Lower the pen, so that future 'Forward' instructions
            -- will draw.
  deriving (Eq, Show) 

-- Task 1: Drawing Shapes 

-- | The triangle function return the commands to draw a triangle of a given
-- | side length
triangle :: Double -> [TurtleCommand]
triangle x = [
                Turn (pi/2), 
                Forward (x/2), 
                Turn (-(2*pi)/3), 
                Forward x, 
                Turn (-(2*pi)/3), 
                Forward x, 
                Turn (-(2*pi)/3), 
                Forward (x/2),
                PenUp, 
                Turn (-(pi/2))
              ] 

-- | The triangle function return the commands to draw polygon of a given
-- | degree and side length
polygon :: Int -> Double -> [TurtleCommand]
polygon n l 
  | n < 3     = error "Polygon must have 3 or more sides"
  | otherwise = Turn (-(pi/2)) : Forward (l/2) : (p (2*pi/(fromIntegral n)) n)
    where 
      p a 1 = Turn a : Forward (l/2) : PenUp : [Turn (pi/2)]
      p a x = Turn a : Forward l : p a (x-1)

-- Task 2: Interpreting Turtle Commands

type Angle = Double
type CurrentPoint = Point
type TurtleState = (Angle, CurrentPoint, [Point])

-- Initial state defined the turtle state before any turtle commands
initialState :: TurtleState
initialState = ((pi/2), (0,0), [(0,0)])

-- | The 'getPoints' function turns Turtle commands a list of points
getPoints :: TurtleState -> Bool -> [TurtleCommand] -> [[Point]]
getPoints _ _ [] = []
getPoints (angle, (x, y), points) penDown (cmd:cs) = case cmd of
  PenDown       -> getPoints (angle, (x,y), points++[(x,y)]) True cs 
  PenUp         ->  points : getPoints (angle, (x,y), points) False cs
  Turn radians  -> getPoints ((angle+radians), (x,y), points) penDown cs
  Forward d     | penDown == True -> getPoints 
                  (angle, newpoint d, points++[newpoint d] ) penDown cs 
                | otherwise -> getPoints (angle, (newpoint d), [newpoint d])
                  penDown cs   
  where
    newpoint d = ( x+d*(cos angle), y+d*(sin angle) )         

-- | The 'runTurtle' function turns Turtle commands into a picture that can be 
-- | interpreted by CodeWorld. 
runTurtle :: [TurtleCommand] -> Picture
runTurtle x = runTurtle' (getPoints initialState True x)
  where
    runTurtle' []     = blank
    runTurtle' (points:xs) = polyline points & runTurtle' xs

-- Task 3: Sierpinski's Triangle

type Rules a  = [(a, [a])]
type InitialStr a = [a]

type LSystem a = (InitialStr a, Rules a)
type SierpinskiAlphabet = Char

-- | The 'interpretLSysten' function expands and LSystem n times
interpretLSystem :: Eq a => LSystem a -> Int -> [a]
interpretLSystem (is,_) 0 = is
interpretLSystem (is,pr) d = interpretLSystem (expand is pr, pr) (d-1)
    where
    expand [] _ = [] 
    expand (x:xs) rules = applyRules x rules ++ expand xs rules
    applyRules x [] = [x]
    applyRules x ((match,expansion):rules) 
      | x == match    = expansion
      | otherwise = applyRules x rules

sierpinskiSystem :: LSystem SierpinskiAlphabet
sierpinskiSystem = ("F-G-G", [('F', "F-G+F+G-F"), ('G', "GG")]) 

-- | The 'sierpinski' function interprets a sierpinskiSystem as turtle commands
sierpinski :: Int -> Double -> [TurtleCommand]
sierpinski d n = [PenDown] ++ s (interpretLSystem sierpinskiSystem d) 
  where
    s [] = [PenUp]
    s (x:xs) 
      | x == 'G' || x == 'F' = Forward n : s xs
      | x == '+'  = Turn ((2*pi)/3) : s xs
      | otherwise = Turn (-(2*pi)/3) : s xs

sierpinskiArrowHeadSystem :: LSystem Char
sierpinskiArrowHeadSystem = ("A", [('A', "B-A-B"), ('B', "A+B+A")])

-- | The 'sierpinskiArrowHead' function interprets amsierpinskiArrowHeadSystem
-- | as turtle commands.
sierpinskiArrowHead :: Int -> [TurtleCommand]
sierpinskiArrowHead d = s (interpretLSystem sierpinskiArrowHeadSystem d)       
  where
    s [] = [PenUp]
    s (x:xs) 
      | x == 'A' || x == 'B' = Forward 0.5 : s xs
      | x == '+'  = Turn (pi/3) : s xs
      | otherwise = Turn (-pi/3) : s xs 

-- | The lSystemCommands functions returns a list of commands to draw a
-- | sierpinskiArrowHead lsystem.
lSystemCommands :: [TurtleCommand]
lSystemCommands = PenUp : Turn (pi/2) : Forward 5 : Turn (pi/2) : 
  Forward 8 : Turn (pi) : PenDown : sierpinskiArrowHead 5

-- | A more complex example to test your interpreter.
comp1100 :: [TurtleCommand]
comp1100 = concat [start, c, o, m, p, one, one, o, o]
  where
    start = [PenUp, f 1.5, l, f 9.25, r]
    c =
      [ r, f 1.5
      , l, f 0.5, PenDown
      , l, l', f d
      , r', f 1
      , r', f d
      , r', f 2
      , r', f d
      , r', f 1
      , r', f d, PenUp
      , r', f 2.5
      , l, f 1
      , l
      ]
    o =
      [ r, f 1.5
      , l, f 0.5, PenDown
      , l, l', f d
      , r', f 1
      , r', f d
      , r', f 2
      , r', f d
      , r', f 1
      , r', f d
      , r', f 2, PenUp
      , f 0.5
      , l, f 1
      , l
      ]
    m =
      [ l, f 0.5, r, PenDown
      , f 3
      , r, r', f (d * 2)
      , l, f (d * 2)
      , r, r', f 3, PenUp
      , l, f 1, l
      ]
    p =
      [ l, f 0.5, r, PenDown
      , f 2.5
      , r', f d
      , r', f 1
      , r', f d
      , r', f 1
      , r', f d
      , r', f 1
      , r', f d, PenUp
      , r, r', f 3
      , r, f 1.5
      , l, l
      ]
    one =
      [ PenDown
      , r, f 1
      , l, l, f 0.5
      , r, f 3
      , l, l', f d, PenUp
      , f d
      , l', f 2
      , l, f 2.5
      , l
      ]

    f = Forward

    -- Left/Right turns, 90 degrees. Primed versions (the ones with an
    -- ' after: l', r') are 45 degrees.
    l = Turn (pi / 2)
    l' = Turn (pi / 4)
    r = Turn (-pi / 2)
    r' = Turn (-pi / 4)

    -- Diagonal length of a right-angle triangle with both sides 0.5
    d = sqrt (2 * 0.5 * 0.5)
