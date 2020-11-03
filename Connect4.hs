import Data.List
import Control.Concurrent
import System.Console.ANSI

-- connect 4 board is 7x6
data Piece = Red | Yellow deriving (Show, Eq)
data Row = R0 | R1 | R2 | R3 | R4 | R5 deriving (Show, Eq)
data Col = A | B | C | D | E | F | G deriving (Show, Eq)
data Token = Token { row :: Row
                   , column :: Col
                   , piece :: Piece } deriving (Show) 

data GameState = Moving | Playing | Win

main :: IO ()
main = do
  putStrLn "Welcome to Connect λ"
  connect4

connect4 :: IO ()
connect4 = do
  putStrLn "\nPress ENTER to begin..."
  _ <- getLine
  gameLoop Playing [] Main.Red

-- clears screen and starts loop
gameLoop :: GameState -> [Token] -> Piece -> IO ()
gameLoop state tokens player = do
  threadDelay 500000
  putStrLn "\ESC[2J"
  showBoard tokens
  gameStep state tokens player

-- decides flow between player move and animation
gameStep :: GameState -> [Token] -> Piece -> IO ()
gameStep Moving xs p = animateTokens xs p
gameStep Playing xs p = playerMove xs p
gameStep Win xs p = winner (rotatePlayer p)

winner :: Piece -> IO()
winner p = do
  putStrLn (setSGRCode [
      SetColor Foreground Dull System.Console.ANSI.White
    ] ++ ((show p) ++ " player wins!"))
  connect4

-- moves tokens and sets state to playing / moving
animateTokens :: [Token] -> Piece -> IO ()
animateTokens tokens p = do
  let updatedTokens = dropTokens tokens
  let state = getUpdatedState tokens updatedTokens
  gameLoop state updatedTokens p

-- return list of tokens where any that can move are 1 step lower
dropTokens :: [Token] -> [Token]
dropTokens (x: xs) =
  let nextRow = getNextRow x
      spaceContents = findToken xs nextRow (column x)
      nextToken = Token{
        row=nextRow, column=(column x), piece=(piece x) 
      }
  in if checkSpace spaceContents
    then (nextToken : xs)
    else (x: xs)

checkSpace :: Maybe Token -> Bool
checkSpace (Just _) = False
checkSpace Nothing  = True

getNextRow :: Token -> Row
getNextRow t = let r = nextRow (row t)
                in case r of
                  Nothing -> row t
                  Just row -> row


getUpdatedState :: [Token] -> [Token] -> GameState
getUpdatedState (x:_) (y:ys)
  | winningMove (y:ys)  = Win
  | row x == row y      = Playing
  | otherwise           = Moving

winningMove :: [Token] -> Bool
winningMove (x:xs)
  | countWest 0 x xs + countEast 0 x xs > 2 = True
  | countNorth 0 x xs + countSouth 0 x xs > 2 = True
  | countNorthEast 0 x xs + countSouthWest 0 x xs > 2 = True
  | countNorthWest 0 x xs + countSouthEast 0 x xs > 2 = True
  | countNorth 0 x xs + countSouth 0 x xs > 2 = True
  | otherwise = False

countWest :: Int -> Token -> [Token] -> Int
countWest n x xs = countDirection westToken n x xs

countEast :: Int -> Token -> [Token] -> Int
countEast n x xs = countDirection eastToken n x xs

countNorth :: Int -> Token -> [Token] -> Int
countNorth n x xs = countDirection northToken n x xs

countSouth :: Int -> Token -> [Token] -> Int
countSouth n x xs = countDirection southToken n x xs

countNorthEast :: Int -> Token -> [Token] -> Int
countNorthEast n x xs = countDirection northEastToken n x xs

countNorthWest :: Int -> Token -> [Token] -> Int
countNorthWest n x xs = countDirection northWestToken n x xs

countSouthWest :: Int -> Token -> [Token] -> Int
countSouthWest n x xs = countDirection southWestToken n x xs

countSouthEast :: Int -> Token -> [Token] -> Int
countSouthEast n x xs = countDirection southEastToken n x xs

playerMove :: [Token] -> Piece -> IO ()
playerMove tokens p = do
  putStrLn (setSGRCode [
      SetColor Foreground Dull System.Console.ANSI.White
    ]
    ++ ((show p) ++ " player, choose a column:"))
  move <- getCommand tokens
  putStrLn ("You picked " ++ [move])
  let token = getToken p move
  nextLoop token tokens p

nextLoop :: Maybe Token -> [Token] -> Piece -> IO ()
nextLoop (Just token) xs p 
  = gameLoop Moving (token: xs) (rotatePlayer p)
nextLoop Nothing xs p    = gameLoop Moving xs p

rotatePlayer :: Piece -> Piece
rotatePlayer Main.Red = Main.Yellow
rotatePlayer Main.Yellow = Main.Red


getCommand :: [Token] -> IO Char
getCommand t = do
  move <- getLine
  return (move!!0)

showBoard :: [Token] -> IO ()
showBoard t = do
  putStrLn (setSGRCode [
      SetColor Foreground Dull System.Console.ANSI.White
    ] ++ "ABCDEFG")
  putStrLn (intercalate "\n" (map (printRow t) getRows))

printRow :: [Token] -> Row -> String
printRow t r
  = intercalate "" (map (getCharFromPosition r t) getCols)

getCharFromPosition :: Row -> [Token] -> Col -> String
getCharFromPosition r t c = mapTokenToChar (findToken t r c)

mapTokenToChar :: Maybe Token -> String
mapTokenToChar Nothing = setSGRCode [
      SetColor Foreground Dull System.Console.ANSI.Cyan
    ]
    ++ "0"
mapTokenToChar (Just Token{piece=Main.Red}) 
  = setSGRCode [
      SetColor Foreground Dull System.Console.ANSI.Red
    ] 
    ++ "X"
mapTokenToChar (Just Token{piece=Main.Yellow}) 
  = setSGRCode [
      SetColor Foreground Dull System.Console.ANSI.Yellow
    ] 
    ++ "X"


findToken :: [Token] -> Row -> Col -> Maybe Token
findToken [] r c = Nothing
findToken (x: xs) r c
  | row x == r && column x == c = Just x
  | otherwise = findToken xs r c 


getToken :: Piece -> Char -> Maybe Token
getToken p 'A'  = Just Token{ row=R0, column=A, piece=p }
getToken p 'B'  = Just Token{ row=R0, column=B, piece=p }
getToken p 'C'  = Just Token{ row=R0, column=C, piece=p }
getToken p 'D'  = Just Token{ row=R1, column=D, piece=p }
getToken p 'E'  = Just Token{ row=R1, column=E, piece=p }
getToken p 'F'  = Just Token{ row=R1, column=F, piece=p }
getToken p 'G'  = Just Token{ row=R1, column=G, piece=p }
getToken _ _    = Nothing

getCols :: [Col]
getCols = [A, B, C, D, E, F, G]

getRows :: [Row]
getRows = [R0, R1, R2, R3, R4, R5]

prevRow :: Row -> Maybe Row
prevRow r 
  | r == R0   = Nothing
  | r == R1   = Just R0
  | r == R2   = Just R1
  | r == R3   = Just R2
  | r == R4   = Just R3 
  | r == R5   = Just R4

nextRow :: Row -> Maybe Row
nextRow r 
  | r == R0   = Just R1
  | r == R1   = Just R2
  | r == R2   = Just R3
  | r == R3   = Just R4
  | r == R4   = Just R5 
  | r == R5   = Nothing

prevCol :: Col -> Maybe Col
prevCol c
  | c == A  = Nothing
  | c == B  = Just A
  | c == C  = Just B
  | c == D  = Just C
  | c == E  = Just D
  | c == F  = Just E
  | c == G  = Just F

nextCol :: Col -> Maybe Col
nextCol c
  | c == A = Just B
  | c == B = Just C
  | c == C = Just D
  | c == D = Just E
  | c == E = Just F
  | c == F = Just G
  | c == G = Nothing

westToken :: Token -> [Token] -> Maybe Token
westToken x xs = findInRow prevCol x xs

eastToken :: Token -> [Token] -> Maybe Token
eastToken x xs = findInRow nextCol x xs

northToken :: Token -> [Token] -> Maybe Token
northToken x xs = findInCol prevRow x xs

southToken :: Token -> [Token] -> Maybe Token
southToken x xs = findInCol nextRow x xs

northEastToken :: Token -> [Token] -> Maybe Token
northEastToken x xs = findDiagonal nextRow prevCol x xs

northWestToken :: Token -> [Token] -> Maybe Token
northWestToken x xs = findDiagonal prevRow prevCol x xs

southWestToken :: Token -> [Token] -> Maybe Token
southWestToken x xs = findDiagonal prevRow nextCol x xs

southEastToken :: Token -> [Token] -> Maybe Token
southEastToken x xs = findDiagonal nextRow nextCol x xs

findInRow ::
  (Col -> Maybe Col) -> Token -> [Token] -> Maybe Token
findInRow f x xs  = let c = f (column x)
                    in case c of
                      Nothing -> Nothing
                      Just col -> findToken xs (row x) col

findInCol ::
  (Row -> Maybe Row) -> Token -> [Token] -> Maybe Token
findInCol f x xs = let r = f (row x)
                   in case r of
                      Nothing -> Nothing
                      Just row -> findToken xs row (column x)

findDiagonal ::
  (Row -> Maybe Row) -> (Col -> Maybe Col)
    -> Token -> [Token] -> Maybe Token
findDiagonal f f' x xs 
  = let r = f (row x)
        c = f' (column x)
    in case (r, c) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just row, Just col) -> findToken xs row col

countDirection :: 
  (Token -> [Token] -> Maybe Token) 
    -> Int -> Token -> [Token] -> Int
countDirection f n x xs
  | matchingColour (f x xs) x 
    = let (Just t) = f x xs
      in countDirection f (n + 1) t xs
  | otherwise = n

matchingColour :: Maybe Token -> Token -> Bool
matchingColour Nothing _ = False
matchingColour (Just t) token = piece token == piece t

