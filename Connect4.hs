import Data.List

-- connect 4 board is 7x6
data Piece = Red | Yellow deriving (Show)
data Row = R0 | R1 | R2 | R3 | R4 | R5 deriving (Show, Eq)
data Col = A | B | C | D | E | F | G deriving (Show, Eq)
data Token = Token { row :: Row
                   , column :: Col
                   , piece :: Piece } deriving (Show) 

main :: IO ()
main = connect4

connect4 :: IO ()
connect4 = do
  putStrLn "Welcome to Connect λ"
  putStrLn "\nPress ENTER to begin..."
  _ <- getLine
  gameLoop []

gameLoop :: [Token] -> IO ()
gameLoop tokens = do
  move <- getCommand tokens
  putStrLn ("You picked " ++ [move])
  let token = getToken Red move
  putStrLn ("Token: " ++ (show token))
  nextLoop token tokens

nextLoop :: Maybe Token -> [Token] -> IO ()
nextLoop (Just token) xs = gameLoop (token: xs)
nextLoop Nothing xs      = gameLoop xs


getCommand :: [Token] -> IO Char
getCommand t = do
  putStrLn "\ESC[2J"
  showBoard t
  putStrLn "Player 1, choose a column:"
  move <- getChar
  return move

showBoard :: [Token] -> IO ()
showBoard t = do
  putStrLn "ABCDEFG"
  putStrLn (intercalate "\n" (map (printRow t) getRows))

printRow :: [Token] -> Row -> [Char]
printRow t r = map (getCharFromPosition r t) getCols

getCharFromPosition :: Row -> [Token] -> Col -> Char
getCharFromPosition r t c = mapTokenToChar (findToken t r c)

mapTokenToChar :: Maybe Token -> Char
mapTokenToChar Nothing = '0'
mapTokenToChar (Just Token{piece=Red}) = 'X'
mapTokenToChar (Just Token{piece=Yellow}) = '@'

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
