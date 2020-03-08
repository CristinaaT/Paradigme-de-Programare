{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

charToString :: Char -> String
charToString c = [c]

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}
cellPosition :: Cell -> Position
cellPosition (HardC p) = p
cellPosition (SoftC p) = p
cellPosition (BlockC p) = p
cellPosition (SwitchC p) = p
cellPosition (EmptyC p) = p
cellPosition (WinningC p) = p

-- Un cell poate fi de tip: hard, soft, block, switch, empty sau winning si contine si un atribut de tip Position pentru
-- a-i determina pozitia
data Cell = HardC Position | SoftC Position | BlockC Position | SwitchC Position | EmptyC Position | WinningC Position
    deriving (Eq, Ord)

instance Show Cell where
    show (HardC _) = [hardTile]
    show (SoftC _) = [softTile]
    show (BlockC _) = [block]
    show (SwitchC _) = [switch]
    show (EmptyC _) = [emptySpace]
    show (WinningC _) = [winningTile]

-- Functii de verificare a tipului de Cell
isWinningCell :: Cell -> Bool
isWinningCell (WinningC _) = True
isWinningCell _ = False

isEmptyCell :: Cell -> Bool
isEmptyCell (EmptyC _) = True
isEmptyCell _ = False

isHardCell :: Cell -> Bool
isHardCell (HardC _) = True
isHardCell _ = False

isSoftCell :: Cell -> Bool
isSoftCell (SoftC _) = True
isSoftCell _ = False

{-
    Tip de date pentru reprezentarea nivelului curent
	
	Un nivel este definit prin:
		* matricea ce celule
		* pozitia switchurilor si a celulelor pe care le activeaza fiecare switch
		* pozitia blocului controlat de jucator
		* starea jocului (castigat, pierdut, inconcludent)
-}
data Level = MLevel (A.Array Position Cell, [(Position, [Position])], (Position, Position), Int)
    deriving (Eq, Ord)

{-
    String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

-- Convertim matricea tinuta in Array intr-o lista de linii. O linie este la randul ei o lista de celule
matrixOf :: A.Array Position Cell -> [[Cell]]
matrixOf arr = [[arr A.! (x, y) | y<-[0..highy]] | x<-[0..highx]]
    where (_, (highx, highy)) =  A.bounds arr

-- In functie de starea jocului, mai adaugam un mesaj la sfarsit, dupa ce afisam tabla de joc
finalMessage :: Int -> String
finalMessage gameState
	| gameState == 1 = "Congrats! You won!\n"
	| gameState == -1 = "Game Over\n"
	| otherwise = ""

-- Convertim Array-ul la o matrice pe care apoi o parcurgem cu foldl pentru a genera stringul pentru Show
instance Show Level where
    show (MLevel (m, _, (pos1, pos2), gameState)) = (foldl 
    	(\acc x -> acc ++ (foldl 
    		(\acc2 x2 -> if elem (cellPosition x2) [pos1, pos2] 
    			then acc2 ++ show (BlockC (0,0))
    			else acc2 ++ show x2)
    		""
    		x
    	) ++ "\n") 
    	"\n"
    	(matrixOf m)) ++ finalMessage gameState

{-
    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}
emptyLevel :: Position -- Dimensiunea tablei de joc
		-> Position    -- Pozitia blocului
		-> Level       -- Nivelul returnat
emptyLevel (boardX, boardY) (blockX, blockY) = MLevel ((A.array ((0, 0), (boardX, boardY)) [((x,y), (EmptyC (x, y))) | x <- [0..boardX], y <-[0..boardY]]), [], ((blockX, blockY), (-1, -1)), 0) 


{-
    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}
addTile :: Char  -- Tipul celulei care va fi adaugate
	-> Position  -- Pozitia la care se adauga celula
	-> Level     -- Nivelul la care se adauga celula
	-> Level     -- Nivelul returnat, cu noua celula
addTile tile pos (MLevel (arr, swArr, blockPos, gameState))
		| tile == 'H' = MLevel ((arr A.// [(pos, (HardC pos))]), swArr, blockPos, gameState)
		| tile == 'S' = MLevel ((arr A.// [(pos, (SoftC pos))]), swArr, blockPos, gameState)
		| otherwise = MLevel ((arr A.// [(pos, (WinningC pos))]), swArr, blockPos, gameState)


{-
    Adaugă o celulă de tip Switch în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}
addSwitch :: Position   -- Pozitia switchului pe tabla
		-> [Position]   -- Pozitiile celulelor care apar/dispar la activarea/dezactivarea switchului
		-> Level        -- Nivelul la care se adauga switch-ul
		-> Level        -- Nivelul rezultat
addSwitch swPos hiddenTilesPos (MLevel (arr, swArr, blockPos, gameState)) = MLevel ((arr A.// [(swPos, (SwitchC swPos))]), newSwArr, blockPos, gameState)
	where newSwArr = (swPos, hiddenTilesPos) : swArr

{-
    === MOVEMENT ===
-}

{-
    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

-- Seteaza o celula din array pe modul activat sau dezactivat, in functie de starea anterioara
flipCell :: A.Array Position Cell   -- Array-ul care contine tabla de joc
		-> Position                 -- Pozitia la care se afla celula ce trebuie activata/dezactivata
		-> A.Array Position Cell    -- Noua tabla de joc
flipCell arr pos
	| isHardCell (arr A.! pos) = arr A.// [(pos, (EmptyC pos))]
	| isEmptyCell (arr A.! pos) = arr A.// [(pos, (HardC pos))]
	| otherwise = arr

-- Itereaza prin lista de celule care trebuie inversate la activarea unui switch
flipCells :: A.Array Position Cell   -- Array-ul care contine tabla de joc
		-> [Position]                -- Pozitiile la care se afla celule ce trebuie activate/dezactivate
		-> A.Array Position Cell     -- Noua tabla de joc
flipCells arr tilePos = foldl flipCell arr tilePos

-- Activeaza un switch primit ca parametru pentru un nivel dat, verificand mai intai daca blocul il apasa.
activate :: Level                   -- Nivelul la care se verifica tabla de joc
		-> (Position, [Position])   -- Pereche (pozitie switch, [pozitii celule mapate la switch])
		-> Level                    -- Noul nivel returnat
activate (MLevel (cells, switches, (bpos1, bpos2), gameState)) (swpos, tilesPos)
	| elem swpos [bpos1, bpos2] = MLevel (newCells, switches, (bpos1, bpos2), gameState) 
	| otherwise = (MLevel (cells, switches, (bpos1, bpos2), gameState))
		where newCells = flipCells cells tilesPos


{-
    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

-- Verifica daca un joc s-a castigat. (pos1, pos2) reprezinta pozitia blocului
gameWon :: A.Array Position Cell -> (Position, Position) -> Bool
gameWon arr (pos1, pos2) = and [isWinningCell (arr A.! pos1), pos2 == (-1, -1)]

-- Verifica daca un joc s-a pierdut. (pos1, pos2) reprezinta pozitia blocului
gameLost :: A.Array Position Cell -> (Position, Position) -> Bool
gameLost arr (pos1, pos2) = if pos2 == (-1, -1) then or [isSoftCell (arr A.! pos1), isEmptyCell (arr A.! pos1)]
	else or [isEmptyCell (arr A.! pos1), isEmptyCell (arr A.! pos2)]

-- Returneaza statusul jocului: daca e castigat -> 1, daca e pierdut -> -1 si altfel -> 0
getGameState :: A.Array Position Cell -> (Position, Position) -> Int
getGameState arr blockPos
	| gameWon arr blockPos = 1
	| gameLost arr blockPos = -1
	| otherwise = 0

-- Returneaza noua pozitie pentru o mutare intr-o anumita directie de N pasi
-- Observatie: Axele x si y sunt inversate, insemnand ca x incrementeaza in jos iar y incrementeaza spre dreapta
movePos :: Position     -- Pozitia initiala
		-> Directions   -- Directia mutarii
		-> Int          -- Numarul de pasi de mutat
		-> Position     -- Noua pozitie
movePos (x, y) dir n
	| dir == North = (x - n, y)
	| dir == South = (x + n, y)
	| dir == West = (x, y - n)
	| otherwise = (x, y + n)

-- Blocul este in picioare, trebuie sa il mutam prin culcare la pamant
move1 :: Directions -> Level -> Level
move1 dir (MLevel (cells, switches, (bPos, _), _)) = (MLevel (cells, switches, (movePos bPos dir 1, movePos bPos dir 2), newGameState))
	where newGameState = getGameState cells (movePos bPos dir 1, movePos bPos dir 2)

-- Blocul este la pamant orizontal, verificam daca mutarea il rostogoleste in picioare sau tot la pamant
move2Horizontal :: Directions -> Level -> Level
move2Horizontal dir (MLevel (cells, switches, ((x1, y1), (x2, y2)), _))
	| or [dir == North, dir == South] = (MLevel (cells, switches, (movePos (x1, y1) dir 1, movePos (x2, y2) dir 1), getGameState cells (movePos (x1, y1) dir 1, movePos (x2, y2) dir 1)))
	| dir == West = (MLevel (cells, switches, (movePos (x1, minimum [y1, y2]) dir 1, (-1, -1)), getGameState cells (movePos (x1, minimum [y1, y2]) dir 1, (-1, -1))))
	| otherwise = (MLevel (cells, switches, (movePos (x1, maximum [y1, y2]) dir 1, (-1, -1)), getGameState cells (movePos (x1, maximum [y1, y2]) dir 1, (-1, -1)) ))

-- Blocul este la pamant vertical, verificam daca mutarea il rostogoleste in picioare sau tot la pamant
move2Vertical :: Directions -> Level -> Level
move2Vertical dir (MLevel (cells, switches, ((x1, y1), (x2, y2)), _))
	| or [dir == West, dir == East] = (MLevel (cells, switches, (movePos (x1, y1) dir 1, movePos (x2, y2) dir 1), getGameState cells (movePos (x1, y1) dir 1, movePos (x2, y2) dir 1)))
	| dir == North = (MLevel (cells, switches, (movePos (minimum [x1, x2], y1) dir 1, (-1, -1)), getGameState cells (movePos (minimum [x1, x2], y1) dir 1, (-1, -1))))
	| otherwise = (MLevel (cells, switches, (movePos (maximum [x1, x2], y1) dir 1, (-1, -1)), getGameState cells (movePos (maximum [x1, x2], y1) dir 1, (-1, -1))))

-- Blocul este la pamant, verificam ce orientare are si apelam functia corespunzatoare
move2 :: Directions -> Level -> Level
move2 dir (MLevel (cells, switches, ((x1, y1), (x2, y2)), gameState))
	| x1 == x2 = move2Horizontal dir (MLevel (cells, switches, ((x1, y1), (x2, y2)), gameState))
	| otherwise = move2Vertical dir (MLevel (cells, switches, ((x1, y1), (x2, y2)), gameState))

-- Verifica daca blocul este la pamant sau in picioare, si trateaza cazurile separat
-- Dupa verifica daca se apasa vreun switch
move :: Directions -> Level -> Level
move dir (MLevel (cells, switches, (pos1, pos2), gameState))
	| pos2 == (-1, -1) = foldl activate (move1 dir (MLevel (cells, switches, (pos1, pos2), gameState))) switches
	| otherwise = foldl activate (move2 dir (MLevel (cells, switches, (pos1, pos2), gameState))) switches

{-
    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}
continueGame :: Level -> Bool
continueGame (MLevel (_, _, _, gameState)) = gameState == 0

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors = undefined

    isGoal = undefined

    -- Doar petru BONUS
    -- heuristic = undefined
