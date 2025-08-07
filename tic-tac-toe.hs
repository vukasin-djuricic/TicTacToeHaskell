import Control.Monad.State
import Data.List (elemIndices)
import Text.Parsec
import Text.Parsec.Char
import Data.Map (valid)
import Data.Maybe (mapMaybe)


{-
VEZBANJE

pretvori u stablo True False
fmap (\x -> if (mod x 2) == 0 then True else False) tree

saberi sve neparne u stablu
foldRose (\acc x -> if(mod x 2) == 1 then acc+x else acc) 0 tree1

saberiSveParne :: Integral a => RoseTree a -> a
saberiSveParne (Podatak x (deca))
 | mod x 2 == 0 = x + sum(map saberiSveParne deca)
 | otherwise = sum(map saberiSveParne deca)

pobedi X u 3 poteza
snd(runGameState applyMovesTest iksOksInitialState)
fst(runGameState applyMovesTest iksOksInitialState)
 elemsOnDepth 0 (fst (runGameState applyMovesTest iksOksInitialState))

 filter(  ( \x -> fst(validniPotezi x) == O && (zavrsnoStanje x)== )   elemsOnDepth 3 (fst (runGameState applyMovesTest iksOksInitialState)))

ghci> filter zavrsnoStanje (elemsOnDepth 3 (fst(runGameState applyMovesTest iksOksInitialState)))

take 10 (treeToList(fst(runGameState applyMovesTest iksOksInitialState)))

map jelImaPobednika (filter zavrsnoStanje (elemsOnDepth 3( fst(runGameState applyMovesTest iksOksInitialState))))
length (filter zavrsnoStanje (elemsOnDepth 3( fst(runGameState applyMovesTest iksOksInitialState))))



ISPISI SVE NA NIVOIMA [0..x]
map (\x -> elemsOnDepth x tree1) [0..5]


foldl (++) [] (map (\x -> elemsOnDepth x tree1) [0..5])

pobednicka stanja:
leavesCount( fst(runGameState applyMovesTest iksOksInitialState))
-}

filterRoseTree :: (a -> Bool) -> RoseTree a -> Maybe (RoseTree a)
filterRoseTree p (Podatak x deca)
  | p x =
      let filtriranaDeca = mapMaybe (filterRoseTree p) deca
      in Just (Podatak x filtriranaDeca)
  | otherwise =
      case mapMaybe (filterRoseTree p) deca of
        [] -> Nothing
        filtriranaDeca -> Just (Podatak x filtriranaDeca)


data RoseTree a = Podatak a [RoseTree a] deriving (Show,Eq)

--a) size - vraća broj čvorova stabla
--height - računa visinu stabla, odnosno najdužu putanju (broj grana) od korena do lista

size :: Num t => RoseTree a -> t
size (Podatak x []) = 1
size (Podatak x (y:ys)) = size (y) + sum( map size ys) +1

tree :: RoseTree Int
tree =
  Podatak 1
    [ Podatak 2 []
    , Podatak 3 [Podatak 5 []]
    , Podatak 4 []
    ]

tree1 :: RoseTree Int
tree1 =
  Podatak 1
    [ Podatak 2 []
    , Podatak 3 [Podatak 5 [Podatak 6 [Podatak 7 []]]]
    , Podatak 4 []
    ]

height (Podatak x []) = 1
height (Podatak x deca) = 1 + mojMax(map height deca)
        where mojMax (x:xs) = mojMax2 (x:xs) 0
                where -- samo max za niz obican
                    mojMax2 [] acc = acc                     
                    mojMax2 (x:xs) acc = mojMax2 xs (if x> acc then x else acc) -- a moglo i samo foldl

--b) leavesCount - vraća broj listova,
-- leaves - vraća listu koja sadrži vrednosti svih listova stabla

leavesCount :: Num t => RoseTree a -> t
leavesCount (Podatak x []) = 1
leavesCount (Podatak x deca) = sum (map leavesCount deca)

leaves (Podatak x []) = [x]
leaves (Podatak x deca) = concat (map leaves deca)


--foldRose (:) [] (fst (runGameState applyMovesTest iksOksInitialState))
treeToList (Podatak x []) = [x]
treeToList (Podatak x deca) = x : concat (map treeToList deca)
--(foldRose (:) [] (fst (runGameState applyMovesTest iksOksInitialState)))==(treeToList (fst (runGameState applyMovesTest iksOksInitialState)))

--c) elemsOnDepth - vraća vrednosti svih elemenat na određenoj dubini
elemsOnDepth 0 (Podatak x deca) = [x]
elemsOnDepth dubina (Podatak x deca) = concat (map (elemsOnDepth(dubina-1)) deca)

--d) instancirati tipsku klasu Functor za tip podataka Rose
instance Functor RoseTree where
    fmap f (Podatak x deca) = (Podatak (f x) (map (fmap f) deca))-- fmap (*2) tree

{-
    e) napisati funkciju foldRose koja izršava fold (levi ili desni) na svim čvorovima stabla tipa Rose (na
    primer ako imamo stablo sa celim brojevima i prosledimo funkciju (+) i akumulator 0 kao rezultat se
    vraća zbir svih čvorova)
-}


foldRose f acc (Podatak x deca) = foldr f acc (x: (dajPodatke deca))
        where 
            dajPodatke [] = []
            dajPodatke ((Podatak y ys) : rest) = y: dajPodatke ys ++ (dajPodatke rest)


--_________________________________2________________________________

data Potez a = Potez {igrac::Igrac , polje::(Int,Int), vrenost :: a} deriving Show
data Igrac = X | O deriving (Show, Eq)

data Tabla a = Tabla [[a]] deriving (Show,Eq)
data GameState a = GameState { stanjeTable:: a, sledeciIgra:: Igrac} deriving (Show, Eq)

--runState :: State s a -> s -> (a, s)
newtype GameStateOp b a  = GameStateOp { runGameState :: GameState b-> (a,GameState b) }
--todo primenim f na rez i akciju na stanje
instance Functor (GameStateOp b) where
  fmap f akcija = GameStateOp { runGameState = funkcija }
    where
      funkcija stanje =
        let (rez, novoStanje) = runGameState akcija stanje
        in (f rez, novoStanje)




instance Applicative (GameStateOp b) where--kombinujemo dve nezavisne gamestateop operacije
    pure x = GameStateOp ( \s -> (x,s))
    GameStateOp gf <*> GameStateOp ga = GameStateOp $ \stanje0 ->
        let (f, stanje1) = gf stanje0 --vraca funkciju
            (a, stanje2) = ga stanje1 -- argument
        in (f a, stanje2) -- primeni f na a i vrati novi rez+ novo stanje

instance Monad (GameStateOp b) where
    return x = GameStateOp (\s -> (x,s))
    (GameStateOp h) >>= f = GameStateOp (\s ->
                        let (a, newState) = h s
                            (GameStateOp g) = f a--monada g
                            in  g newState) -- izracunavanje g primenjujemo


newtype GameStateOpHistory b a =
  GameStateOpHistory { runGameStateH :: GameState b -> (a, GameState b, [GameState b]) }

instance Functor (GameStateOpHistory b) where
  fmap f (GameStateOpHistory h) = GameStateOpHistory $ \s ->
    let (a, s1, history) = h s
    in (f a, s1, history)

instance Applicative (GameStateOpHistory b) where
  pure x = GameStateOpHistory $ \s -> (x, s, [s])
  GameStateOpHistory ff <*> GameStateOpHistory fa = GameStateOpHistory $ \s ->
    let (f, s1, hist1) = ff s
        (a, s2, hist2) = fa s1
    in (f a, s2, hist2 ++ hist1)

instance Monad (GameStateOpHistory b) where
  return = pure
  GameStateOpHistory h >>= f = GameStateOpHistory $ \s ->
    let (a, s1, hist1) = h s
        GameStateOpHistory g = f a
        (b, s2, hist2) = g s1
    in (b, s2, hist2++ hist1)



--_____________________________________3________________________________
data Polje = Iks | Oks | Prazno deriving Eq

instance Show Polje where
    show Iks = "X"
    show Oks= "O"
    show Prazno = " "

tabla3x3 :: Tabla Polje
tabla3x3 = Tabla [[Prazno, Prazno, Prazno],[Prazno, Prazno, Prazno],[Prazno, Prazno, Prazno]]

ispis3x3 (Tabla redovi) =
  unlines (map ispisiRed redovi)
  where
    ispisiRed polja = "| " ++ concatMap (\p -> show p ++ " | ") polja


--praznaPolja (stanjeTable (snd (runGameState applyMoves iksOksInitialState)))
--runGameState applyMoves iksOksInitialState

{-
runGameState applyMoves iksOksInitialState
(False,GameState {stanjeTable = Tabla [[X,X, ],[O, , ],[ , , ]], sledeciIgra = O})
Haskell Interactive Shell (ispitniProjekat1.hs) λ praznaPolja (stanjeTable (snd (runGameState applyMoves iksOksInitialState)))
[(0,2),(1,1),(1,2),(2,0),(2,1),(2,2)] - RADI
-}
praznaPolja :: Tabla Polje -> [(Int, Int)]
praznaPolja (Tabla redovi) =
  [ (i, j)
  | (i, red) <- zip [0..] redovi--numerisem redove
  , j <- elemIndices Prazno red --elemIndices vraca indekse praznih polja u redu
  ]

validniPotezi tt@(Tabla redovi) = if (sum(map(count Iks)redovi) > sum(map(count Oks)redovi))
  then (O,praznaPolja tt) 
  else (X,praznaPolja tt)
    where count x red = length (filter (==x) red)


--validniPotezi (potez 0 0 Iks tabla3x3)
potez i j z (Tabla t) = Tabla
  [ if x == i then izmeniRed z red else red
  | (x, red) <- zip [0..] t ]
    where 
      izmeniRed z red = [ if y == j then z else val | (y, val) <- zip [0..] red ]
      -- mogao sam i sa take i drop


zavrsnoStanje t@(Tabla [[x00,x01,x02],[x10,x11,x12],[x20,x21,x22]])
    | x00 == x01 && x00 == x02 && x00 /= Prazno = True
    | x10 == x11 && x10 == x12 && x10 /= Prazno = True
    | x20 == x21 && x20 == x22 && x20 /= Prazno = True
    | x00 == x10 && x00 == x20 && x00 /= Prazno = True
    | x01 == x11 && x01 == x21 && x01 /= Prazno = True
    | x02 == x12 && x02 == x22 && x02 /= Prazno = True
    | x00 == x11 && x00 == x22 && x00 /= Prazno = True
    | x02 == x11 && x02 == x20 && x02 /= Prazno = True
    | praznaPolja (t) == [] = True
    | otherwise = False

jelImaPobednika t
  | zavrsnoStanje t == True = sledeci (fst(validniPotezi(t)))

--stabloMogucihPoteza (tabla3x3) X
--TEST sa jednim slobodnim poljem:
--stanjeTable (snd (runGameState applyMovesTest iksOksInitialState))
--stabloMogucihPoteza (stanjeTable (snd (runGameState applyMovesTest iksOksInitialState))) (sledeciIgra (snd (runGameState applyMovesTest iksOksInitialState)) )
applyMovesTest = do
  applyMove (0,0)--X
  applyMove (0,1)--O
  applyMove (0,2)--X
  applyMove (1,0)--O
  --applyMove (1,1)--X
  --applyMove (2,0)--O
  --applyMove (1,2)--X
  --applyMove (2,1) --O
  --applyMove (2,2) --X
  currentState <- GameStateOp $ \s -> (s, s)
  let tabla = stanjeTable currentState
  let igrac = sledeciIgra currentState
  return (stabloMogucihPoteza tabla igrac)
  

stabloMogucihPoteza tt@(Tabla t) igrac =
    let
      brojDece = length ( praznaPolja (Tabla t) ) -- praznaPolja niz tuplova
      deca = [potez i j (igracToPolje igrac) tt | (i, j) <- praznaPolja tt]--table dece
      sledeciIgrac = sledeci igrac
    in
      if brojDece /= 0 && not (zavrsnoStanje tt) 
      then Podatak tt (map (\tablaDeteta -> stabloMogucihPoteza tablaDeteta sledeciIgrac) deca)
      else Podatak tt []


{- ne menja igraca svaki put:
stabloMogucihPoteza tt@(Tabla t)=
    let
      brojDece = length ( praznaPolja (Tabla t) ) -- praznaPolja niz tuplova
      decaIks = [potez i j Iks tt | (i, j) <- praznaPolja tt]
      decaOks = [potez i j Oks tt | (i, j) <- praznaPolja tt]
      deca = decaIks ++ decaOks
    in
      if brojDece /= 0 then Podatak tt (map stabloMogucihPoteza deca)
      else Podatak tt []
-}

igracToPolje X = Iks
igracToPolje O = Oks

sledeci :: Igrac -> Igrac
sledeci X = O
sledeci O = X


applyMove :: (Int,Int) -> GameStateOp (Tabla Polje) Bool
applyMove (i,j)  = 
  GameStateOp $ \s -> 
    let rezultat = zavrsnoStanje (potez i j (igracToPolje (sledeciIgra s)) (stanjeTable s))
    in (rezultat, GameState {
      stanjeTable = potez i j (igracToPolje (sledeciIgra s)) (stanjeTable s) ,
      sledeciIgra = sledeci (sledeciIgra s)})

applyMoves = do
  applyMove (0,1)
  applyMove (1,0)
  applyMove (0,0)

iksOksInitialState = GameState {
  stanjeTable = tabla3x3,
  sledeciIgra = X
}

applyMoveH :: (Int,Int) -> GameStateOpHistory (Tabla Polje) Bool
applyMoveH (i,j)  = 
  GameStateOpHistory $ \s ->
    let rezultat = zavrsnoStanje (potez i j (igracToPolje (sledeciIgra s)) (stanjeTable s))
    in (rezultat, GameState {
      stanjeTable = potez i j (igracToPolje (sledeciIgra s)) (stanjeTable s) ,
      sledeciIgra = sledeci (sledeciIgra s)}, [GameState {
      stanjeTable = potez i j (igracToPolje (sledeciIgra s)) (stanjeTable s) ,
      sledeciIgra = sledeci (sledeciIgra s)}])

applyMovesH = do
  initialize
  applyMoveH (0,10)
  applyMoveH (1,0)
  applyMoveH (0,0)


initialize = GameStateOpHistory $ \s -> (False, iksOksInitialState, [iksOksInitialState])


{-}

parse iksoks "" testInput
Right (Tabla [[X,O, ],[ ,X, ],[ , , ]],[(O,(2,2)),(X,(1,0)),(O,(1,2))])
-}

iksoks :: Parsec String () (Tabla Polje)
iksoks = do
  -- Parse TABLU |X|O| |
  char '|' --parsepolje toString oobican
  red1 <- count 3(parsePolje <*char '|')
  endOfLine
  char '|'  
  red2 <- count 3(parsePolje <*char '|') 
  endOfLine
  char '|'
  red3 <- count 3(parsePolje <*char '|') 
  endOfLine
  let pocetnaTabla = Tabla [red1, red2,red3 ]
  
  -- parse potezee X (i,j)
  potezi <- many  (do
    igrac <- choice [char 'X' >> return X, char 'O' >> return O]
    spaces
    char '('
    x <- digit
    char ','
    y <- digit
    char ')'
    --spaces
    endOfLine
    return (igrac, (read [x], read [y])))
  
  eof
  --TODO promeniti sledeciIgra na osnovu count x count y
  -- sam skontati koji treba da igra po odnosu x i o - DONE
  let pocetnoStanje = GameState { stanjeTable = pocetnaTabla, sledeciIgra = fst(validniPotezi pocetnaTabla) } 
      finalnoStanje = primeniPoteze potezi pocetnoStanje
  
  case finalnoStanje of --hendluj za fail
    Just stanje -> return (stanjeTable stanje)
    Nothing -> fail "Nevalidni potezi"
  
  where
    parsePolje = choice 
      [ char 'X' >> return Iks
      , char 'O' >> return Oks  
      , char ' ' >> return Prazno
      ]
    
    --izmedju svakog poteza provera i WRAPOVANJE U KONTEKST
    primeniPoteze :: [(Igrac, (Int, Int))] -> GameState (Tabla Polje) -> Maybe (GameState (Tabla Polje))
    primeniPoteze [] gameState = Just gameState
    primeniPoteze ((igrac, (i, j)):ostaliPotezi) gameState = 
      if igrac == sledeciIgra gameState && potezValjan (i, j) (stanjeTable gameState)--validan
        then --mogao i sam da kreiram nov state sa novom tabelom i potezom
          let (_, novoStanje) = runGameState (applyMove (i, j)) gameState
          in primeniPoteze ostaliPotezi novoStanje
        else Nothing
    
    potezValjan (i, j) (Tabla tabela) = 
      i >= 0 && i < 3 && j >= 0 && j < 3 && 
      (tabela!! i !!j) ==Prazno --elem praznaPolja


--_________________________za history:


iksoksH :: Parsec String () (Tabla Polje, [GameState (Tabla Polje)])
iksoksH = do
  -- Parse TABLU |X|O| |
  char '|'
  red1 <- count 3(parsePolje <*char '|') <*endOfLine
  char '|'  
  red2 <- count 3(parsePolje <*char '|') <*endOfLine
  char '|'
  red3 <- count 3(parsePolje <*char '|') <*endOfLine
  let pocetnaTabla = Tabla [red1, red2,red3 ]
  
  -- parse potezee X (i,j)
  potezi <- many (do
    igrac <- choice [char 'X' >> return X, char 'O' >> return O]
    spaces
    char '('
    x <- digit
    char ','
    y <- digit
    char ')'
    endOfLine
    return (igrac, (read [x], read [y])))

  let pocetnoStanje = GameState { stanjeTable = pocetnaTabla, sledeciIgra = fst(validniPotezi pocetnaTabla) }
      (finalnaTabla, finalnoStanje, istorija) = runGameStateH (primeniPotezeH potezi) pocetnoStanje
  
  case finalnaTabla of
    Just tabla -> return (tabla, istorija)
    Nothing -> fail "Nevalidni potezi"
  
  where--tako da mi bude u monadskom kontekstu
    --vraca m onadu - pozivam u runGameStateH, sa razlikom na runGameState
    primeniPotezeH :: [(Igrac, (Int, Int))] -> GameStateOpHistory (Tabla Polje) (Maybe (Tabla Polje))
    primeniPotezeH [] = GameStateOpHistory $ \s -> (Just (stanjeTable s), s, [])--zbog Nothing->fail mogu prvi da koristim kao just Tabla umesto Bool
    primeniPotezeH ((igrac, (i,j)):rest) = GameStateOpHistory $ \s ->
      if igrac == sledeciIgra s && potezValjan (i, j) (stanjeTable s)
        then
          let (_, s1, h1) = runGameStateH (applyMoveH (i, j)) s
              (rez, s2, h2) = runGameStateH (primeniPotezeH rest) s1
          in (rez, s2, h2 ++ h1)
        else (Nothing, s, [])

    parsePolje = choice 
      [ char 'X' >> return Iks
      , char 'O' >> return Oks  
      , char ' ' >> return Prazno
      ]

    potezValjan (i, j) (Tabla tabela) = 
      i >= 0 && i < 3 && j >= 0 && j < 3 && 
      (tabela!! i !!j) ==Prazno --elem praznaPolja

    



-- Test input
testInput :: String
testInput = "|X|O| |\n| |X| |\n| | | |\nO (2,2)\nX (1,0)\nO(1,2)\n"
{-}
-- Test funkcija
testIksOks :: Maybe (Tabla Polje)
testIksOks = 
  case parse iksoks "test" testInput of
    Left _ -> Nothing
    Right finalnaTabla -> Just finalnaTabla
    -}


-- Test funkcija za prazna polja nakon poteza
testPraznaPolja :: [(Int, Int)]
testPraznaPolja = 
  let (_, finalnoStanje) = runGameState applyMoves iksOksInitialState
  in praznaPolja (stanjeTable finalnoStanje)

--test funkcija za finalno stanje table
testFinalnoStanje :: Tabla Polje
testFinalnoStanje = 
  let (_, finalnoStanje) = runGameState applyMoves iksOksInitialState
  in stanjeTable finalnoStanje


main :: IO ()
main = do
  input <- readFile "testInput.txt"
  
  case parse iksoks "" input of
    Left err -> putStrLn $ "greska: " ++ show err
    Right rezultat -> do
        print rezultat
        putStrLn $ "Zavrsno stanje: " ++ show (zavrsnoStanje rezultat)