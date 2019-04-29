module Lib where

import Graphics.UI.GLUT
import Data.IORef
import Data.List
import System.Random
import System.Exit
import Data.Time

width = 10 :: Int
height = 20 :: Int
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

type Cell = (Int, Int)
type Cluster = [Cell]
data Mino = BMino | LMino | RMino | ZMino | SMino | TMino | OMino deriving (Eq, Enum)
data DirectOfMino = Ue | Hidari | Sita | Migi deriving (Eq, Enum)
type StateOfMino = (Mino, Cell, DirectOfMino)
data State = State {somino :: StateOfMino, cluster :: Cluster, hold :: [Mino], nexts :: ([Mino], StdGen), nextCount :: Int, score :: Int, mode :: Bool}

display :: IORef State -> DisplayCallback
display state = do
  clear [ColorBuffer]
  som <- somino <$> get state
  clu <- cluster <$> get state
  hol <- hold <$> get state
  nex <- fst . nexts <$> get state
  nec <- fromIntegral . nextCount <$> get state
  sco <- show . score <$> get state
  mo <- mode <$> get state
  loadIdentity
  if mo then do
    color (Color3 0.25 0.5 0.25 :: Color3 GLdouble)
    renderPrimitive Polygon . mapM_ vertex2d $ [Vertex2 (0.1875*cos x-0.8125) (0.775+0.15*sin x) | x <- [0,0.1..2*pi]]
    color (Color3 0.5 0.25 0.25 :: Color3 GLdouble)
    renderPrimitive Polygon . mapM_ vertex2d $ [Vertex2 (0.8125+0.1875*cos x) (0.775+0.15*sin x) | x <- [0,0.1..2*pi]]
    color (Color3 (1-nec*0.1) (nec*0.1) (nec*0.05) :: Color3 GLdouble)
    mapM_ (renderPrimitive LineLoop . mapM_ vertex2d) $ map cellGraph [(x,y) | x <- [1..width], y <- [1..height]]
    preservingMatrix $ do
      scale (0.001::Double) 0.001 0.001
      w <- stringWidth Roman "Stroke font"
      translate (Vector3 (-0.5*(fromIntegral w)-520) 400 0 ::Vector3 Float)
      renderString Roman (init . init $ show nec)
    color (Color3 1 0 1 :: Color3 GLdouble)
    mapM_ (renderPrimitive Polygon . mapM_ vertex2d) $ map cellGraph (mino som)
    color (Color3 0.7 0.7 0.75 :: Color3 GLdouble)
    mapM_ (renderPrimitive Polygon . mapM_ vertex2d) $ map cellGraph clu
    color (Color3 1 1 0 :: Color3 GLdouble)
    mapM_ (renderPrimitive Polygon . mapM_ vertex2d) $ holdGraph hol
    color (Color3 0 1 1 :: Color3 GLdouble)
    mapM_ (mapM_ (renderPrimitive Polygon . mapM_ vertex2d)) $ nextGraph nex
    preservingMatrix $ do
      scale (0.001::Double) 0.001 0.001
      w <- stringWidth Roman "Stroke font"
      translate (Vector3 (-0.5*(fromIntegral w)-570) (-800) 0 ::Vector3 Float)
      renderString Roman sco
  else do
    color (Color3 1 1 1 :: Color3 GLdouble)
    preservingMatrix $ do
      scale (0.001::Double) 0.001 0.001
      w <- stringWidth Roman "Stroke font"
      translate (Vector3 (-0.5*(fromIntegral w)-550) 0 0 ::Vector3 Float)
      renderString Roman "Press 'R' to play the game"
  flush
    where vertex2d = vertex :: Vertex2 GLdouble -> IO ()

keyboard :: IORef State -> KeyboardCallback
keyboard state c _ = do
  case c of
    'p' -> exitSuccess
    'j' -> state $~! (\(State som clu hol nex nec sco mo) -> State (jkey som clu) clu hol nex nec sco mo)
    'k' -> state $~! (\(State som clu hol nex nec sco mo) -> State (kkey som clu) clu hol nex nec sco mo)
    'l' -> state $~! (\(State som clu hol nex nec sco mo) -> State (lkey som clu) clu hol nex nec sco mo)
    'a' -> state $~! (\(State som clu hol nex@(m1:_,_) nec sco mo) -> State (akeyCurrent som hol m1) clu (akeyHold som hol) (akeyNexts hol nex) nec sco mo)
    's' -> state $~! (\(State som clu hol nex nec sco mo) -> State (skey som clu) clu hol nex nec sco mo)
    'd' -> state $~! (\(State som@(m,_,_) clu hol (m1:m5,g) _ sco mo) -> State (m1, (div width 2,height+2), Ue) (erase $ clu ++ mino (under som clu)) hol ((\(x,y) -> (m5++[x], y)) (randomMino g)) (level sco) (sco + (let diff = highest clu - highest (erase $ clu ++ mino (under som clu)) in if diff < 0 then 0 else scoreList diff)) mo)
    'r' -> state $~! (\(State (m,_,_) _ _ nex _ _ mo) -> State (m, (div width 2,height+2), Ue) [] [] nex (level 0) 0 (not mo))
    _ -> return ()
  where
    succDir d = if d == Migi then Ue else succ d
    jkey (m,(x,y),d) clu = if touch (m,(x-1,y),d) clu then (m,(x,y),d) else (m,(x-1,y),d)
    kkey (m,(x,y),d) clu = if touch (m,(x,y-1),d) clu then (m,(x,y),d) else (m,(x,y-1),d)
    lkey (m,(x,y),d) clu = if touch (m,(x+1,y),d) clu then (m,(x,y),d) else (m,(x+1,y),d)
    akeyCurrent (m,(x,y),d) hol m1 = if null hol then (m1,(div width 2,height+2),Ue) else (head hol,(div width 2,height+2),Ue)
    akeyHold (m,(x,y),d) hol = [m]
    akeyNexts hol nex@(m1:m5,g) = if null hol then (\(x,y) -> (m5++[x], y)) (randomMino g) else nex
    skey (m,(x,y),d) clu = if touch (m,(x,y),succDir d) clu then (m,(x,y),d) else (m,(x,y),succDir d)
    under som clu = last . take height $ iterate (\(m,(x,y),d) -> if touch (m,(x,y-1),d) clu then (m,(x,y),d) else (m,(x,y-1),d)) som

timer :: IORef State -> IO ()
timer state = do
  ending <- endOfGame . cluster <$> get state
  sco <- score <$> get state
  mo <- mode <$> get state
  if mo then state $~! (\(State som@(mi,_,_) clu hol (m1:m5,g) nec sco mo) -> State (fall som clu nec m1) (fixate som clu nec) (newHold clu hol) ((\(x,y) -> (m5++[x], y)) (randomMino g)) (nextCountdown clu nec sco) (gameScore som clu nec sco) (gameSet clu mo))
  else state $~! (\(State (m,_,_) _ _ nex _ _ mo) -> State (m, (div width 2,height+2), Ue) [] [] nex (level 0) 0 False)
  if ending then print sco else return () where
  fall (m,(x,y),d) clu nec m1
    | nec == 0 = (m1,(div width 2,height+2),Ue)
    | touch (m,(x,y-1),d) clu = (m,(x,y),d)
    | otherwise = (m,(x,y-1),d)
  nextCountdown clu nec sco
    | endOfGame clu = level 0
    | nec == 0 = level sco
    | otherwise = nec-1
  newHold clu hol = if endOfGame clu then [] else hol
  fixate som clu nec
    | endOfGame clu = []
    | nec == 0 = erase $ clu ++ mino (under som clu)
    | otherwise = clu
  under som clu = last . take height $ iterate (\(m,(x,y),d) -> if touch (m,(x,y-1),d) clu then (m,(x,y),d) else (m,(x,y-1),d)) som
  gameScore som clu nec sco
    | endOfGame clu = 0
    | otherwise = sco + (let diff = highest clu - highest (fixate som clu nec) in if diff < 0 then 0 else scoreList diff)
  gameSet clu mo
    | endOfGame clu = False
    | otherwise = True

timerProc :: IO a -> TimerCallback
timerProc m = m >> addTimerCallback 480 (timerProc m)

resize :: Size -> IO ()
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  loadIdentity

idle :: IdleCallback
idle = postRedisplay Nothing

-----------------------------------------------------------------------------------
cellGraph :: Cell -> [Vertex2 GLdouble]
cellGraph (x, y) = [
  Vertex2 ((x_-1)*wd) (y_*hd), Vertex2 (x_*wd) (y_*hd), Vertex2 (x_*wd) ((y_-1)*hd), Vertex2 ((x_-1)*wd) ((y_-1)*hd)] where
  x_ = fromIntegral x - w/2 :: GLdouble
  y_ = fromIntegral y - h/2 :: GLdouble
  wd = 2/(w+6)
  hd = 2/h

holdGraph :: [Mino] -> [[Vertex2 GLdouble]]
holdGraph [] = []
holdGraph [m] = map (map (\(Vertex2 x y) -> (Vertex2 (x*3/5-1) (y*3/5+0.9))) . cellGraph) $ mino (m,(8,8),Ue)

nextGraph :: [Mino] -> [[[Vertex2 GLdouble]]]
nextGraph [m1,m2,m3,m4,m5] = [
  map (map (\(Vertex2 x y) -> (Vertex2 (x*3/5+0.625) (y*3/5+0.9))) . cellGraph) (mino (m1,(8,8),Ue)),
  map (map (\(Vertex2 x y) -> (Vertex2 (x*3/5+0.625) (y*3/5+0.65))) . cellGraph) (mino (m2,(8,8),Ue)),
  map (map (\(Vertex2 x y) -> (Vertex2 (x*3/5+0.625) (y*3/5+0.4))) . cellGraph) (mino (m3,(8,8),Ue)),
  map (map (\(Vertex2 x y) -> (Vertex2 (x*3/5+0.625) (y*3/5+0.15))) . cellGraph) (mino (m4,(8,8),Ue)),
  map (map (\(Vertex2 x y) -> (Vertex2 (x*3/5+0.625) (y*3/5-0.1))) . cellGraph) (mino (m5,(8,8),Ue))]

-----------------------------------------------------------------------------------
touch :: StateOfMino -> Cluster -> Bool
touch som clu = not . null $ intersect (mino som) (clu ++ waku) where
  waku = [(x,y) | x <- [0..width], y <- [0..height+5], x*y == 0] ++ [(width+1,y) | y <- [0..height+5]]

erase :: Cluster -> Cluster
erase clu = concat . zipWith (\n k -> map (\(x,y) -> (x,n)) k) [1..] . filter (\k -> length k < width) . groupBy (\u v -> snd u == snd v) $ sortOn snd clu

randomMino :: StdGen -> (Mino, StdGen)
randomMino gen = (\(x,y) -> (intToMino x, y)) (random gen :: (Int, StdGen))

random5Minos :: Int -> [Mino]
random5Minos n = (take 5 . map intToMino) (randoms (mkStdGen n) :: [Int])

endOfGame :: Cluster -> Bool
endOfGame clu = height < highest clu

highest :: Cluster -> Int
highest clu = (\x -> if x == [] then 0 else maximum x) (map snd clu)

intToMino :: Int -> Mino
intToMino m
  | n == 0 = BMino
  | n == 1 = LMino
  | n == 2 = RMino
  | n == 3 = ZMino
  | n == 4 = SMino
  | n == 5 = TMino
  | n == 6 = OMino
  where n = m `mod` 7

level :: Int -> Int
level sco
  | sco <  10 = 10
  | sco <  25 = 9
  | sco <  45 = 8
  | sco <  70 = 7
  | sco < 100 = 6
  | sco < 135 = 5
  | sco < 175 = 4
  | sco < 220 = 3
  | sco < 270 = 2
  | otherwise = 1

scoreList :: Int -> Int
scoreList d
  | d == 0 = 0
  | d == 1 = 1
  | d == 2 = 3
  | d == 3 = 6
  | d == 4 = 10

mino :: StateOfMino -> Cluster
mino (mi,c,d)
  | d ==     Ue = map (vplus c) $ originMino mi
  | d == Hidari = map (vplus c . rotate1) $ originMino mi
  | d ==   Sita = map (vplus c . rotate2) $ originMino mi
  | d ==   Migi = map (vplus c . rotate3) $ originMino mi
  where
    originMino m
      | m == BMino = [( 0, 2),( 0, 1),( 0, 0),( 0,-1)]
      | m == LMino = [( 0, 1),( 0, 0),( 0,-1),( 1,-1)]
      | m == RMino = [( 0, 1),( 0, 0),(-1,-1),( 0,-1)]
      | m == ZMino = [(-1, 1),( 0, 1),( 0, 0),( 1, 0)]
      | m == SMino = [( 0, 1),( 1, 1),(-1, 0),( 0, 0)]
      | m == TMino = [( 0, 1),(-1, 0),( 0, 0),( 1, 0)]
      | m == OMino = [( 0, 1),( 1, 1),( 0, 0),( 1, 0)]
    rotate1 (x,y) = (-y, x)
    rotate2 (x,y) = (-x,-y)
    rotate3 (x,y) = ( y,-x)
    vplus (p,q) (r,s) = (p+r,q+s)
