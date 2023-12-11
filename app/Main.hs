{-# LANGUAGE NamedFieldPuns #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

type Position = (Float, Float)
type Velocity = (Float, Float)

data GameState = Playing | Lost deriving Eq

data World = World {
    circlePos :: Position,
    circleVel :: Velocity,
    radius :: Float,
    otherRadius :: Float,
    otherCircles :: [Position],
    evilCircles :: [Position],
    evilVelocities :: [Velocity],
    score :: Int,
    gameState :: GameState,
    rndGen :: StdGen,
    timePassed :: Float,
    level :: Int
}

width, height :: Int
width = 800
height = 600

timeThreshold :: Float
timeThreshold = 0.5

randomPosition :: StdGen -> Position
randomPosition g =
    let
        (genX, newGenY) = randomR (-800, 800) g
        x = genX
        (genY, newGen) = randomR (-600, 600) newGenY
        y = genY
    in (x, y)

randomEdge :: StdGen -> Position
randomEdge g =
    let
        (edge, newGen) = randomR (0 :: Int, 3 :: Int) g
        (x, y) = case edge of
            0 -> (-800, fst $ randomR (-600, 600) newGen)
            1 -> (800, fst $ randomR (-600, 600) newGen)
            2 -> (fst $ randomR (-800, 800) newGen, -600)
            _ -> (fst $ randomR (-800, 800) newGen, 600)
    in (x, y)

randomVelocity :: StdGen -> Velocity
randomVelocity g  =
    let
        (genX, newGenY) = randomR (-3.0, 3.0) g
        vx = genX
        (genY, newGen) = randomR (-3.0, 3.0) newGenY
        vy = genY
    in (vx, vy)

initialWorld :: World
initialWorld = World {
    circlePos = (0, 0),
    circleVel = (0, 0),
    radius = 30,
    otherRadius = 8,
    otherCircles = [],
    evilCircles = [],
    evilVelocities = [],
    score = 0,
    gameState = Playing,
    rndGen = mkStdGen 0,
    timePassed = 0,
    level = 1
}

areColliding :: World -> Position -> Bool
areColliding world (x2, y2) =
    let (x1, y1) = circlePos world
    in sqrt ((x2 - x1)^2 + (y2 - y1)^2) < radius world + otherRadius world

handleInput :: Event -> World -> World
handleInput (EventMotion (x, y)) world
    | gameState world == Playing =
        let (cx, cy) = circlePos world
            lag = 0.1
            x1 = cx + (x - cx) * lag
            y1 = cy + (y - cy) * lag
        in world { circlePos = (x1, y1) }
    | otherwise = world  -- stops during game over screen
handleInput (EventKey (Char 'r') Down _ _) world =
    initialWorld  -- restart
handleInput _ world = world

updateWorld :: Float -> StdGen -> World -> World
updateWorld deltaTime gen world =
    let
        newTimer = deltaTime + timePassed world
        updatedGen = snd (split gen)
        addCircles = newTimer >= timeThreshold
        updatedWorld =
            if addCircles
                then let newEvilCircle = randomEdge updatedGen
                         updatedGen2 = snd $ split updatedGen
                         newRandomCircle = randomPosition updatedGen2
                         updatedGen3 = snd $ split updatedGen2
                         newEvilVelocity = randomVelocity updatedGen3
                     in world {
                         otherCircles = otherCircles world ++ [newRandomCircle],
                         evilCircles = evilCircles world ++ [newEvilCircle],
                         evilVelocities = evilVelocities world ++ [newEvilVelocity],
                         timePassed = 0,
                         rndGen = updatedGen3
                     }
                else world { timePassed = newTimer }

        newEvilCircles = zipWith (\(x, y) (vx, vy) -> (x + vx, y + vy)) (evilCircles updatedWorld) (evilVelocities updatedWorld)
        collidedPositions = filter (areColliding updatedWorld) (otherCircles updatedWorld)

        scoreIncrease = length collidedPositions
        updatedScore = score updatedWorld + scoreIncrease
        updatedLevel = determineLevel updatedScore
    in
        if any (areColliding updatedWorld) (otherCircles updatedWorld)
            then
                let newRadius = radius updatedWorld + 1
                in updatedWorld { radius = newRadius, otherCircles = filter (not . areColliding updatedWorld) (otherCircles updatedWorld), evilCircles = newEvilCircles, rndGen = updatedGen, score = updatedScore, level = updatedLevel }
            else
                if any (areColliding updatedWorld) (evilCircles updatedWorld)
                    then
                        updatedWorld { gameState = Lost }
                    else
                        updatedWorld { evilCircles = newEvilCircles, score = updatedScore, level = updatedLevel }
  where
    determineLevel :: Int -> Int
    determineLevel s
        | s >= 100 = 5
        | s >= 50 = 4
        | s >= 25 = 3
        | s >= 10 = 2
        | s >= 1 = 1
        | otherwise = 1

render :: World -> Picture
render world
    | gameState world == Lost =
        pictures [gameoverText, restartText, scoreText]
    | otherwise =
        pictures $ mainCircle : foodCircles ++ badCircles ++ [scoreText]
   where
        (x, y) = circlePos world
        mainCircle = translate x y $ color (levelColor $ level world) $ circleSolid (radius world)
        foodCircles = map (\(cx, cy) -> translate cx cy $ color green $ circleSolid (otherRadius world)) (otherCircles world)
        badCircles = map (\(cx, cy) -> translate cx cy $ color red $ circleSolid (otherRadius world)) (evilCircles world)
        gameoverText = translate (-200) 0 $ color black $ scale 0.5 0.5 $ text "GAME OVER"
        restartText = translate (-170) (-60) $ color black $ scale 0.25 0.25 $ text "Press R to restart!"
        scoreText = translate (-370) 270 $ color black $ scale 0.25 0.25 $ text $ "Score: " ++ show (score world) ++ "   Level: " ++ show (level world)

        levelColor :: Int -> Color
        levelColor l
            | l == 1 = magenta
            | l == 2 = cyan
            | l == 3 = yellow
            | l == 4 = orange
            | l == 5 = blue
            | otherwise = blue

main :: IO ()
main = do
    gen <- getStdGen
    play
        (InWindow "Agar.io" (width, height) (100, 100))
        white -- background color
        60 -- fps
        (initialWorld { rndGen = gen })
        render
        handleInput
        (\dt world -> updateWorld dt (rndGen world) world)