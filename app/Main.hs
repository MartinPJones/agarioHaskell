{-# LANGUAGE NamedFieldPuns #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

type Position = (Float, Float)
type Velocity = (Float, Float)

data World = World {
    circlePos :: Position,
    circleVel :: Velocity,
    radius :: Float,
    otherRadius :: Float,
    otherCircles :: [Position],
    rndGen :: StdGen,
    timePassed :: Float
} deriving Show

width, height :: Int
width = 800
height = 600

numCircles :: Int
numCircles = 5

randomPosition :: StdGen -> Position
randomPosition g =
    let
        (genX, newGenY) = randomR (-700, 700) g -- Generates dots in bounds, change ints to change bounds
        x = genX
        (genY, newGen) = randomR (-700, 700) newGenY
        y = genY 
    in (x, y)

initialWorld :: World
initialWorld = World {
    circlePos = (0, 0),
    circleVel = (0, 0),
    radius = 30,
    otherRadius = 8,
    otherCircles = [],
    rndGen = mkStdGen 0, -- Initialization, not used seed
    timePassed = 0
}

areColliding :: World -> Position -> Bool
areColliding world (x2, y2) =
    let (x1, y1) = circlePos world
    in sqrt ((x2 - x1)^2 + (y2 - y1)^2) < radius world + otherRadius world 

moveTowardsMouse :: Float -> Position -> World -> World
moveTowardsMouse deltaTime (mouseX, mouseY) world =
    let (cx, cy) = circlePos world
        maxSpeed = 20.0  -- You can adjust the maximum speed here
        directionX = mouseX - cx
        directionY = mouseY - cy
        distance = sqrt (directionX ^ 2 + directionY ^ 2) 
        (currentVelX, currentVelY) = circleVel world
        (newVelX, newVelY) = if distance == 0 -- Idk why I thought this was a good idea. Need to change it to if mouse position hasn't been updated, maintain current velocity
                                then (currentVelX, currentVelY) -- Retain current velocity if no movement
                                else let speed = min maxSpeed distance
                                         normalizedDirX = directionX / distance
                                         normalizedDirY = directionY / distance
                                     in (speed * normalizedDirX, speed * normalizedDirY)
        newCirclePos = (cx + newVelX * deltaTime, cy + newVelY * deltaTime)
    in world { circleVel = (newVelX, newVelY), circlePos = newCirclePos }

-- Define a time threshold for circle addition
timeThreshold :: Float
timeThreshold = 5.0  -- Change this value as needed for the interval between new circle additions

updateWorld :: Float -> StdGen -> World -> World
updateWorld deltaTime gen world =
    let updatedWorld = moveTowardsMouse deltaTime (mousePosToWorldCoords world) world

        -- Update the timer by adding the elapsed time
        newTimer = deltaTime + timePassed world
        updatedGen = snd (split gen) -- Generate a new random generator

        -- Check if it's time to add a new circle
        addNewCircle = newTimer >= timeThreshold

        updatedWorldWithNewCircle =
            if addNewCircle
                then let newRandomCircle = randomPosition updatedGen -- Generate a new random circle
                     in updatedWorld {
                         otherCircles = otherCircles updatedWorld ++ [newRandomCircle],
                         timePassed = 0,  -- Reset the timer after adding a new circle
                         rndGen = updatedGen -- Replace the generator to maintain randomness
                     }
                else updatedWorld { timePassed = newTimer }

        -- Check for collisions after potential updates
        collidedPositions = filter (areColliding updatedWorld) (otherCircles updatedWorld)
    in if not (null collidedPositions)
        then let newRadius = radius updatedWorld * 1.2
                 updatedCircles = filter (not . (`elem` collidedPositions)) (otherCircles updatedWorld)
                 newRandomCircle = randomPosition updatedGen -- Generate a new random circle

             in updatedWorldWithNewCircle {
                 radius = newRadius,
                 otherCircles = updatedCircles ++ [newRandomCircle],
                 rndGen = updatedGen -- Replace the generator to maintain randomness
             }
        else updatedWorldWithNewCircle

-- Helper function to convert mouse coordinates to world coordinates
mousePosToWorldCoords :: World -> Position
mousePosToWorldCoords world =
    let (mouseX, mouseY) = circlePos world
        worldWidth = fromIntegral width
        worldHeight = fromIntegral height
    in ((mouseX - worldWidth / 2) * 2, (worldHeight / 2 - mouseY) * 2)

render :: World -> Picture
render world = pictures $ mainCircle : otherCirclesPictures
  where
    (x, y) = circlePos world
    mainCircle = translate x y $ color blue $ circleSolid (radius world)
    otherCirclesPictures = map (\(cx, cy) -> translate cx cy $ color red $ circleSolid (otherRadius world)) (otherCircles world)

renderWithCamera :: World -> Picture
renderWithCamera world =
    translate (-circleX) (-circleY) $ pictures [renderWorld, renderCircle]
    where
        (circleX, circleY) = circlePos world
        renderWorld = render world
        renderCircle = translate circleX circleY $ color blue $ circleSolid (radius world)

main :: IO ()
main = do
    gen <- getStdGen -- Get the initial random generator
    play
        (InWindow "Agar.io" (width, height) (100, 100))
        white
        60
        (initialWorld { rndGen = gen }) -- Pass the initial random generator to the world state
        renderWithCamera
        (\event world -> case event of
            EventMotion pos -> moveTowardsMouse 0.1 pos world
            _ -> world)
        (\dt world -> updateWorld dt (rndGen world) world) -- Update function now takes a StdGen parameter
