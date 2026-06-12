-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

{- | A clone of the classic 1989 DOS game BLOCKOUT (3D Tetris).

Single configuration: 5x5x12 pit, FLAT block set.

Controls (same as the original):
  Arrows      move piece in the pit cross-section
  Q\/A W\/S E\/D rotate about the X \/ Y \/ Z axis
  Space       drop
  P           pause, Esc restart
-}
module Main where

-----------------------------------------------------------------------------
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless, when)
import Data.List (nub)
import Data.Maybe (isNothing)
-----------------------------------------------------------------------------
import Miso hiding (status, (!!))
import Miso.CSS (StyleSheet)
import qualified Miso.CSS as CSS
import qualified Miso.Event.Decoder as D
import Miso.Html.Element as H
import Miso.Html.Property as P
import Miso.JSON (withObject, (.!=), (.:), (.:?))
import Miso.Lens
import Miso.Random (replicateRM)
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP

-----------------------------------------------------------------------------
-- Model
-----------------------------------------------------------------------------

{- | A cell in the pit. x grows right, y grows down (screen), z grows away
from the viewer (deeper into the pit).
-}
type Cell = (Int, Int, Int)

data Status = Playing | Paused | Over
    deriving (Eq, Show)

{- | Transient state of the rotation animation. The logical piece cells
always hold the final orientation; rendering applies the remaining part
of the inverse rotation, which shrinks to nothing as progress reaches 1.
-}
data Spin = Spin
    { spinAxis :: Int
    -- ^ 0 = X, 1 = Y, 2 = Z
    , spinDir :: Double
    -- ^ +1 or -1, the sign of the 90 degree turn
    , spinOff :: (Double, Double, Double)
    -- ^ old centroid minus new centroid (wall kicks shift the piece)
    , spinT :: Double
    -- ^ progress, 0 to 1
    }
    deriving (Eq, Show)

data Model = Model
    { _well :: [Cell]
    -- ^ cells locked into the pit
    , _piece :: [Cell]
    -- ^ absolute cells of the falling piece
    , _spin :: Maybe Spin
    -- ^ rotation animation in flight, if any
    , _score :: Int
    , _highScore :: Int
    , _cubes :: Int
    -- ^ cubes played
    , _cleared :: Int
    -- ^ layers cleared
    , _status :: Status
    , _ticks :: Int
    -- ^ gravity tick accumulator
    }
    deriving Eq

initialModel :: Model
initialModel =
    Model
        { _well = []
        , _piece = []
        , _spin = Nothing
        , _score = 0
        , _highScore = 0
        , _cubes = 0
        , _cleared = 0
        , _status = Playing
        , _ticks = 0
        }

well :: Lens Model [Cell]
well = lens _well (\r f -> r{_well = f})

piece :: Lens Model [Cell]
piece = lens _piece (\r f -> r{_piece = f})

spin :: Lens Model (Maybe Spin)
spin = lens _spin (\r f -> r{_spin = f})

score :: Lens Model Int
score = lens _score (\r f -> r{_score = f})

highScore :: Lens Model Int
highScore = lens _highScore (\r f -> r{_highScore = f})

cubes :: Lens Model Int
cubes = lens _cubes (\r f -> r{_cubes = f})

cleared :: Lens Model Int
cleared = lens _cleared (\r f -> r{_cleared = f})

status :: Lens Model Status
status = lens _status (\r f -> r{_status = f})

ticks :: Lens Model Int
ticks = lens _ticks (\r f -> r{_ticks = f})

-----------------------------------------------------------------------------
-- Game constants
-----------------------------------------------------------------------------
pitW, pitH, pitD :: Int
pitW = 5
pitH = 5
pitD = 12

-- | The FLAT block set of the original game: every piece is one cube thick.
pieceSet :: [[(Int, Int)]]
pieceSet =
    [ [(0, 0)] -- single cube
    , [(0, 0), (1, 0)] -- domino
    , [(0, 0), (1, 0), (2, 0)] -- 1x3
    , [(0, 0), (0, 1), (1, 1)] -- corner
    , [(1, 0), (0, 1), (1, 1), (2, 1)] -- T
    , [(1, 0), (2, 0), (0, 1), (1, 1)] -- S
    , [(0, 0), (0, 1), (1, 1), (2, 1)] -- L
    ]

level :: Model -> Int
level m = min 9 (_cubes m `div` 25)

-- | Gravity period in 100ms ticks for a given level.
dropTicks :: Int -> Int
dropTicks lvl = max 3 (12 - lvl)

hsKey :: MisoString
hsKey = "blockout-high-score"

-----------------------------------------------------------------------------
data Action
    = Boot
    | Tick
    | SpinTick
    | KeyDown (Int, Bool)
    | NewPiece Int
    | HighScoreLoaded (Maybe MisoString)
    deriving (Eq, Show)

-----------------------------------------------------------------------------
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
-----------------------------------------------------------------------------
main :: IO ()
#ifdef INTERACTIVE
main = reload defaultEvents app
#else
main = startApp defaultEvents app
#endif
-----------------------------------------------------------------------------
app :: App Model Action
app =
    (component initialModel updateModel viewModel)
        { styles = [Sheet sheet]
        , subs = [gravitySub, windowSub "keydown" keyDownDecoder KeyDown]
        , mount = Just Boot
        }

{- | Decodes a keydown event into its keyCode and the auto-repeat flag.
Acting on raw keydown events (rather than diffing a set of currently
pressed keys) means a physical key press can never be swallowed by stale
key-tracking state, e.g. when a keyup got lost while the window was
unfocused.
-}
keyDownDecoder :: D.Decoder (Int, Bool)
keyDownDecoder = D.at [] $ withObject "event" $ \o ->
    (,) <$> o .: "keyCode" <*> (o .:? "repeat" .!= False)

gravitySub :: Sub Action
gravitySub sink = forever (threadDelay 100000 >> sink Tick)

{- | Drives the rotation animation at ~60fps. Started when a rotation
begins and stopped once the animation has played out.
-}
spinSub :: Sub Action
spinSub sink = forever (threadDelay 16000 >> sink SpinTick)

spinKey :: MisoString
spinKey = "spin"

-- | Animation progress per 16ms frame: a 90 degree turn takes 0.2s.
spinStep :: Double
spinStep = 0.016 / 0.2

-----------------------------------------------------------------------------
-- Update
-----------------------------------------------------------------------------
updateModel :: Action -> Effect parent props Model Action
updateModel = \case
    Boot -> do
        io (HighScoreLoaded <$> getLocalStorage hsKey)
        spawnPiece
    HighScoreLoaded stored ->
        highScore .= maybe 0 parseScore stored
    Tick -> do
        m <- use this
        when (_status m == Playing && not (null (_piece m))) $ do
            let t = _ticks m + 1
            if t >= dropTicks (level m)
                then do
                    ticks .= 0
                    stepDown
                else ticks .= t
    SpinTick -> do
        m <- use this
        case _spin m of
            Nothing -> stopSub spinKey
            Just sp
                | spinT sp + spinStep >= 1 -> do
                    spin .= Nothing
                    stopSub spinKey
                | otherwise -> spin .= Just sp{spinT = spinT sp + spinStep}
    KeyDown (code, isRepeat) ->
        unless isRepeat (handleKey code)
    NewPiece i -> do
        m <- use this
        let cells = spawnCells (pieceSet !! min i (length pieceSet - 1))
        spin .= Nothing
        if fits (_well m) cells
            then do
                piece .= cells
                ticks .= 0
            else do
                piece .= []
                status .= Over

parseScore :: MisoString -> Int
parseScore s =
    case reads (fromMisoString s) of
        [(n, "")] -> n
        _ -> 0

-- | Place a flat piece centered at the mouth of the pit.
spawnCells :: [(Int, Int)] -> [Cell]
spawnCells ps =
    [(x + ox, y + oy, 0) | (x, y) <- ps]
  where
    ox = (pitW - 1 - maximum (map fst ps)) `div` 2
    oy = (pitH - 1 - maximum (map snd ps)) `div` 2

fits :: [Cell] -> [Cell] -> Bool
fits w = all ok
  where
    ok c@(x, y, z) =
        x >= 0
            && x < pitW
            && y >= 0
            && y < pitH
            && z >= 0
            && z < pitD
            && c `notElem` w

spawnPiece :: Effect parent props Model Action
spawnPiece = io $ do
    ds <- replicateRM 1
    let d = case ds of (x : _) -> x; [] -> 0.5
    pure (NewPiece (floor (d * fromIntegral (length pieceSet))))

handleKey :: Int -> Effect parent props Model Action
handleKey k = do
    m <- use this
    case _status m of
        Over -> when (k == 27) restart
        Paused -> case k of
            80 -> status .= Playing
            27 -> restart
            _ -> pure ()
        Playing -> case k of
            37 -> tryMove (-1) 0
            39 -> tryMove 1 0
            38 -> tryMove 0 (-1)
            40 -> tryMove 0 1
            32 -> hardDrop
            81 -> tryRotate 0 1 rotXcw
            65 -> tryRotate 0 (-1) rotXccw
            87 -> tryRotate 1 1 rotYcw
            83 -> tryRotate 1 (-1) rotYccw
            69 -> tryRotate 2 (-1) rotZccw
            68 -> tryRotate 2 1 rotZcw
            80 -> status .= Paused
            27 -> restart
            _ -> pure ()

restart :: Effect parent props Model Action
restart = do
    m <- use this
    this .= initialModel{_highScore = _highScore m}
    spawnPiece

tryMove :: Int -> Int -> Effect parent props Model Action
tryMove dx dy = do
    m <- use this
    let moved = [(x + dx, y + dy, z) | (x, y, z) <- _piece m]
    when (fits (_well m) moved) (piece .= moved)

stepDown :: Effect parent props Model Action
stepDown = do
    m <- use this
    let moved = [(x, y, z + 1) | (x, y, z) <- _piece m]
    if fits (_well m) moved
        then piece .= moved
        else lockPiece

hardDrop :: Effect parent props Model Action
hardDrop = do
    m <- use this
    unless (null (_piece m)) $ do
        let down k cs = [(x, y, z + k) | (x, y, z) <- cs]
            descend k
                | fits (_well m) (down (k + 1) (_piece m)) = descend (k + 1)
                | otherwise = k
            dist = descend 0
        piece .= down dist (_piece m)
        score += dist
        lockPiece

lockPiece :: Effect parent props Model Action
lockPiece = do
    m <- use this
    let w0 = _piece m ++ _well m
        full =
            [ z
            | z <- [0 .. pitD - 1]
            , length [() | (_, _, cz) <- w0, cz == z] == pitW * pitH
            ]
        w1 =
            [ (x, y, z + length (filter (> z) full))
            | (x, y, z) <- w0
            , z `notElem` full
            ]
        lvl = level m
        sc =
            _score m
                + length (_piece m) * (lvl + 1)
                + 100 * (lvl + 1) * length full * length full
    well .= w1
    piece .= []
    spin .= Nothing
    cubes += length (_piece m)
    cleared += length full
    score .= sc
    when (sc > _highScore m) $ do
        highScore .= sc
        io_ (setLocalStorage hsKey (ms sc))
    spawnPiece

-----------------------------------------------------------------------------
-- Rotation. Pieces rotate about the center of their bounding box, with a
-- few "kick" offsets tried so rotation works next to walls.
-----------------------------------------------------------------------------
type Dims = (Int, Int, Int)

rotXcw, rotXccw, rotYcw, rotYccw, rotZcw, rotZccw :: Dims -> Cell -> Cell
rotXcw (_, _, sz) (x, y, z) = (x, sz - 1 - z, y)
rotXccw (_, sy, _) (x, y, z) = (x, z, sy - 1 - y)
rotYcw (_, _, sz) (x, y, z) = (sz - 1 - z, y, x)
rotYccw (sx, _, _) (x, y, z) = (z, y, sx - 1 - x)
rotZcw (_, sy, _) (x, y, z) = (sy - 1 - y, x, z)
rotZccw (sx, _, _) (x, y, z) = (y, sx - 1 - x, z)

{- | Attempt a rotation. @axis@ (0 = X, 1 = Y, 2 = Z) and @dir@ describe
the same turn as the discrete @rot@ function and are used to animate it.
-}
tryRotate :: Int -> Double -> (Dims -> Cell -> Cell) -> Effect parent props Model Action
tryRotate axis dir rot = do
    m <- use this
    unless (null (_piece m)) $ do
        let cs = _piece m
            mnx = minimum [x | (x, _, _) <- cs]
            mny = minimum [y | (_, y, _) <- cs]
            mnz = minimum [z | (_, _, z) <- cs]
            sx = maximum [x | (x, _, _) <- cs] - mnx + 1
            sy = maximum [y | (_, y, _) <- cs] - mny + 1
            sz = maximum [z | (_, _, z) <- cs] - mnz + 1
            rel' = [rot (sx, sy, sz) (x - mnx, y - mny, z - mnz) | (x, y, z) <- cs]
            sx' = maximum [x | (x, _, _) <- rel'] + 1
            sy' = maximum [y | (_, y, _) <- rel'] + 1
            sz' = maximum [z | (_, _, z) <- rel'] + 1
            ox = mnx + (sx - sx') `div` 2
            oy = mny + (sy - sy') `div` 2
            oz = mnz + (sz - sz') `div` 2
            kicks =
                [ (0, 0, 0)
                , (-1, 0, 0)
                , (1, 0, 0)
                , (0, -1, 0)
                , (0, 1, 0)
                , (-2, 0, 0)
                , (2, 0, 0)
                , (0, -2, 0)
                , (0, 2, 0)
                , (0, 0, -1)
                , (0, 0, -2)
                ]
            attempts =
                [ [(x + ox + kx, y + oy + ky, z + oz + kz) | (x, y, z) <- rel']
                | (kx, ky, kz) <- kicks
                ]
        case filter (fits (_well m)) attempts of
            (good : _) -> do
                let (px, py, pz) = centroid cs
                    (gx, gy, gz) = centroid good
                piece .= good
                spin .= Just (Spin axis dir (px - gx, py - gy, pz - gz) 0)
                when (isNothing (_spin m)) (startSub spinKey spinSub)
            [] -> pure ()

-- | Center of mass of a set of cells, in lattice corner coordinates.
centroid :: [Cell] -> (Double, Double, Double)
centroid cs =
    ( avg [fi x | (x, _, _) <- cs]
    , avg [fi y | (_, y, _) <- cs]
    , avg [fi z | (_, _, z) <- cs]
    )
  where
    avg xs = sum xs / fi (length xs) + 0.5

-----------------------------------------------------------------------------
-- View: perspective projection into the pit
-----------------------------------------------------------------------------
svgSize, halfSize, unit, focal :: Double
svgSize = 560
halfSize = 280
unit = 106
focal = 5

fi :: Int -> Double
fi = fromIntegral

{- | Project a pit coordinate to SVG screen space. The eye looks straight
down the Z axis through the center of the pit mouth.
-}
proj :: Double -> Double -> Double -> (Double, Double)
proj x y z = (halfSize + (x - 2.5) * unit * s, halfSize + (y - 2.5) * unit * s)
  where
    s = focal / (focal + z)

msd :: Double -> MisoString
msd d = ms (fromIntegral (round (d * 10) :: Int) / 10 :: Double)

pointsOf :: [(Double, Double)] -> MisoString
pointsOf ps = ms (unwords [pt p | p <- ps])
  where
    pt (a, b) = fromMisoString (msd a) <> "," <> fromMisoString (msd b)

poly :: MisoString -> MisoString -> MisoString -> [(Double, Double)] -> View Model Action
poly fillCol strokeCol w ps =
    S.polygon_
        [ SP.points_ (pointsOf ps)
        , SP.fill_ fillCol
        , SP.stroke_ strokeCol
        , SP.strokeWidth_ w
        ]

lineSeg :: MisoString -> MisoString -> (Double, Double) -> (Double, Double) -> View Model Action
lineSeg strokeCol w (ax, ay) (bx, by) =
    S.line_
        [ SP.x1_ (msd ax)
        , SP.y1_ (msd ay)
        , SP.x2_ (msd bx)
        , SP.y2_ (msd by)
        , SP.stroke_ strokeCol
        , SP.strokeWidth_ w
        ]

gridColor :: MisoString
gridColor = "#00b400"

gline :: (Double, Double) -> (Double, Double) -> View Model Action
gline = lineSeg gridColor "1"

-- | The green wireframe of the empty pit.
pitGrid :: [View Model Action]
pitGrid =
    concat
        [ [poly "none" gridColor "1" (ring (fi z)) | z <- [0 .. pitD]]
        , [gline (proj x y 0) (proj x y depth) | x <- [0 .. fi pitW], y <- [0, fi pitH]]
        , [gline (proj x y 0) (proj x y depth) | x <- [0, fi pitW], y <- [1 .. fi pitH - 1]]
        , [gline (proj x 0 depth) (proj x (fi pitH) depth) | x <- [0 .. fi pitW]]
        , [gline (proj 0 y depth) (proj (fi pitW) y depth) | y <- [0 .. fi pitH]]
        ]
  where
    depth = fi pitD
    ring z = [proj 0 0 z, proj (fi pitW) 0 z, proj (fi pitW) (fi pitH) z, proj 0 (fi pitH) z]

-- | (face, shaded side) color per pit layer, front (z=0) to bottom (z=11).
palette :: [(MisoString, MisoString)]
palette =
    [ ("#ff3030", "#8c1a1a")
    , ("#ff8020", "#8c4612")
    , ("#ffd000", "#8c7200")
    , ("#b0e000", "#5f7a00")
    , ("#40d040", "#227222")
    , ("#00c890", "#006e4f")
    , ("#00b8d8", "#006576")
    , ("#0090ff", "#004f8c")
    , ("#4060ff", "#23348c")
    , ("#8040ff", "#46238c")
    , ("#c030e0", "#691a7a")
    , ("#ff30a0", "#8c1a58")
    ]

faceColor, sideColor :: Int -> MisoString
faceColor z = fst (palette !! z)
sideColor z = snd (palette !! z)

{- | Locked cubes, painted back to front. Each cube shows its front face,
plus any side faces that look toward the viewer and are not hidden by a
neighbouring cube in the same layer.
-}
wellCubes :: [Cell] -> [View Model Action]
wellCubes w = concat [layerViews z | z <- [pitD - 1, pitD - 2 .. 0]]
  where
    layerViews z =
        let cs = [c | c@(_, _, cz) <- w, cz == z]
         in concatMap sideFaces cs ++ map frontFace cs
    frontFace (x, y, z) =
        poly
            (faceColor z)
            "#000000"
            "1"
            [pr x y z, pr (x + 1) y z, pr (x + 1) (y + 1) z, pr x (y + 1) z]
    sideFaces (x, y, z) =
        concat
            [ [ quad z [pr x y z, pr x (y + 1) z, pr x (y + 1) (z + 1), pr x y (z + 1)]
              | x >= 3
              , free (x - 1) y z
              ]
            , [ quad z [pr (x + 1) y z, pr (x + 1) (y + 1) z, pr (x + 1) (y + 1) (z + 1), pr (x + 1) y (z + 1)]
              | x <= 1
              , free (x + 1) y z
              ]
            , [ quad z [pr x y z, pr (x + 1) y z, pr (x + 1) y (z + 1), pr x y (z + 1)]
              | y >= 3
              , free x (y - 1) z
              ]
            , [ quad z [pr x (y + 1) z, pr (x + 1) (y + 1) z, pr (x + 1) (y + 1) (z + 1), pr x (y + 1) (z + 1)]
              | y <= 1
              , free x (y + 1) z
              ]
            ]
    quad z = poly (sideColor z) "#000000" "1"
    free x y z = (x, y, z) `notElem` w
    pr x y z = proj (fi x) (fi y) (fi z)

{- | The falling piece, drawn as a white wireframe of its outline only.
While a rotation animation is in flight, every outline corner is rotated
back by the not-yet-elapsed part of the 90 degree turn about the piece
centroid (and translated back along any wall-kick offset), so the
wireframe sweeps smoothly into its final resting orientation.
-}
pieceWire :: Maybe Spin -> [Cell] -> [View Model Action]
pieceWire msp cs =
    [lineSeg "#ffffff" "1.5" (corner a) (corner b) | (a, b) <- outlineEdges cs]
  where
    corner (x, y, z) = let (px, py, pz) = place (fi x) (fi y) (fi z) in proj px py pz
    place = case msp of
        Nothing -> (,,)
        Just (Spin axis dir (ox, oy, oz) t) ->
            let theta = -dir * (pi / 2) * (1 - t)
                (cx0, cy0, cz0) = centroid cs
             in \x y z ->
                    let (rx, ry, rz) = rotate3 axis theta (x - cx0, y - cy0, z - cz0)
                     in ( rx + cx0 + (1 - t) * ox
                        , ry + cy0 + (1 - t) * oy
                        , rz + cz0 + (1 - t) * oz
                        )

{- | Rotate a vector by @theta@ radians about the X, Y or Z axis. At
+90 degrees this agrees with the linear part of the corresponding
discrete cw rotation ('rotXcw' etc.), at -90 degrees with the ccw one.
-}
rotate3 :: Int -> Double -> (Double, Double, Double) -> (Double, Double, Double)
rotate3 0 th (x, y, z) = (x, y * cos th - z * sin th, y * sin th + z * cos th)
rotate3 1 th (x, y, z) = (x * cos th - z * sin th, y, x * sin th + z * cos th)
rotate3 _ th (x, y, z) = (x * cos th - y * sin th, x * sin th + y * cos th, z)

{- | The crease edges of the union of the piece's unit cubes, in lattice
corner coordinates. Each lattice edge touches up to four cells; it is
part of the outline when 1 or 3 of them are filled, or when exactly 2
are filled diagonally. Edges in the middle of a flat surface (2 filled
side by side) or interior edges (0 or 4 filled) are not drawn.
-}
outlineEdges :: [Cell] -> [(Cell, Cell)]
outlineEdges cs =
    concat
        [ [ ((x, y, z), (x + 1, y, z))
          | (x, y, z) <- nub [(cx, cy + dy, cz + dz) | (cx, cy, cz) <- cs, dy <- [0, 1], dz <- [0, 1]]
          , sharp (occ (x, y - 1, z - 1)) (occ (x, y - 1, z)) (occ (x, y, z - 1)) (occ (x, y, z))
          ]
        , [ ((x, y, z), (x, y + 1, z))
          | (x, y, z) <- nub [(cx + dx, cy, cz + dz) | (cx, cy, cz) <- cs, dx <- [0, 1], dz <- [0, 1]]
          , sharp (occ (x - 1, y, z - 1)) (occ (x - 1, y, z)) (occ (x, y, z - 1)) (occ (x, y, z))
          ]
        , [ ((x, y, z), (x, y, z + 1))
          | (x, y, z) <- nub [(cx + dx, cy + dy, cz) | (cx, cy, cz) <- cs, dx <- [0, 1], dy <- [0, 1]]
          , sharp (occ (x - 1, y - 1, z)) (occ (x - 1, y, z)) (occ (x, y - 1, z)) (occ (x, y, z))
          ]
        ]
  where
    occ c = c `elem` cs
    -- a\/d and b\/c are the diagonal pairs of the four cells around an edge
    sharp a b c d = case length (filter id [a, b, c, d]) of
        1 -> True
        3 -> True
        2 -> (a && d) || (b && c)
        _ -> False

overlay :: Status -> [View Model Action]
overlay = \case
    Playing -> []
    Paused ->
        [shade, banner 296 "40" "#ffd000" "PAUSED"]
    Over ->
        [ shade
        , banner 270 "44" "#ff3030" "GAME OVER"
        , banner 320 "20" "#ffffff" "press ESC to restart"
        ]
  where
    shade =
        S.rect_
            [ SP.x_ "0"
            , SP.y_ "0"
            , P.width_ "560"
            , P.height_ "560"
            , SP.fill_ "rgba(0,0,0,0.65)"
            ]
    banner y size col t =
        S.text_
            [ SP.x_ "280"
            , SP.y_ (ms (y :: Int))
            , SP.textAnchor_ "middle"
            , SP.fill_ col
            , SP.fontSize_ size
            , SP.fontFamily_ "'Courier New', monospace"
            , SP.fontWeight_ "bold"
            ]
            [text t]

-----------------------------------------------------------------------------
-- View: page layout
-----------------------------------------------------------------------------
viewModel :: props -> Model -> View Model Action
viewModel _ m =
    H.div_
        [P.class_ "blockout"]
        [ H.div_ [P.class_ "titlebar"] ["BLOCKOUT \x2014 \x1F35C miso"]
        , H.div_
            [P.class_ "layout"]
            [ leftPanel m
            , pitSvg m
            , rightPanel m
            ]
        , H.div_
            [P.class_ "controls"]
            ["\x2190 \x2192 \x2191 \x2193 move \x2022 Q/A W/S E/D rotate \x2022 SPACE drop \x2022 P pause \x2022 ESC restart"]
        ]

pitSvg :: Model -> View Model Action
pitSvg m =
    S.svg_
        [ P.width_ "560"
        , P.height_ "560"
        , SP.viewBox_ "0 0 560 560"
        , P.class_ "pit"
        ]
        (pitGrid ++ wellCubes (_well m) ++ pieceWire (_spin m) (_piece m) ++ overlay (_status m))

leftPanel :: Model -> View Model Action
leftPanel m =
    H.div_
        [P.class_ "panel"]
        [ infoBox "LEVEL" (ms (level m))
        , H.div_
            [P.class_ "stack"]
            [ H.div_
                [ P.class_ "seg"
                , CSS.style_ ["background-color" =: segColor z]
                ]
                []
            | z <- [0 .. pitD - 1]
            ]
        ]
  where
    segColor z
        | any (\(_, _, cz) -> cz == z) (_well m) = faceColor z
        | otherwise = "#101010"

rightPanel :: Model -> View Model Action
rightPanel m =
    H.div_
        [P.class_ "panel wide"]
        [ infoBox "SCORE" (ms (_score m))
        , infoBox "CUBES PLAYED" (ms (_cubes m))
        , infoBox "LAYERS" (ms (_cleared m))
        , infoBox "HIGH SCORE" (ms (_highScore m))
        , infoBox "PIT" "5\x00D7\&5\x00D7\&12"
        , infoBox "BLOCK SET" "FLAT"
        ]

infoBox :: MisoString -> MisoString -> View Model Action
infoBox label val =
    H.div_
        [P.class_ "infobox"]
        [ H.div_ [P.class_ "label"] [text label]
        , H.div_ [P.class_ "value"] [text val]
        ]

-----------------------------------------------------------------------------
sheet :: StyleSheet
sheet =
    CSS.sheet_
        [ CSS.selector_
            "html, body"
            [ CSS.margin "0"
            , CSS.height "100%"
            , "overflow" =: "hidden"
            , "background-color" =: "#000000"
            ]
        , CSS.selector_
            "body"
            [ CSS.display "flex"
            , CSS.justifyContent "center"
            , CSS.alignItems "center"
            , CSS.fontFamily "'Courier New', monospace"
            , "color" =: "#00cc00"
            ]
        , CSS.selector_
            ".blockout"
            [ CSS.display "flex"
            , "flex-direction" =: "column"
            , "gap" =: "10px"
            , "align-items" =: "stretch"
            ]
        , CSS.selector_
            ".titlebar"
            [ "background-color" =: "#1a1a1a"
            , "border" =: "2px solid #555"
            , "color" =: "#ffd000"
            , CSS.fontSize "22px"
            , CSS.fontWeight "bold"
            , CSS.textAlign "center"
            , CSS.padding (CSS.px 6)
            , "letter-spacing" =: "4px"
            ]
        , CSS.selector_
            ".layout"
            [ CSS.display "flex"
            , "flex-direction" =: "row"
            , "gap" =: "12px"
            , "align-items" =: "stretch"
            ]
        , CSS.selector_
            ".pit"
            [ "border" =: "2px solid #2244cc"
            , "background-color" =: "#000000"
            ]
        , CSS.selector_
            ".panel"
            [ CSS.display "flex"
            , "flex-direction" =: "column"
            , "gap" =: "12px"
            , "width" =: "90px"
            ]
        , CSS.selector_
            ".panel.wide"
            [ "width" =: "170px"
            ]
        , CSS.selector_
            ".stack"
            [ "flex" =: "1"
            , CSS.display "flex"
            , "flex-direction" =: "column"
            , "gap" =: "3px"
            , "border" =: "2px solid #00cc00"
            , CSS.padding (CSS.px 4)
            ]
        , CSS.selector_
            ".seg"
            [ "flex" =: "1"
            ]
        , CSS.selector_
            ".infobox .label"
            [ "color" =: "#00cc00"
            , CSS.fontSize "13px"
            , CSS.fontWeight "bold"
            , CSS.textAlign "center"
            , "margin-bottom" =: "2px"
            ]
        , CSS.selector_
            ".infobox .value"
            [ "border" =: "2px solid #2244cc"
            , "color" =: "#ffb000"
            , CSS.fontSize "18px"
            , CSS.fontWeight "bold"
            , CSS.textAlign "right"
            , CSS.padding (CSS.px 4)
            ]
        , CSS.selector_
            ".controls"
            [ "color" =: "#888888"
            , CSS.fontSize "14px"
            , CSS.textAlign "center"
            ]
        ]

-----------------------------------------------------------------------------
