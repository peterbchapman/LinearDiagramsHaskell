-- Fiddling about with the layout, windows, and buttons.
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import LinearFoundations
import LinearRules

-- the cs is the colours, so each colour is bound to a line
-- length of cs needs to be the same as the number of sets in d
mainD :: (Ld a) => (a,[(Double,Double,Double)]) -> IO()
mainD (d,cs) = do
  initGUI

-- setting up the window, of a decent size
  win <- windowNew
  windowSetTitle win "Lhinear Diagrams"
  set win [windowDefaultWidth := 1800, windowDefaultHeight := 800, windowTitle := "Lhinear Diagrams", windowWindowPosition := WinPosCenter]


  putD win (d,cs) -- this does the actual drawing of the diagram in the window.

  widgetShowAll win
--  win `onDestroy` mainQuit
  mainGUI

-- everything in here needed to redraw in the same window
putD :: (Ld a) => Window -> (a,[(Double,Double,Double)]) -> IO()
putD w (d,cs) = do
                    lay <- layoutNew Nothing Nothing
                    canvas <- drawingAreaNew
                    canvas `on` sizeRequest $ return (Requisition 1600 270) -- the 270 was vReq
                    containerAdd w lay
                    layoutPut lay canvas 100 100
                    canvas `onExpose` (\_ -> renderScene canvas (resizeD d (1500 / dLength d)) (70,50) 50 cs) 
                    putButts w (resizeD d (1500 / dLength d),cs) lay (70,50) 50 (fromIntegral 270)
                    quitButt <- buttonNewWithLabel "Exit"
                    widgetSetSizeRequest quitButt 50 50
                    onClicked quitButt (do mainQuit
                                           widgetDestroy w
                                           )
                    layoutPut lay quitButt 0 0 
                    
  


coloursRGB :: [(Double,Double,Double)]
coloursRGB = map (\(x,y,z) -> (x/255,y/255,z/255)) [(228,26,28),(55,126,184),(75,175,74),(152,78,163),(255,127,0),(255,255,51),(166,86,40)]

renderScene :: (Ld a) => DrawingArea -> a -> (Double,Double) -> Double -> [(Double,Double,Double)] -> IO Bool
renderScene da d (x,y) h cs = do
    dw <- widgetGetDrawWindow da
    renderWithDrawable dw $ do 
                              setLineWidth 5 -- width of the line
                              setLineCap LineCapButt -- what ends of line look like
                              setLineJoin LineJoinRound -- what lines joining look like. 

                              drawDiagram d (x,y) h cs
    return True

-- COLOURFUL DRAWING. Requires the start points for each (possible)
-- line segment to be calculated beforehand, which can then be passed to moveTo functions.

-- for an individual overlap
drawOverlap :: Overlap -> [(Double,Double)] -> [(Double,Double,Double)] -> Render ()
drawOverlap ([x],len) [(px,py)] [(cr,cg,cb)]
    | x == Pr = do
                  setSourceRGB cr cg cb
                  moveTo px py
                  relLineTo (fromIntegral len) 0
                  stroke
    | otherwise = do
                  moveTo px py
drawOverlap ((x:xs),len) ((px,py):ps) ((cr,cg,cb):cs)
    | x == Pr = do
                  setSourceRGB cr cg cb
                  moveTo px py
                  relLineTo (fromIntegral len) 0
                  stroke
                  drawOverlap (xs,len) ps cs
    | otherwise = do
                  drawOverlap (xs,len) ps cs

-- for a set of overlaps
drawOverlaps :: [Overlap] -> [[(Double,Double)]] -> [(Double,Double,Double)] -> Render ()
drawOverlaps [x] [p] cs = do drawOverlap x p cs
drawOverlaps (x:xs) (p:ps) cs = do 
                                  drawOverlap x p cs
                                  drawOverlaps xs ps cs


drawGuidelines' :: [(Double,Double)] -> Double -> Render ()
drawGuidelines' [(px,py)] h = do 
                                moveTo px (py-10)
                                relLineTo 0 h
drawGuidelines' ((px,py):ps) h = do
                                    moveTo px (py-10)
                                    relLineTo 0 h
                                    drawGuidelines' ps h

drawGuidelines :: [(Double,Double)] -> Double -> Render ()
drawGuidelines ps h = do
                        setSourceRGB 0.5 0.5 0.5
                        setDash [5,5] 0
                        setLineWidth 2
                        drawGuidelines' ps h
                        stroke

drawLabels :: Labels -> [(Double,Double)] -> [(Double,Double,Double)] -> Render ()
drawLabels [l] [(px,py)] [(cr,cg,cb)] = do
                                          setSourceRGB cr cg cb
                                          moveTo (px-68) (py+6)
                                          setFontSize 18
                                          showText l
drawLabels (l:ls) ((px,py):ps) ((cr,cg,cb):cs) = do
                                                   setSourceRGB cr cg cb
                                                   moveTo (px-68) (py + 6)
                                                   setFontSize 18
                                                   showText l
                                                   drawLabels ls ps cs

-- puts everything together. Draws diagram d, with top-left overlap starting at (px,py), and separation between
-- the sets of h. Takes in a list of colours cs.
drawDiagram :: (Ld a) => a -> (Double,Double) -> Double -> [(Double,Double,Double)] -> Render ()
drawDiagram d (px,py) h cs = do       
                            drawOverlaps (overlaps d) (getPointsD d (px,py) h) cs
                            drawLabels (names d) (head (getPointsD d (px,py) h)) cs
                            drawGuidelines (map head (getPointsD d (px,py) h)) h'
             where n = length $ names d
                   h' =  h * fromIntegral (n-1) + 22



-- Function that resizes a diagram by an Double factor
resizeD :: (Ld a) => a -> Double -> a
resizeD d k = mkD (names d) (map (\(a,l) -> (a,floor (k*(fromIntegral l)))) (overlaps d))

-- get the length of a diagram, based on the overlaps
dLength :: (Ld a) => a -> Double
dLength d = fromIntegral (sum (map snd (overlaps d)))

-- functions that put the buttons in the correct place on the layout.
-- Use a matrix of [[(Double,Double]], derived from the diagram, to place.


-- Will need to translate them all to Ints, since the layout function uses Ints.
intMatrix :: [[(Double,Double)]] -> [[(Int,Int)]]
intMatrix [] = []
intMatrix (p:ps) = map (\(x,y) -> (floor x, floor y)) p : intMatrix ps

-- function which gets a list of strings to use as button names.
-- two versions. One for sets (vertical buttons), one for overlaps (horizontal buttons)
-- helper function does the actual work
buttonNames :: String -> [a] -> [String]
buttonNames str ss = map (\ x -> str ++ show x) [1..n]
    where n = length ss

-- all buttons on the vertical are labelled "buttonSUx" or "buttonSDx", for up and down.
buttonS :: Bool -> [[(Int,Int)]] -> (Int,[(Int,String)])
buttonS b ps = if b then (x,ts) else (x,ss)
    where x = fst (head $ head ps)
          ts = tail $ zip (map snd (head ps)) (buttonNames "buttonSU" (head ps))
          ss = init $ zip (map snd (head ps)) (buttonNames "buttonSD" (head ps))


-- all buttons on the horizontal are labelled "buttonOLx" and "buttonORx", for left and right.
buttonO :: Bool -> [[(Int,Int)]] -> (Int,[(Int,String)])
buttonO b ps = if b then (y,ts) else (y,ss)
    where y = snd (last $ head ps)
          ts = init $ zip (map (fst.head) ps) (buttonNames "buttonOR" [1..length ps])
          ss = tail $ zip (map (fst.head) ps) (buttonNames "buttonOL" [1..length ps])


-- Function to extract some information from the label, to be passed to the appropriate function.
-- Need to throw away the first seven letters of a string, and then do something. 
-- Extremely unsafe if called elsewhere.
getSwapInds :: String -> (Int,Int)
getSwapInds str
    | (head str') == 'D' = (read (tail str'),(read (tail str') + 1))
    | (head str') == 'R' = (read (tail str'),(read (tail str') + 1))
    | (head str') == 'U' = (read (tail str'),(read (tail str') - 1))
    | (head str') == 'L' = (read (tail str'),(read (tail str') - 1))
         where str' = drop 7 str


-- function which puts the set buttons at given points on a given layout.
-- needs to know about the diagram and the current set of colours, so can put the correct function in place
putButtonsSets :: (Ld a) => Window -> Layout -> (a,[(Double,Double,Double)]) -> ArrowType -> (Int,[(Int,String)]) -> IO (ConnectId Button)
putButtonsSets w lay (d,cs) arr (x,[(y,str)]) = do
                                     let lab = str
                                     let (n,m) = getSwapInds str
                                     str <- buttonNew
                                     widgetSetSizeRequest str 20 20
                                     arrow1 <- arrowNew arr ShadowEtchedIn
                                     containerAdd str arrow1
                                     layoutPut lay str x y
                                     onClicked str (do widgetDestroy w
                                                       mainD (verReorder n m d, switchInd n m cs))
                                                       
putButtonsSets w lay (d,cs) arr (x,(y,str):ps) = do
                                     let lab = str
                                     let (n,m) = getSwapInds str
                                     str <- buttonNew
                                     widgetSetSizeRequest str 20 20
                                     arrow1 <- arrowNew arr ShadowEtchedIn
                                     containerAdd str arrow1
                                     layoutPut lay str x y
                                     onClicked str (do widgetDestroy w
                                                       mainD (verReorder n m d, switchInd n m cs))
                                                                                   
                                     putButtonsSets w lay (d,cs) arr (x,ps)

-- function which puts the overlap buttons at given points on a given layout
-- Like above, needs to know about the diagram
putButtonsOverlaps :: (Ld a) => Window -> Layout -> (a,[(Double,Double,Double)]) -> ArrowType -> (Int,[(Int,String)]) -> IO (ConnectId Button)
putButtonsOverlaps w lay (d,cs) arr (y,[(x,str)]) = do
                                     let lab = str
                                     let (n,m) = getSwapInds str
                                     str <- buttonNew
                                     widgetSetSizeRequest str 20 20
                                     arrow1 <- arrowNew arr ShadowEtchedIn
                                     containerAdd str arrow1
                                     layoutPut lay str x y
                                     onClicked str (do widgetDestroy w
                                                       mainD (horReorder n m d,cs))
putButtonsOverlaps w lay (d,cs) arr (y,(x,str):ps) = do
                                     let lab = str
                                     let (n,m) = getSwapInds str
                                     str <- buttonNew
                                     widgetSetSizeRequest str 20 20
                                     arrow1 <- arrowNew arr ShadowEtchedIn
                                     containerAdd str arrow1
                                     layoutPut lay str x y
                                     onClicked str (do widgetDestroy w
                                                       mainD (horReorder n m d,cs))
                                     putButtonsOverlaps w lay (d,cs) arr (y,ps)


-- function which puts all the buttons in the correct places for a given diagram. Requires the height of a diagram, too
putButts :: (Ld a) => Window -> (a,[(Double,Double,Double)]) -> Layout -> (Double, Double) -> Double -> Double -> IO (ConnectId Button)
putButts w (d,cs) lay (x,y) h vSize = do
                           putButtonsSets w lay (d,cs) ArrowUp (buttonS True (intMatrix $ getPointsD d (x+10,y+80) h))
                           putButtonsSets w lay (d,cs) ArrowDown (buttonS False (intMatrix $ getPointsD d (x+10,y+98) h))
                           putButtonsOverlaps w lay (d,cs) ArrowRight (buttonO True (intMatrix $ getMidPointsD d (x + 95,vSize-(n-3)*h) h))
                           putButtonsOverlaps w lay (d,cs) ArrowLeft (buttonO False (intMatrix $ getMidPointsD d (x + 78,vSize-(n-3)*h) h))
    where n = fromIntegral (length (names d))


-- TEST DATA

starts2 :: [(Double,Double)]
starts2 = [(30,30),(30,50),(30,70)]

bools2 :: [Bool]
bools2 = [True,True,True]

-- test start points
starts :: [[(Double,Double)]]
starts = [[(30,30),(30,50),(30,70)],[(60,30),(60,50),(60,70)],[(75,30),(75,50),(75,70)],[(100,30),(100,50),(100,70)],[(150,30),(150,50),(150,70)]]

bools :: [[Bool]]
bools = [[True,False,True],[True,True,False],[False,False,True],[True,True,True],[False,True,False]]

d1 = LP ["Animal","Plant","Mineral","Fungus","Water"] [([Pr,Ab,Pr,Ab,Ab],97),([Pr,Pr,Ab,Ab,Pr],36),([Ab,Pr,Ab,Pr,Ab],102),([Ab,Pr,Pr,Ab,Pr],70),([Ab,Ab,Pr,Ab,Ab],84),([Ab,Ab,Ab,Pr,Pr],34)]
