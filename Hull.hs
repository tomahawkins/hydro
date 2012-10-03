module Main (main) where

import Language.DevSurf

main :: IO ()
main = do
  print $ canFlatten 0.1 twist
  --print $ map (canFlatten 0.5) sponson
  writeFile "Hull.stl" $ exportSTL $ concatMap (meshPanel LHR) sponson
  writeFile "Hull.svg" $ exportSVG $ layout sponson

layout :: [Panel] -> [Curve]
layout panels = move (20, 20, 0) $ scale (660, 660, 1) $ f $ map (head . bound . (: []) . profile . flatten) panels
  where
  f :: [Curve] -> [Curve]
  f [] = []
  f (a : b) = a : move (0, my, 0) (f b)
    where
    my' = maximum [ y | (_, y, _) <- a ]
    my = my' * 1.15

sponson :: [Panel]
sponson =
  [ sidePanel
  , bottomPanel
  , insidePanel
  , topPanel
  , backPanel
  ]
  where
  rail = subdivideN 5
    [ (0  , 1   , 0)
    , (0.2, 0.85, 0)
    , (0.3, 0.5 , 0)
    , (0.3, 0   , 0)
    ]
  chine  = rotateY ( 30 * pi / 180) $ scale (0.8 , 1, 1) rail
  bottom = rotateY ( 90 * pi / 180) $ scale (0.4 , 1, 1) rail
  top    = rotateY (-90 * pi / 180) $ scale (0.15, 1, 1) rail

  sidePanel   = loft rail chine
  bottomPanel = loft chine bottom
  insidePanel = loft bottom top
  topPanel    = loft top rail
  backPanel   = [last bottom, last top, last chine, last rail]


bound :: [Curve] -> [Curve]
bound curves = [ move m curve | curve <- curves ]
  where
  (xs, ys, _) = unzip3 $ concat curves
  m = (- minimum xs, - minimum ys, 0)

twist :: Panel
twist = loft c1 c2
  where
  c1 = subdivideN 3 [(0, 0, 0), (1, 1, 0)]
  c2 = subdivideN 3 [(1, 0, 1), (0, 1, 1)]

-- As a test for developable, check that the dot product between corresponding normal vectors is below a certain threshold.

