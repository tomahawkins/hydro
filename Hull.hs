module Main (main) where

import Language.DevSurf

main :: IO ()
main = writeFile "Hull.stl" $ exportSTL sponson

sponson :: Mesh
sponson = concat $ map (meshPanel LHR)
  [ loft rail chine
  , loft chine bottom
  , loft bottom top
  , loft top rail
  , [last bottom, last top, last chine, last rail]
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

{-
  bound :: [Curve] -> [Curve]
  bound curves = [ move m curve | curve <- curves ]
    where
    (xs, ys, _) = unzip3 $ concat curves
    m = (- minimum xs, - minimum ys, 0)
-}

{-
twist :: Mesh
twist = meshPanel LHR $ loft c1 c2
  where
  c1 = subdivideN 3 [(0, 0, 0), (1, 1, 0)]
  c2 = subdivideN 3 [(1, 0, 1), (0, 1, 1)]
-}

-- As a test for developable, check that the dot product between corresponding normal vectors is below a certain threshold.

