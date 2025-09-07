module Main where
import Graphics.Gloss

pic :: Picture
pic = Pictures
  [ translate (-150) 0 (color azure (circleSolid 80))
  , translate   150  0 (color orange (rectangleSolid 160 100))
  ]

main :: IO ()
main = display (InWindow "Shapes" (800,600) (20,20)) black pic
