
module Main where

import Graphics.Gnuplot.Simple
import Physics.Newton2

carGraph :: IO ()
carGraph 
    = let details = [Title "Car on an air track"
                    ,XLabel "Time (s)"
                    ,YLabel "Velocity of Car (m/s)"
                    ,PNG "notes/images/ch14_car_graph.png"
                    ,Key Nothing
                    ]
      in plotFunc details [0..4 :: Time] (velocityCF 0.1 0.6 [0.04, -0.08])

main :: IO ()
main = carGraph
