
module Main where

import Graphics.Gnuplot.Simple
import Physics.Newton2

bikeVelocity :: Time -> Velocity
bikeVelocity 
    = let constant = const 100
          air = fAir 2 1.225 0.6
      in velocityFv 0.01 70 0 [constant, air]

bikeGraph :: IO ()
bikeGraph
    = let details = [Title "Bike Velocity"
                    ,XLabel "Time (s)"
                    ,YLabel "Velocity of Bike (m/s)"
                    ,PNG "notes/images/ch14_bike_velocity.png"
                    ,Key Nothing]
      in plotFunc details [0,0.5..60] bikeVelocity

main :: IO ()
main = bikeGraph
