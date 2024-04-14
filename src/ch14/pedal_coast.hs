
module Main where

import Graphics.Gnuplot.Simple

import Physics.Newton2

pedalCoast :: Time -> Force
pedalCoast t = let cycle_time = 20 -- seconds
                   remainder :: Integer
                   remainder = (truncate t) `mod` cycle_time
               in if remainder < 10 then 10 else 0

childGraph :: IO ()
childGraph = let details = [Title "Child pedaling then coasting"
                           ,XLabel "Time (s)"
                           ,YLabel "Position of Bike (m)"
                           ,PNG "notes/images/ch14_pedal_coast.png"
                           ,Key Nothing]
             in plotFunc details [0..40 :: R] (positionFt 0.1 20 0 0 [pedalCoast])

main :: IO ()
main = childGraph
