
module Main where

import Graphics.Gnuplot.Simple
import Physics.Newton2

-- From pedal_coast.hs
pedalCoast :: Time -> Force
pedalCoast t = let cycle_time = 20 -- seconds
                   remainder :: Integer
                   remainder = (truncate t) `mod` cycle_time
               in if remainder < 10 then 10 else 0

pedalCoastAir :: [(Time, Velocity)]
pedalCoastAir = let pedalCoastTV (t, _) = pedalCoast t
                    fAirTV (_, v) = fAir 2 1.225 0.5 v
                in statesTV 0.1 20 (0, 0) [pedalCoastTV, fAirTV]

pedalCoastAirV :: Time -> Velocity
pedalCoastAirV t = let pedalCoastTV (t, _) = pedalCoast t
                       fAirTV (_, v) = fAir 2 1.225 0.5 v
                   in velocityTV 0.1 20 (0, 0) [pedalCoastTV, fAirTV] t

pedalCoastAirGraph :: IO ()
pedalCoastAirGraph
    = let time_x = [0,0.1..100]
          details = [Title "Pedaling and coasting with air"
                    ,XLabel "Time (s)"
                    ,YLabel "Velocity of Bike (m/s)"
                    ,PNG    "notes/images/ch14_pedal_coast_air.png"
                    ,Key    Nothing]
      in plotFunc details time_x pedalCoastAirV

main :: IO ()
main = pedalCoastAirGraph
