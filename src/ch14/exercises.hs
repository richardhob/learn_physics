
module Main where

import Physics.Newton2
import Graphics.Gnuplot.Simple

-- Exercise 14.1
velocityCF' :: Mass -> Velocity -> [Force] -> Time -> Velocity
velocityCF' mass v0 forces time
    = let net_forces = sum forces
          a0 = net_forces / mass
          v t = v0 + a0 * t
      in v time

-- Exercise 14.2
plot_14_2 :: IO ()
plot_14_2 
    = let details = [Title "Position of Air Car over Time"
                    ,XLabel "Time (s)"
                    ,YLabel "Position (m)"
                    ,PNG "notes/images/ch14_e14_2.png"
                    ,Key Nothing]
          position = positionCF 0.1 (-1) 0 [0.04, -0.08]
      in plotFunc details [0,0.1..10] position

-- Exercise 14.3
sumF :: [R -> Force] -> R -> Force
sumF forces value
    = sum [f value | f <- forces]

-- Exercise 14.4
--
-- Force only depends on the Velocity
positionFv :: R -> Mass -> Position -> Velocity -> [Velocity -> Force] -> Time -> Position
positionFv dt mass x0 v0 forces 
    = let f0 = sum [f v0 | f <- forces]
          a0 = f0 / mass
          dv = antiDerivative dt 
          net_force v = sum [f v | f <- forces]
          a t = net_force t / mass
          velocityFv = antiDerivative dt v0 a
      in antiDerivative dt x0 (

main :: IO ()
main = do
    plot_14_2
