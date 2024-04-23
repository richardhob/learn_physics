
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
    = let net_forces v = sum [f v | f <- forces]
          a t = net_forces t / mass
      in antiDerivative dt x0 (antiDerivative dt v0 a)

-- Exercise 14.5
--
-- Rewrite bikeVelocity to use velocityFtv instead of velocityFv
plot_14_5 :: IO () 
plot_14_5 
    = let bikeVelocity :: Time -> Velocity
          bikeVelocity = let constant _ _ = 100
                             air _ v = fAir 2 1.225 0.6 v
                         in velocityFv 0.01 70 0 [constant air]
          details = [Title "Bike Velocity"
                    ,XLabel "Time (s)"
                    ,YLabel "Velocity of Bike (m/s)"
                    ,PNG "notes/images/ch14_e14_5.png"
                    ,Key Nothing]
      in plotFunc details [0,0.5..60] bikeVelocity

-- Exercise 14_6 - skip cause it's BORING
-- Exercise 14_7 - skip cause it's BORING
-- Exercise 14_8 - skip casue it's BORING

-- Exercise 14_9
positionFtv :: R -> Mass -> Position -> Velocity -> [(Time,Velocity) -> Force] -> Time -> Position
positionFtv dt mass x0 v0 forces t 
    = let v = velocityTV dt mass (t,v0) forces
      in antiDerivative dt x0 v t

-- Exercise 14_10
plot_14_10 :: IO ()
plot_14_10 
    = let pedalCoast :: Time -> Force
          pedalCoast t = let remainder :: Integer
                             remainder = (truncate t) `mod` 20
                         in if remainder < 10 then 10 else 0

          pcTV (t, _) = pedalCoast t
          fAirTV (_, v) = fAir 2 1.225 0.5 v

          pos :: Time -> Position
          pos = positionFtv 0.1 20 0 0 [pcTV, fAirTV]
          details = [Title "Pedaling and coasting with air"
                    ,XLabel "Time (s)"
                    ,YLabel "Position of Bike (m)"
                    ,PNG    "notes/images/ch14_e14_10.png"
                    ,Key    Nothing]

      in plotFunc details [0,0.1..100] pos

-- Exercise 14.11
--
-- Do some Euler methods by hand, using:
--
-- F (t, v0) = F1(t) + F2(V0) = 4 cos 2t - 3 v0
--
-- where v0 = 2 m/s
--       dt = 0.1
--
-- Evaluate for 0.3 s
--
-- F (0.1, 2) = 4 - 6 = (-2)
-- F'(0.1, 2) = 2 + 0.1 * (-2) = 1.8
--
-- F (0.2, 1.8) = 3.920 - ... = -1.480
-- F'(0.2, 1.8) = 1.8 + 0.1 * (-1.480) = 1.652
--
-- F (0.3, 1.652) = 3.684 - ... = -1.272
-- F'(0.3, 1.652) = 1.652 + (0.1 * (-1.272)) = 1.524  
--
-- F (0.4, 1.524) = 3.300 - ... = -1.272
-- F'(0.4, 1.524) = 1.524 + (0.1 * (-1.272) = 1.400
--
-- Exercise 14.12
--
-- Plot v vs. t fro 14.11
--
plot_14_12 :: IO ()
plot_14_12 
    = let f_t (t, _) = 4 * cos (2 * t)
          f_v (_, v) = -3 * v
          ts = 0.01
          vel t = velocityTV ts 1 (0, 2) [f_t, f_v] t
          details = [Title "Velocity vs. Time"
                    ,XLabel "Time (s)"
                    ,YLabel "Velocity (m/s)"
                    ,PNG "notes/images/ch14_e14_12.png"
                    ,Key Nothing]
      in plotFunc details [0.0,ts..1] vel

-- Exercise 14.13
--
-- F(t,v) = av where a = -1 N s/m
--
-- Plot the velocity as a function of time. Compare results to the exact
-- solution: 
--
-- 8e^{-at}
plot_14_13 :: IO ()
plot_14_13
    = let force :: (Time, Velocity) -> Force
          force (_, v) = (-1 * v)
          ts = 0.1
          vel dt = velocityTV dt 1 (0, 8) [force]
          actual t = 8 * (exp  ((-1) * t))
          details = [Title "Velocity vs. Time"
                    ,XLabel "Time (s)"
                    ,YLabel "Velocity (m/s)"
                    ,PNG "notes/images/ch14_e14_13.png"
                    ,Key Nothing]
      in plotFuncs details [0.0,ts..1] [(vel ts), (actual)]

main :: IO ()
main = do
    plot_14_2
    plot_14_5
    plot_14_10
    plot_14_12
    plot_14_13
