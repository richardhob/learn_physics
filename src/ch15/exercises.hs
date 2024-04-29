
module Main where

import Graphics.Gnuplot.Simple

import Physics.Mechanics1D

-- Chapter 15 Exercises

-- 15.1 Someone throws a ball from the ground straight up in to the air with an
-- initial velocity of 10 m/s. Ignoring air resistance, use the function
-- `positionFtxv to find the height of the ball as a function of time. Make a
-- plot of height as a function of time.
--
-- Let's assume the ball weighs 1 kg
plot_15_1 :: IO ()
plot_15_1 
    = let mass = 1 :: Mass
          dt = 0.1 :: TimeStep
          gravity (_, _, _) = -9.8 * mass
          pos = positionFTXV dt mass (0, 0, 10) [gravity]
          time_x = [0,dt..2.2]
          details = [Title "Position of Baseball vs. Time"
                    ,XLabel "Time (s)"
                    ,YLabel "Position (m)"
                    ,PNG "notes/images/ch15_ex_15_1.png"
                    ,Key Nothing]
      in plotFunc details time_x pos

-- Skip 15_2 -> Hand calculations (BORING)
-- Skip 15_3 -> Hand calculations (BORING)

-- Take a tuple (t0, x0, v0) and return (t1, x1, v1) for a single step of the
-- euler method for the provided differential equations
--
--   dx/dt = v
--   dv/dt = -3x + 4cos2t - 2v
--
-- Plot with a dt of 0.1
plot_15_3 :: IO () 
plot_15_3 
    = let dt = 0.1
          eq t x v= -3x + 4 * cos (2 * t) - 2 * v
          ex :: DifferentialEquation (R,R,R) (R,R,R)
          ex (t0,x0,v0) = (1,v0,(eq t0 x0 v0))
              

main :: IO () 
main = do
    plot_15_1
