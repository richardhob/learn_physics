
module Main where

import Graphics.Gnuplot.Simple

import Physics.Mechanics1D
import Physics.Newton2 (fAir)

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

-- Take a tuple (t0, x0, v0) and return (t1, x1, v1) for a single step of the
-- euler method for the provided differential equations
--
--   dx/dt = v
--   dv/dt = -3x + 4cos2t - 2v
--
-- Plot with a dt of 0.1
--
-- Combined with 15_4
plot_15_3 :: IO () 
plot_15_3 
    = let dt = 0.01
          max_time = 20
          steps = abs $ round (max_time / dt)

          equation :: TimeStep -> Position -> Velocity -> R
          equation t x v= -3*x + 4*cos(2*t) - 2*v
          ex :: DifferentialEquation (R,R,R) (R,R,R)
          ex (t0,x0,v0) = (1,v0,(equation t0 x0 v0))

          states = solver (euler dt) ex (0, 2, 1)
          values = map (\(t, x, _) -> (t, x)) $ take steps states

          details = [Title "Position over Time"
                    ,XLabel "Time (s)"
                    ,YLabel "Position (m)"
                    ,PNG "notes/images/ch15_ex_15_3.png"
                    ,Key Nothing]
      in plotPath details values

-- Plot 
--
-- The Timestep being 0.01 (which is what I have it set to right now) does not
-- do a good job of estimating the result.
--
-- Using 0.0001 does, but it takes WAY longer to plot the result.
--
-- It LOOKS like the result is sinusoidal with Gravity, Friction, air
-- resistance, etc. are ignored.
plot_15_5 :: IO ()
plot_15_5
    = let dt = 0.01 :: TimeStep
          mass = 3000 :: Mass
          springK = 100000 -- N / m
          forces = [springForce springK]

          pos = positionFTXV dt mass (0,0.01,0) forces
          time_x = [0,dt..5]

          details = [Title "Linear Spring mass position over time"
                    ,XLabel "Time (s)"
                    ,YLabel "Position (m)"
                    ,PNG "notes/images/ch15_ex_15_5.png"
                    ,Key Nothing]
      in plotFunc details time_x pos

-- Let's drop a ping pong ball and a bowling ball from 100m and 500m. Make
-- graphs of velocity vs time, as well as velocity vs. vertical position.
-- 
-- Use a drag coefficient C = 1/2 and make reasonable assumptions about the
-- bowling ball and ping pong ball
plot_15_6 :: IO () 
plot_15_6 
    = let dt = 0.1
          h1 = 100 -- m
          -- h2 = 500 -- m
          c = 0.5
          rho = 1.225

          -- Bowling ball -> (10 kg, 6 * 0.25 m*m)
          -- Ping Pong ball -> (0.1 kg, 0.10 m*m)
          _get height mass area
              = let gravity (_, _, _) = -9.8 * mass
                    f_air (_, _, v) = fAir c rho area v
                    forces = [gravity, f_air]
                    states = statesTXV dt mass (0, height, 0) forces
                    s = takeWhile (\(_, x, _) -> x > 0) states
                in s

          bb_area = 1.5 -- m^2
          bb_mass = 10 -- kg

          pp_area = 0.01 -- m^2
          pp_mass = 0.01 -- kg

          bb_style = defaultStyle {lineSpec=CustomStyle [LineTitle "Bowling ball"]}
          pp_style = defaultStyle {lineSpec=CustomStyle [LineTitle "Ping Pong ball"]}

          bb_100 = _get h1 bb_mass bb_area
          bb_100_v = map (\(t, _, v) -> (t, v)) bb_100
          bb_100_p = map (\(_, x, v) -> (x, v)) bb_100
          pp_100 = _get h1 pp_mass pp_area
          pp_100_v = map (\(t, _, v) -> (t, v)) pp_100
          pp_100_p = map (\(_, x, v) -> (x, v)) pp_100

          details_100_v = [Title "Bowling Ball vs. Ping Pong Ball Velocity vs Time"
                        ,XLabel "Time (s)"
                        ,YLabel "Velocity (m/s)"
                        ,PNG "notes/images/ch15_ex_15_5_100_v.png"
                        ]

          details_100_p = [Title "Bowling Ball vs. Ping Pong Ball Velocity vs. Position"
                        ,XLabel "Position (m)"
                        ,YLabel "Velocity (m/s)"
                        ,PNG "notes/images/ch15_ex_15_5_100_p.png"
                        ]

      in do
             plotPathsStyle details_100_v [(bb_style, bb_100_v), (pp_style, pp_100_v)]
             plotPathsStyle details_100_p [(bb_style, bb_100_p), (pp_style, pp_100_p)]

-- 15_7 - Boring, repeat of 15_5 mostly

-- 15_8 - Consider an object with mss 'm' attached to a spring with a spring
-- constant 'k'. The other end of the spring is attached to a vertical wall. The
-- object slides hoizontally across the floor. There is a coefficient of kenetic
-- friction (mew_k = 0.3) between the object and the floor. The weight of the
-- object is mg, so the force of kinetic friction on the object is (mew * m *
-- g), directed opposite the velocity of the object.
--
-- Assume m = 3 kg and k = 12 N/m
--
-- a. Write a function with type State1D -> Force that gives teh horizontal
-- force of kinetic friction. You may want to use the signum function (which
-- returns '1' if the argument is positive, and '-1' if the argument is negative
--
-- b. Use the function `positionFTXV` to find the position of the object as a
-- function of time
--
-- c. Make a plot of position as a function of time
--
-- Note - working better right now, but the result is NOT quite right.
plot_15_8 :: IO ()
plot_15_8 
    = let k = 12
          mass = 3
          mew = 0.3
          dt = 0.001
          max_t = 10

          spring :: State1D -> Force
          spring = springForce k

          friction :: State1D -> Force
          friction (_, _, v) = 9.8 * mass * mew 

          total_f state = (spring state) - (friction state)

          forces = [total_f]

          pos = positionFTXV dt mass (0,1,0) forces
          time_x = [0,dt..max_t]

          details = [Title "Mass on spring with friction position vs time"
                    ,XLabel "Time (s)"
                    ,YLabel "Position (m)"
                    ,PNG "notes/images/ch15_ex_15_8.png"
                    ,Key Nothing]

      in plotFunc details time_x pos

main :: IO () 
main = do
    plot_15_1
    plot_15_3
    plot_15_5
    plot_15_6
    plot_15_8
