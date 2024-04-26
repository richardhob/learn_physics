
module Main where

import Graphics.Gnuplot.Simple

import Physics.Mechanics1D
import Physics.Newton2 (fAir)

gravity :: Force
gravity = 9.0665

dampedForces :: [State1D -> Force]
dampedForces 
    = let spring = springForce 0.8
          air (_, _, v0) = fAir 2 1.225 (pi * 0.02**2) v0
          ping_pong_ball _ = -0.0027 * gravity
      in [spring, air, ping_pong_ball]

dampedStates :: [State1D]
dampedStates = statesTXV 0.001 0.0027 (0.0, 0.1, 0.0) dampedForces

dampedGraph :: IO ()
dampedGraph
    = let details = [Title "Ping Pong Ball on a Slinky"
                    ,XLabel "Time (s)"
                    ,YLabel "Position (m)"
                    ,PNG "notes/images/ch15_damped_HO.png"]
          (time_x, pos, vel) = unzip3 (take 3000 dampedStates)
          pos_path = zip time_x pos
          vel_path = zip time_x vel
          style = [(defaultStyle {lineSpec = CustomStyle [LineTitle "Position"]}, pos_path)
                  ,(defaultStyle {lineSpec = CustomStyle [LineTitle "Velocity"]}, vel_path)]
      in plotListsStyle details style 

main :: IO ()
main = dampedGraph
