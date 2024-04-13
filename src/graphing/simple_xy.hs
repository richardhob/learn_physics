
module Main where

import Graphics.Gnuplot.Simple

type R = Double

tRange :: [R]
tRange = [0,0.01..5]

yPos :: R -> R -> R -> R -> R
yPos y0 vy0 ay t = y0 + vy0 * t + ay * t**2 / 2

plot1 ::  IO ()
plot1 = let details = [Title  "Projectile Motion"
                      ,XLabel "Time (s)"
                      ,YLabel "Projectile Height (m)"
                      ,PNG    "notes/images/graphing-simple-xy.png"
                      ,Key    Nothing
                      ] 
        in plotFunc details tRange (yPos 0 20 (-9.8))

main :: IO ()
main = plot1
