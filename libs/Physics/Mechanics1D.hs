
module Physics.Mechanics1D where

import Physics.Newton2 (fAir)

type R = Double

-- Type synonyms
type Time       = R
type TimeStep   = R
type Mass       = R
type Position   = R
type Velocity   = R
type Force      = R

type State1D = (Time, Position, Velocity)

newtonSecond1D :: Mass -> [State1D -> Force] -> State1D -> (R, R, R)
newtonSecond1D mass forces (t0, x0, v0)
    = let net_force = sum [f (t0, x0, v0) | f <- forces]
          acc = net_force / mass
      in (1, v0, acc)

euler1D :: TimeStep -> (State1D -> (R,R,R)) -> State1D -> State1D
euler1D dt f (t0, x0, v0)
    = let (_, _, dvdt) = f (t0, x0, v0)
          t1 = t0 + dt
          x1 = x0 + v0 * dt
          v1 = v0 + dvdt * dt
      in (t1, x1, v1)

updateTXV :: TimeStep -> Mass -> [State1D -> Force] -> State1D -> State1D
updateTXV dt mass forces = euler1D dt (newtonSecond1D mass forces)

statesTXV :: TimeStep -> Mass -> State1D -> [State1D -> Force] -> [State1D]
statesTXV dt mass initial forces = iterate (updateTXV dt mass forces) initial

velocity1D :: [State1D] -> Time -> Velocity
velocity1D states t
    = let (t0,_,_) = head $ take 1 states
          (t1,_,_) = head $ take 1 $ drop 1 states
          dt = t1 - t0
          steps = abs $ round (t / dt)
          (_,_,vn) = head $ take 1 $ drop steps states
      in vn

velocityFTXV :: TimeStep -> Mass -> State1D -> [State1D -> Force] -> Time -> Velocity
velocityFTXV dt mass txv0 forces = velocity1D (statesTXV dt mass txv0 forces)

position1D :: [State1D] -> Time -> Position
position1D states t
    = let (t0,_,_) = head $ take 1 states
          (t1,_,_) = head $ take 1 $ drop 1 states
          dt = t1 - t0
          steps = abs $ round (t / dt)
          (_,xn,_) = head $ take 1 $ drop steps states
      in xn

positionFTXV :: TimeStep -> Mass -> State1D -> [State1D -> Force] -> Time -> Position
positionFTXV dt mass txv0 forces = position1D (statesTXV dt mass txv0 forces)

springForce :: R -> State1D -> Force
springForce k (_,x0,_) = -k * x0
