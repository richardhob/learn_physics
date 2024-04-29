
module Physics.Mechanics1D where

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

type UpdateFunction s = s -> s
type DifferentialEquation s ds = s -> ds
type NumericalMethod s ds = DifferentialEquation s ds -> UpdateFunction s

solver :: NumericalMethod s ds -> DifferentialEquation s ds -> s -> [s]
solver method = iterate . method

class RealVectorSpace ds where
    (+++) :: ds -> ds -> ds
    scale :: R -> ds -> ds

instance RealVectorSpace (R, R, R) where
    (dtdt0, dxdt0, dvdt0) +++ (dtdt1, dxdt1, dvdt1)
        = (dtdt0 + dtdt1, dxdt0 + dxdt1, dvdt0 + dvdt1)
    scale w (dtdt, dxdt, dvdt) = (w * dtdt, w * dxdt, w * dvdt)

class RealVectorSpace ds => Diff s ds where
    shift :: R -> ds -> s -> s

instance Diff State1D (R, R, R) where
    shift dt (dtdt, dxdt, dvdt) (t, x, v) 
        = (t + dtdt * dt, x + dxdt * dt, v + dvdt * dt)

euler :: Diff s ds => R -> (s -> ds) -> s -> s
euler dt f st0 = shift dt (f st0) st0

rungeKutta4 :: Diff s ds => R -> (s -> ds) -> s -> s
rungeKutta4 dt f st0 
    = let m0 = f                  st0
          m1 = f (shift (dt/2) m0 st0)
          m2 = f (shift (dt/2) m1 st0)
          m3 = f (shift (dt/2) m2 st0)
      in shift (dt/6) (m0 +++ m1 +++ m1 +++ m2 +++ m2 +++ m3) st0

