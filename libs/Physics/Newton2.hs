
module Physics.Newton2 where

-- import Graphics.Gnuplot.Simple

type R = Double

-- Type synonyms
type Mass       = R
type Time       = R
type Position   = R
type Velocity   = R
type Force      = R

-- Find the velocity as a function of Time when provided an initial velocity
-- and some forces.
velocityCF :: Mass -> Velocity -> [Force] -> (Time -> Velocity)
velocityCF mass v0 forces = let net_forces = sum forces
                                a0 = net_forces / mass
                                v t = v0 + a0 * t
                            in v

-- Find the Position as a function of Time when provided an initial velocity,
-- initial position, and some forces
positionCF :: Mass -> Position -> Velocity -> [Force] -> (Time -> Position)
positionCF mass x0 v0 forces = let net_forces = sum forces
                                   a0 = net_forces / mass
                                   x t = x0 + v0 * t + a0 * (t**2) / 2
                               in x

-- Get the the velocity function when forces depend on time
--
-- Args:
--   dt = Integration time step
--   mass = mass
--   v0 = Initial velocity
--   forces = list of force functions
velocityFt :: R -> Mass -> Velocity -> [(Time -> Force)] -> (Time -> Velocity)
velocityFt dt mass v0 forces = let net t = sum [f t | f <- forces]
                                   a t = net t / mass
                               in antiDerivative dt v0 a

positionFt :: R -> Mass -> Position -> Velocity -> [(Time -> Force)] -> (Time -> Position)
positionFt dt mass x0 v0 forces = antiDerivative dt x0 (velocityFt dt mass v0 forces)

antiDerivative :: R -> R -> (R -> R) -> (R -> R)
antiDerivative dt v0 a t = v0 + integral dt a 0 t

integral :: R -> (R -> R) -> R -> R -> R
integral dt f a b = sum [f t * dt | t <- [a+dt/2, a+3*dt/2 .. b-dt/2]]
