
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
