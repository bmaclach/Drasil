module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Data.Drasil.Concepts.Physics as CP (angAccel, angDisp, angVelo, 
    acceleration, displacement, distance, energy, force, gravitationalAccel, 
    gravitationalConst, impulseS, impulseV, linAccel, linDisp, linVelo, 
    momentOfInertia, position, pressure, restitutionCoef, time, torque, velocity)
import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, second)
import Data.Drasil.Units.Physics (accelU, angAccelU, angVelU, gravConstU, 
    impulseU, momtInertU, torqueU, velU)

restitutionCoef :: DefinedQuantityDict
restitutionCoef = dqd CP.restitutionCoef (sub cC cR) Real Nothing

angularAccel, angularDisplacement, angularVelocity, acceleration, displacement,
  distance, energy, force, gravitationalAccel, gravitationalConst, impulseS,
  impulseV, linearAccel, linearDisplacement, linearVelocity, momentOfInertia,
  position, pressure, time, torque, velocity :: UnitalChunk

angularAccel        = uc CP.angAccel lAlpha angAccelU
angularDisplacement = uc CP.angDisp lTheta radian
angularVelocity     = uc CP.angVelo lOmega angVelU
acceleration        = uc CP.acceleration lA accelU
displacement        = uc CP.displacement lR metre
distance            = uc CP.distance lR metre
energy              = uc CP.energy cE joule
force               = uc CP.force cF newton
gravitationalAccel  = uc CP.gravitationalAccel lG accelU
gravitationalConst  = uc CP.gravitationalConst cG gravConstU
impulseS            = uc CP.impulseS lJ impulseU
impulseV            = uc CP.impulseV cJ impulseU
linearAccel         = uc CP.linAccel (Concat [lA, Atomic "(", lT, Atomic ")"]) accelU
linearDisplacement  = uc CP.linDisp (Concat [lR, Atomic "(",lT, Atomic ")"]) metre
linearVelocity      = uc CP.linVelo (Concat [lV, Atomic "(", lT, Atomic ")"]) velU
momentOfInertia     = uc CP.momentOfInertia cI momtInertU
position            = uc CP.position lP metre
pressure            = uc CP.pressure lP pascal
time                = uc CP.time lT second
torque              = uc CP.torque lTau torqueU
velocity            = uc CP.velocity lV velU
