module Drasil.GamePhysics.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands

import Data.Drasil.SI_Units(kilogram, metre, m_2, newton, second)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, 
  angularAccel, angularDisplacement, angularVelocity, displacement, distance, 
  force, gravitationalAccel, gravitationalConst, impulseS, impulseV, 
  linearAccel, linearDisplacement, linearVelocity, momentOfInertia, position, 
  restitutionCoef, time, torque, velocity, kEnergy)
import qualified Data.Drasil.Quantities.Math as QM (euclidNorm, normalVect, 
  orientation, perpVect, pi_, unitVect)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.Units.Physics (accelU, angVelU, impulseU, momtInertU, 
  torqueU, velU)

import Control.Lens((^.))

defSymbols :: [DefinedQuantityDict]
defSymbols = (map dqdWr unitSymbs) ++ (map dqdWr inputConstraints) ++
  (map dqdWr outputConstraints)

unitSymbs :: [UnitaryConceptDict]
unitSymbs = map ucw unitalChunks ++ map ucw [iVect, jVect, normalVect,
 force_1, force_2, forceI, mass_1, mass_2, dispUnit, 
  dispNorm, sqrDist, velA, velB, velO, rOB, angVelA, angVelB,
  posCM, massI, posI, accI, mTot, velI, torqueI, timeC, initRelVel, 
  massA, massB, massIRigidBody, normalLen, contDispA, contDispB, 
  perpLenA, momtInertA, perpLenB, momtInertB, timeT, inittime, 
  momtInertK, pointOfCollision, contDispK, collisionImpulse, velAP, velBP ]

----------------------
-- TABLE OF SYMBOLS --
----------------------

symbols, symbolsAll, inputSymbols, outputSymbols :: [QuantityDict]

symbolsAll = symbols ++ inputSymbols ++ outputSymbols

symbols = (map qw unitalChunks) ++ 
  (map qw unitless) ++ 
  (map qw inputConstraints)

inputSymbols = map qw [QP.position, QP.velocity, QP.force, QM.orientation, 
  QP.angularVelocity, QP.linearVelocity, QP.gravitationalConst, QPP.mass, 
  QPP.len, QP.momentOfInertia, QP.torque, QP.kEnergy] ++ [qw QP.restitutionCoef]

outputSymbols = map qw [QP.position, QP.velocity, QM.orientation, 
  QP.angularVelocity]


unitalChunks :: [UnitalChunk]
unitalChunks = [QP.acceleration, QP.angularAccel, QP.gravitationalAccel, 
  QP.impulseV, QP.impulseS, iVect, jVect, normalVect, QP.distance, QP.displacement, 
  QP.time, QP.angularDisplacement, posCM, posI, massI, mTot, accI, velI,
  QP.linearDisplacement, QP.linearVelocity, QP.linearAccel, initRelVel, normalLen,
  perpLenA, perpLenB, forceI, torqueI, timeC, velA, velB, massA, massB,
  angVelA, angVelB, force_1, force_2, mass_1, mass_2, dispUnit, 
  dispNorm, sqrDist, velO, rOB, massIRigidBody, contDispA, contDispB, 
  momtInertA, momtInertB, timeT, inittime, momtInertK, pointOfCollision, 
  contDispK, collisionImpulse, QP.kEnergy, finRelVel, velAP, velBP]
-----------------------
-- PARAMETRIZED HACK --
-----------------------
--FIXME: parametrized hack
--FIXME: "A" is not being capitalized when it should be.
forceParam, massParam, momtParam, contParam :: String -> String -> UnitalChunk
forceParam n w = ucs'
 (dccWDS ("force" ++ n) (cn $ "force exerted by the " ++ w ++ 
  " body (on another body)") (phrase QP.force)) 
  (sub (eqSymb QP.force) (Atomic n)) Real newton

massParam n w = ucs'
 (dccWDS ("mass" ++ n) (cn $ "mass of the " ++ w ++ " body") 
  (phrase QPP.mass)) (sub (eqSymb QPP.mass) (Atomic n)) Real kilogram

momtParam n w = ucs'
 (dccWDS ("momentOfInertia" ++ n) (compoundPhrase'
  (QP.momentOfInertia ^. term) (cn $ "of rigid body " ++ n))
  (phrase QP.momentOfInertia)) (sub (eqSymb QP.momentOfInertia) (Atomic w)) Real momtInertU

contParam n w = ucs'
 (dccWDS ("r_" ++ n ++ "P") (contdispN n) 
  (phrase QP.displacement)) (sub (eqSymb QP.displacement)
  (Concat [Atomic w, cP])) Real metre

contdispN :: String -> NP
contdispN n = cn $ "displacement vector between the centre of mass of rigid body " 
  ++ n ++ " and contact point P"

perpParam, rigidParam, velParam, 
  angParam :: String -> Symbol -> UnitalChunk

velParam n w = ucs'
 (dccWDS ("velocity" ++ n) (compoundPhrase' (QP.velocity ^. term)
  (cn $ "at point " ++ n)) (phrase QP.velocity)) (sub (eqSymb QP.velocity) w) Real velU

angParam n w = ucs'
 (dccWDS ("angular velocity" ++ n) (compoundPhrase'
  (cn $ n ++ " body's") (QP.angularVelocity ^. term))
  (phrase QP.angularVelocity)) (sub (eqSymb QP.angularVelocity) w) Real angVelU

perpParam n w = ucs'
 (dccWDS ("|| r_A" ++ n ++ " x n ||") 
  (compoundPhrase' (compoundPhrase (cn' "length of the") (QM.perpVect ^. term))
  (cn $ "to the contact displacement vector of rigid body " ++ n)) 
  (phrase QM.perpVect)) (Concat [Atomic "||", w, Atomic "*", --should be x for cross
  (eqSymb QM.perpVect), Atomic "||"]) Real metre

rigidParam n w = ucs'
 (dccWDS ("rig_mass" ++ n) (compoundPhrase' (QPP.mass ^. term)
  (cn $ "of rigid body " ++ n)) (phrase QPP.mass)) (sub (eqSymb QPP.mass) w) Real kilogram

-----------------------
-- CHUNKS WITH UNITS --
-----------------------

iVect, jVect, normalVect, force_1, force_2, forceI, mass_1, mass_2, dispUnit, 
  dispNorm, sqrDist, velA, velB, velO, rOB, angVelA, angVelB,
  posCM, massI, posI, accI, mTot, velI, torqueI, timeC, initRelVel, 
  massA, massB, massIRigidBody, normalLen, contDispA, contDispB, 
  perpLenA, momtInertA, perpLenB, momtInertB, timeT, inittime, 
  momtInertK, pointOfCollision, contDispK, collisionImpulse, finRelVel, velAP, velBP :: UnitalChunk

iVect = ucs' (dccWDS "unitVect" (compoundPhrase' (cn "horizontal")
               (QM.unitVect ^. term)) (phrase QM.unitVect)) 
               (eqSymb QM.unitVect) Real metre
jVect       = ucs' (dccWDS "unitVectJ" (compoundPhrase' (cn "vertical")
               (QM.unitVect ^. term)) (phrase QM.unitVect)) (vec $ hat lJ) Real metre
normalVect  = ucs' (dccWDS "normalVect" (compoundPhrase' (cn "collision")
                   (QM.normalVect ^. term)) (phrase QM.normalVect)) 
                   (eqSymb QM.normalVect) Real metre

dispUnit = ucs' (dccWDS "dispUnit" (cn "displacement unit vector") 
                   (S "displacement" +:+ (phrase QM.unitVect))) (vec (hat lR)) Real metre

dispNorm = ucs' (dccWDS "euclideanNormDisp" (cn "Euclidean norm of the displacement")
               (phrase QM.euclidNorm) ) (eqSymb QM.euclidNorm) Real metre

sqrDist = ucs' (dccWDS "euclideanNorm" (cn' "squared distance")
               (phrase QM.euclidNorm) ) (sup (eqSymb QM.euclidNorm) 
               (Atomic "2")) Real m_2

rOB    = uc' "rOB" 
  (nounPhraseSP "displacement vector between the origin and point B")
  "FIXME: Define this or remove the need for definitions" 
  (sub (eqSymb QP.displacement) (Concat [cO, cB])) metre

{-r_F    = uc' "r_F" 
  (nounPhraseSP "position vector of the point where is applied, measured from the axis of rotation")
  (sub (eqSymb QP.displacement) (Concat [cO, cB])) metre-}
  
posCM = ucs "p_CM" (nounPhraseSP "Center of Mass")
 --"mass-weighted average position of a rigid " ++
 -- "body's particles") 
  "FIXME: Define this or remove the need for definitions" 
  (sub (eqSymb QP.position) (Atomic "CM")) Real metre

massI = ucs' (dccWDS "m_j" (compoundPhrase' (QPP.mass ^. term)
                (cn "of the j-th particle")) (phrase QPP.mass)) 
                (sub (eqSymb QPP.mass) lJ) Real kilogram

posI = ucs' (dccWDS "p_j" (compoundPhrase' (QP.position ^. term) 
               (cn "vector of the j-th particle")) (phrase QP.position))
               (sub (eqSymb QP.position) lJ) Real metre

accI = ucs' (dccWDS "accI" (compoundPhrase' (cn "the i-th body's")
               (QP.acceleration ^. term)) (phrase QP.acceleration))
               (sub (eqSymb QP.acceleration) lI) Real accelU

velI = ucs' (dccWDS "velI" (compoundPhrase' (QP.velocity ^. term) 
               (cn "of the i-th body's velocity")) (phrase QP.velocity))
               (sub (eqSymb QP.velocity) lI) Real velU

torqueI = ucs' (dccWDS "torqueI" 
               (cn "torque applied to the i-th body")
               (phrase QP.torque)) (sub (eqSymb QP.torque) lI) Real torqueU

mTot = ucs' (dccWDS "M" (compoundPhrase' (cn "total mass of the") 
                 (CP.rigidBody ^. term)) (phrase QPP.mass)) cM Real kilogram

timeC = ucs' (dccWDS "timeC" (cn "denotes the time at collision") 
                (phrase QP.time)) (sub (eqSymb QP.time) lC) Real second

initRelVel = ucs' (dccWDS "v_i^AB" (compoundPhrase'
                 (compoundPhrase' (cn "initial relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B")) (phrase QP.velocity))
                 (sup (sub (eqSymb QP.velocity) lI) (Concat [cA, cB])) Real velU

finRelVel = ucs' (dccWDS "v_f^AB" (compoundPhrase'
                 (compoundPhrase' (cn "final relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B")) (phrase QP.velocity))
                 (sup (sub (eqSymb QP.velocity) lF) (Concat [cA, cB])) Real velU

massIRigidBody = ucs' (dccWDS "massI" (compoundPhrase' (QPP.mass ^. term) 
                (cn "of the i-th rigid body")) (phrase QPP.mass)) 
                (sub (eqSymb QPP.mass) lI) Real kilogram
normalLen = ucs' (dccWDS "length of the normal vector" (compoundPhrase'
                  (cn "length of the") (QM.normalVect ^. term)) 
                  (phrase QM.normalVect))
                  (Concat [Atomic "||",(eqSymb QM.normalVect), Atomic "||"]) Real metre

timeT = ucs' (dccWDS "t" (cn "point in time") (phrase QP.time))
                (eqSymb QP.time) Real second

inittime = ucs' (dccWDS "t_0" (cn "denotes the initial time") 
                (phrase QP.time)) (sub (eqSymb QP.time) (Atomic "0")) Real second

momtInertK = ucs' (dccWDS "momentOfInertiaK" (compoundPhrase'
               (QP.momentOfInertia ^. term) 
               (cn "of the k-th rigid body"))
               (phrase QP.momentOfInertia)) 
               (sub (eqSymb QP.momentOfInertia) lK) Real momtInertU

pointOfCollision = ucs' (dccWDS "point_c" (cn "point of collision") 
                 (S "point")) cP Real metre

collisionImpulse = ucs' (dccWDS "collisionImp" (compoundPhrase' 
                (cn "collision") (QP.impulseS ^. term)) (phrase QP.impulseS)) 
                (eqSymb QP.impulseS) Real impulseU

forceI = ucs' (dccWDS "forceI" (compoundPhrase' 
      (QP.force ^. term) (cn "applied to the i-th body at time t")) 
      (phrase QP.force)) (sub (eqSymb QP.force) lI) Real newton

velAP = ucs' (dccWDS "v^AP" (compoundPhrase' (QP.velocity ^. term)
              (cn "of the point of collision P in body A")) 
              (phrase QP.velocity))(sup (eqSymb QP.velocity)(Concat [cA, cP])) Real velU
velBP = ucs' (dccWDS "v^BP" (compoundPhrase' (QP.velocity ^. term)
              (cn "of the point of collision P in body B")) 
              (phrase QP.velocity))(sup (eqSymb QP.velocity)(Concat [cB, cP])) Real velU

force_1    = forceParam "1" "first"
force_2    = forceParam "2" "second"
mass_1     = massParam  "1" "first"
mass_2     = massParam  "2" "second"
velA       = velParam   "A" cA
velB       = velParam   "B" cB
velO       = velParam   "origin" cO
angVelA    = angParam   "A" cA
angVelB    = angParam   "B" cB
perpLenA   = perpParam  "A" $ eqSymb contDispA
perpLenB   = perpParam  "B" $ eqSymb contDispB
momtInertA = momtParam  "A" "A"
momtInertB = momtParam  "B" "B"
contDispA  = contParam  "A" "A"
contDispB  = contParam  "B" "B"
contDispK  = contParam  "k" "k"
massA      = rigidParam "A" cA
massB      = rigidParam "B" cB

--------------------------
-- CHUNKS WITHOUT UNITS --
--------------------------

unitless :: [QuantityDict]
unitless = qw QM.pi_ : [numParticles]

numParticles :: QuantityDict
numParticles = vc "n" (nounPhraseSP "number of particles in a rigid body") lN Integer

-----------------------
-- CONSTRAINT CHUNKS --
-----------------------

lengthCons, massCons, mmntOfInCons, gravAccelCons, posCons, orientCons,
  angVeloCons, forceCons, torqueCons, veloCons, restCoefCons :: ConstrConcept

inputConstraints :: [UncertQ]
inputConstraints = map (`uq` defaultUncrt)
  [lengthCons, massCons, mmntOfInCons, gravAccelCons, posCons, orientCons,
  veloCons, angVeloCons, forceCons, torqueCons, restCoefCons]

outputConstraints :: [UncertQ]
outputConstraints = map (`uq` defaultUncrt) 
  [posCons, veloCons, orientCons, angVeloCons]

nonNegativeConstraint :: Constraint -- should be pulled out and put somewhere for generic constraints
nonNegativeConstraint = physc $ UpFrom (Inc,0)

lengthCons     = constrained' QPP.len               [nonNegativeConstraint] (dbl 44.2)
massCons       = constrained' QPP.mass              [nonNegativeConstraint] (dbl 56.2)
mmntOfInCons   = constrained' QP.momentOfInertia    [nonNegativeConstraint] (dbl 74.5)
gravAccelCons  = constrained' QP.gravitationalConst [] (dbl 9.8)
posCons        = constrained' QP.position           [] (dbl 0.412) --FIXME: should be (0.412, 0.502) vector
veloCons       = constrained' QP.velocity           [] (dbl 2.51)
orientCons     = constrained' QM.orientation        [] (sy QM.pi_ / 2) -- physical constraint not needed space is radians
angVeloCons    = constrained' QP.angularVelocity    [] (dbl 2.1)
forceCons      = constrained' QP.force              [] (dbl 98.1)
torqueCons     = constrained' QP.torque             [] (dbl 200)
restCoefCons   = constrained' QP.restitutionCoef    [physc $ Bounded (Inc,0) (Inc,1)] (dbl 0.8)

---------------------
-- INSTANCE MODELS --
---------------------

transMotLegTerms, rotMotLegTerms, col2DLegTerms :: [UnitalChunk]
transMotLegTerms = [massIRigidBody, QP.gravitationalAccel, timeT, inittime, posCM,
  QP.acceleration, QP.velocity, forceI]

rotMotLegTerms = [massIRigidBody, QP.gravitationalAccel, timeT, inittime,
  QM.orientation, QP.angularVelocity, QP.angularAccel, torqueI, momtInertK]

col2DLegTerms = [massIRigidBody, momtInertK, timeT, inittime, timeC, posCM,
  QP.velocity, QM.orientation, QP.angularVelocity, normalVect, -- +:+. S "Its signed direction is determined by (A4)",
  collisionImpulse, pointOfCollision, contDispK]

---------------------
-- GOAL STATEMENTS --
---------------------

