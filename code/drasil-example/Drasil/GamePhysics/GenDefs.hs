-- %%%%%%%%%%%%%%%%%%%%%%%%%%
-- WARNING
-- File not actually used in drasil.cabal for GamePhysics
-- so it is not actually 'plugged in'.  The definitions in here
-- may not type check anymore!
-- %%%%%%%%%%%%%%%%%%%%%%%%%%
module Drasil.GamePhysics.GenDefs (generalDefinitions) where

import Prelude hiding (sin, cos, tan)

import Language.Drasil
import Drasil.DocLang (refA)

import Data.Drasil.SentenceStructures (foldlSent, foldlSentCol)
import Data.Drasil.Concepts.Physics (rigidBody)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Units.Physics(impulseU, accelU)
import Drasil.GamePhysics.Unitals
import Data.Drasil.Quantities.Physics(force, impulseV, time, acceleration,
  velocity, gravitationalAccel)
import Drasil.GamePhysics.TMods(newtonSL, newtonTL)
import Data.Drasil.Utils (unwrap, weave)

----- General Models -----
generalDefinitions :: [GenDefn]
generalDefinitions = [gd' impulseGDef (Just impulseU) impulseDeriv "impulse" [impulseDesc],
  gd' conservationOfMomentGDef (Nothing :: Maybe DerUChunk) conservationOfMomentDeriv
   "conservOfMoment" [conservationOfMomentDesc],
  gd accelerationDueToGravityGDef (Just accelU) accelerationDueToGravityDeriv
   "conservOfMoment"]

impulseGDef :: RelationConcept
impulseGDef = makeRC "impulse" (nounPhraseSP "Impulse") 
  impulseDesc impulseRel

impulseRel :: Relation
impulseRel = sy impulseV $= (int_all (eqSymb time) (sy force)) $= sy deltaP $= (sy mass)*(sy deltaV)

impulseDesc :: Sentence
impulseDesc = foldlSent [S "An", phrase impulseV, ch impulseV, 
  S "occurs when a", phrase force, ch force, 
  S "acts over an interval of", phrase time]

--[impulseS, force, changeInMomentum, mass, changeInVelocity]

impulseDeriv :: Derivation
impulseDeriv = (weave [impulseDeriv_sentences, map E impulseDeriv_eqns])

impulseDeriv_sentences :: [Sentence]
impulseDeriv_sentences = map foldlSentCol [gd1_desc1, gd1_desc2, gd1_desc3]

gd1_desc1 :: [Sentence]
gd1_desc1 = [S "Newton's second law of motion" +:+ (makeRef $ reldefn newtonSL) 
  +:+ S "states"]

gd1_desc2 :: [Sentence]
gd1_desc2 = [S "Rearranging"]

gd1_desc3 :: [Sentence]
gd1_desc3 = [S "Integrating the right hand side"]

impulseDeriv_eqns :: [Expr]
impulseDeriv_eqns = [gd1_eq1, gd1_eq2, gd1_eq3]

gd1_eq1 :: Expr
gd1_eq1 = sy force $= (sy mass)*(sy acceleration) $= (sy acceleration)*(deriv (sy velocity) time)

gd1_eq2 :: Expr
gd1_eq2 = int_all (eqSymb time) (sy force) $= (sy mass)*(int_all (eqSymb time) (sy force))

gd1_eq3 :: Expr
gd1_eq3 = int_all (eqSymb time) (sy force) $= (sy mass)*(sy vel_2) - (sy mass)*(sy vel_1)
  $= (sy mass)*(sy deltaV)

conservationOfMomentGDef :: RelationConcept
conservationOfMomentGDef = makeRC "conservOfMoment" (nounPhraseSP "Conservation of Momentum") 
  conservationOfMomentDesc conservationOfMomentRel

conservationOfMomentRel :: Relation
conservationOfMomentRel = (defsum (Atomic "k") (str "0") (str "n"))
 ((sy mass_k)*(sy vel_ik)) $= (defsum (Atomic "k") (str "0") (str "n"))
 ((sy mass_k)*(sy vel_fk))

conservationOfMomentDesc :: Sentence
conservationOfMomentDesc = foldlSent [S "In an isolated system," `sC`
  S "where the sum of external impulses acting on the system is zero" `sC`
  S "the total momentum of the bodies is constant (conserved)"
  ]
conservationOfMomentDeriv :: Derivation
conservationOfMomentDeriv = (weave [conservationOfMomentDeriv_sentences,
 map E conservationOfMomentDeriv_eqns])

conservationOfMomentDeriv_sentences :: [Sentence]
conservationOfMomentDeriv_sentences = map foldlSentCol [gd2_desc1, gd2_desc2, gd2_desc3,
  gd2_desc4, gd2_desc5, gd2_desc6, gd2_desc7]

gd2_desc1 :: [Sentence]
gd2_desc1 = [S "When bodies collide, they exert an equal (force) on each other in opposite directions.",
  S "This is Newton's third law" +:+ (makeRef $ reldefn newtonTL)]

gd2_desc2 :: [Sentence]
gd2_desc2 = [S "The objects collide with each other for the exact same amount of", phrase time, ch time]

gd2_desc3 :: [Sentence]
gd2_desc3 = [S "The above equation is equal to the impulse (GD1)"]

gd2_desc4 :: [Sentence]
gd2_desc4 = [S "The impulse is equal to the change in momentum"]

gd2_desc5 :: [Sentence]
gd2_desc5 = [S "Substituting 2 into 1 yields"]

gd2_desc6 :: [Sentence]
gd2_desc6 = [S "Expanding and rearranging the above formula gives"]

gd2_desc7 :: [Sentence]
gd2_desc7 = [S "Generalizing for multiple (k) colliding objects"]

conservationOfMomentDeriv_eqns :: [Expr]
conservationOfMomentDeriv_eqns = [gd2_eq1, gd2_eq2, gd2_eq3, gd2_eq4, gd2_eq5
  , gd2_eq6, gd2_eq7]

gd2_eq1 :: Expr
gd2_eq1 = sy force_1 $= negate (sy force_2)

gd2_eq2 :: Expr
gd2_eq2 = (sy force_1)*(sy mass) $= (negate (sy force_2))*(sy mass)

gd2_eq3 :: Expr
gd2_eq3 = (sy force_1)*(sy mass) $= int_all (eqSymb time) (sy force_1) $= sy impulseV

gd2_eq4 :: Expr
gd2_eq4 = sy impulseV $= sy deltaP $= (sy mass)*(sy deltaV)

gd2_eq5 :: Expr
gd2_eq5 = (sy mass)*(sy deltaV1) $= (negate (sy mass)*(sy deltaV2))

gd2_eq6 :: Expr
gd2_eq6 = (sy mass_1)*(sy vel_i1) + (sy mass_2)*(sy vel_i2) $= 
  (sy mass_1)*(sy vel_f1) + (sy mass_2)*(sy vel_f2)

gd2_eq7 :: Expr
gd2_eq7 = (defsum (Atomic "k") (str "0") (str "n"))
 ((sy mass_k)*(sy vel_ik)) $= (defsum (Atomic "k") (str "0") (str "n"))
 ((sy mass_k)*(sy vel_fk))

accelerationDueToGravityGDef :: RelationConcept
accelerationDueToGravityGDef = makeRC "accelDueToGrav" (nounPhraseSP "Acceleration due to gravity") 
  accelerationDueToGravityDesc accelerationDueToGravityRel

accelerationDueToGravityRel :: Relation
accelerationDueToGravityRel = sy force_g $= (sy mass)*(sy gravitationalAccel)

accelerationDueToGravityDesc :: Sentence
accelerationDueToGravityDesc = S ""

accelerationDueToGravityDeriv :: Derivation
accelerationDueToGravityDeriv = (weave [conservationOfMomentDeriv_sentences,
 map E conservationOfMomentDeriv_eqns])

accelerationDueToGravityDeriv_sentences :: [Sentence]
accelerationDueToGravityDeriv_sentences = map foldlSentCol [gd3_desc1, gd3_desc2, gd3_desc3,
  gd3_desc4, gd3_desc5, gd3_desc6, gd3_desc7]

gd3_desc1 :: [Sentence]
gd3_desc1 = [S "From Newton's law of universal gravitation (T3)" `sC` (makeRef $ reldefn newtonTL),
  S "we have:"]

gd3_desc2 :: [Sentence]
gd3_desc2 = [S "Equation 3 governs the gravitational attraction between two bodies." +:+ 
  S "Suppose that one of the bodies is significantly more massive than the other" `sC` 
  S "so that we concern ourselves with the force the massive body exerts on the lighter body." +:+
  S "Further suppose that the coordinate system is chosen such that this force" +:+
  S "acts on a line which lies along one of the principal axes" +:+. S "(A2)" +:+
  S "Then our unit vector for the x or y axes (A3), respectively."]

gd3_desc3 :: [Sentence]
gd3_desc3 = [S "Given the above assumptions" `sC` 
  S "let M and m be the mass of the massive and light body," +:+
  S "respectively." +:+ S "Using 3 and equating this with Newton's second law" +:+ 
  (makeRef $ reldefn newtonTL) +:+ S "for the force experienced by the light body" `sC`
  S "we get"]

gd3_desc4 :: [Sentence]
gd3_desc4 = [S "where g is gravitational acceleration." +:+ 
  S "Dividing 4 by m, and resolving this into separate x and y components:"]

gd3_desc5 :: [Sentence]
gd3_desc5 = [S "Thus"]

gd3_desc6 :: [Sentence]
gd3_desc6 = [S "Expanding and rearranging the above formula gives"]

gd3_desc7 :: [Sentence]
gd3_desc7 = [S "Generalizing for multiple (k) colliding objects"]

accelerationDueToGravityDeriv_eqns :: [Expr]
accelerationDueToGravityDeriv_eqns = [gd3_eq1, gd2_eq2, gd2_eq3, gd2_eq4, gd2_eq5
  , gd2_eq6, gd2_eq7]

gd3_eq1 :: Expr
gd3_eq1 = sy force_1 $= negate (sy force_2)

gd3_eq2 :: Expr
gd3_eq2 = (sy force_1)*(sy mass) $= (negate (sy force_2))*(sy mass)

gd3_eq3 :: Expr
gd3_eq3 = (sy force_1)*(sy mass) $= int_all (eqSymb time) (sy force_1) $= sy impulseV

gd3_eq4 :: Expr
gd3_eq4 = sy impulseV $= sy deltaP $= (sy mass)*(sy deltaV)

gd3_eq5 :: Expr
gd3_eq5 = (sy mass)*(sy deltaV1) $= (negate (sy mass)*(sy deltaV2))

gd3_eq6 :: Expr
gd3_eq6 = (sy mass_1)*(sy vel_i1) + (sy mass_2)*(sy vel_i2) $= 
  (sy mass_1)*(sy vel_f1) + (sy mass_2)*(sy vel_f2)

gd3_eq7 :: Expr
gd3_eq7 = (defsum (Atomic "k") (str "0") (str "n"))
 ((sy mass_k)*(sy vel_ik)) $= (defsum (Atomic "k") (str "0") (str "n"))
 ((sy mass_k)*(sy vel_fk))

{--relativeVelocityInCollisionsGDef :: RelationConcept
relativeVelocityInCollisionsGDef = makeRC "relVeloInColl"
  (nounPhraseSP "Relative velocity in collision")
  relativeVelocityInCollisionsDesc relativeVelocityInCollisionsRel

relativeVelocityInCollisionsDesc :: Sentence
relativeVelocityInCollisionsDesc = foldlSent [S "In a collision, the",
  (phrase velocity), S "of", S "rigid body A", 
  S "colliding with another body B relative to that",
  S "body, (symbol vAB), is the difference between the", (plural velocity), 
  S "of A", S "and B at point P"
  ]

--[velocityAB, collisionPoint, velocityAP, velocityBP]

relativeVelocityInCollisionsRel :: Relation
relativeVelocityInCollisionsRel = FCall (C thFluxVect) [C QP.time] := C htTransCoeff :*
  FCall (C temp_diff) [C QP.time] -- replace with proper Expr

coefficientOfRestitutionGDef :: RelationConcept
coefficientOfRestitutionGDef = makeRC "coeffOfRest" 
  (nounPhraseSP "Coefficient of restitution")
  coefficientOfRestitutionDesc coefficientOfRestitutionRel

coefficientOfRestitutionDesc :: Sentence
coefficientOfRestitutionDesc = foldlSent [S "The", (phrase restitutionCoef), 
  (getS restitutionCoef), S "is",
  S "a unitless, dimensionless quantity that determines the elasticity of a",
  S "collision between two bodies. (symbol/expr)[CR = 1] results in an elastic",
  S "collision, while (symbol/expr)[CR < 1] results in an inelastic collision,",
  S "and (symbol/expr)[CR = 0] results in a totally inelastic collision"
  ]

--[restitutionCoef, normCollisionVect, initRelativeVelocityAB, finalRelativeVelocityAB]

coefficientOfRestitutionRel :: Relation
coefficientOfRestitutionRel = FCall (C thFluxVect) [C QP.time] := C htTransCoeff :*
  FCall (C temp_diff) [C QP.time] -- replace with proper Expr


torqueGDef :: RelationConcept
torqueGDef = makeRC "torque"
  (nounPhraseSP "Torque")
  torqueDesc torqueRel

torqueDesc :: Sentence
torqueDesc = foldlSent [S "The", (phrase torque), (getS torque), 
  S "on a body measures the", S "the tendency of a", (phrase force), 
  S "to rotate the body around an axis or pivot"
  ]

--[torque, force, position]

torqueRel :: Relation
torqueRel = FCall (C thFluxVect) [C QP.time] := C htTransCoeff :*
  FCall (C temp_diff) [C QP.time] -- replace with proper Expr

momentOfInertiaGDef :: RelationConcept
momentOfInertiaGDef = makeRC "momentOfInertia"
  (nounPhraseSP "Moment of Inertia")
  momentOfInertiaDesc momentOfInertiaRel

momentOfInertiaDesc :: Sentence
momentOfInertiaDesc = foldlSent []


--[momentOfInertia, numOfParticles, mass_i, distanceBtwParticleI]

momentOfInertiaRel :: Relation
momentOfInertiaRel = FCall (C thFluxVect) [C QP.time] := C htTransCoeff :*
  FCall (C temp_diff) [C QP.time] -- replace with proper Expr--}
