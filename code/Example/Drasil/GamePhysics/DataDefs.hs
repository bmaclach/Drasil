module Drasil.GamePhysics.DataDefs where

import Drasil.GamePhysics.Unitals

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Concepts.Physics (rigidBody, collision)
import Data.Drasil.Quantities.Physics (restitutionCoef)
import Prelude hiding (id)
import Control.Lens ((^.))

----- Data Definitions -----

cpDDefs :: [QDefinition]
cpDDefs = [dd1CtrOfMass, dd2linDisp, dd3linVel, dd4linAcc, dd5angDisp,
  dd6angVel, dd7angAccel, dd8impulse]

-- DD1 : Centre of mass --

dd1CtrOfMass :: QDefinition
dd1CtrOfMass = fromEqn "dd1CtrOfMass" (pos_CM ^. term) (pos_CM ^. symbol)
  metre ctrOfMassEqn

ctrOfMassEqn :: Expr
ctrOfMassEqn = (UnaryOp (Summation Nothing
  ((C mass_i) * (C pos_i)))) / (C mTot)

-- DD2 : Linear displacement --

dd2linDisp :: QDefinition
dd2linDisp = fromEqn "dd2linDisp" dd2descr 
  (Concat [(disp ^. symbol), Atomic "(",(time ^. symbol), Atomic ")"]) 
  metre dispEqn

dispEqn :: Expr
dispEqn = Deriv Total (FCall (C position) [C time]) (C time)

dd2descr :: Sentence
dd2descr = S "linear" +:+ (disp ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (time ^. term) +:+ 
  P (time ^. symbol) +:+ sParen (Sy (time ^. unit)) `sC`
  S "also equal to the derivative of its linear" +:+ (position ^. term) +:+ 
  S "with respect to" +:+ (time ^. term) +:+ P (time ^. symbol)

-- DD3 : Linear velocity --

dd3linVel :: QDefinition
dd3linVel = fromEqn "dd3linVel" dd3descr 
  (Concat [(vel ^. symbol), Atomic "(", (time ^. symbol), Atomic ")"])
  velU velEqn

velEqn :: Expr
velEqn = Deriv Total (FCall (C disp) [C time]) (C time)

dd3descr :: Sentence
dd3descr = S "linear" +:+ (vel ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (time ^. term) +:+ 
  P (time ^. symbol) +:+ sParen (Sy (time ^. unit)) `sC`
  S "also equal to the derivative of its linear" +:+ (vel ^. term) +:+
  S "with respect to" +:+ (time ^. term) +:+ P (time ^. symbol)

-- DD4 : Linear acceleration --

dd4linAcc :: QDefinition
dd4linAcc = fromEqn "dd4linAcc" dd4descr 
  (Concat [(accel ^. symbol), Atomic "(", (time ^. symbol), Atomic ")"]) 
  accelU accelEqn

accelEqn :: Expr
accelEqn = Deriv Total (FCall (C vel) [C time]) (C time)

dd4descr :: Sentence
dd4descr = S "linear" +:+ (accel ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (time ^. term) +:+ 
  P (time ^. symbol) +:+ sParen (Sy (time ^. unit)) `sC`
  S "also equal to the derivative of its linear" +:+ (accel ^. term) +:+
  S "with respect to" +:+ (time ^. term) +:+ P (time ^. symbol)

-- DD5 : Angular displacement --

dd5angDisp :: QDefinition
dd5angDisp = fromEqn "dd5angDisp" dd5descr 
  (Concat [(angDisp ^. symbol), Atomic "(", (time ^. symbol), Atomic ")"])
  radians angDispEqn

angDispEqn :: Expr
angDispEqn = Deriv Total (FCall (C orientation) [C time]) (C time)

dd5descr :: Sentence
dd5descr = (angDisp ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (time ^. term) +:+ 
  P (time ^. symbol) +:+ sParen (Sy (time ^. unit)) `sC`
  S "also equal to the derivative of its" +:+ (orientation ^. term) +:+
  S "with respect to" +:+ (time ^. term) +:+ P (time ^. symbol)

-- DD6 : Angular velocity --

dd6angVel :: QDefinition
dd6angVel = fromEqn "dd6angVel" dd6descr 
  (Concat [(angVel ^. symbol), Atomic "(", (time ^. symbol), Atomic ")"]) 
  angVelU angVelEqn

angVelEqn :: Expr
angVelEqn = Deriv Total (FCall (C angDisp) [C time]) (C time)

dd6descr :: Sentence
dd6descr = ((angVel ^. term)) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (time ^. term) +:+ 
  P (time ^. symbol) +:+ sParen (Sy (time ^. unit)) `sC`
  S "also equal to the derivative of its" +:+ (angDisp ^. term) +:+
  S "with respect to" +:+ (time ^. term) +:+ P (time ^. symbol)

-- DD7 : Angular acceleration --

dd7angAccel :: QDefinition
dd7angAccel = fromEqn "dd7angAccel" dd7descr 
  (Concat [(angAccel ^. symbol), Atomic "(", (time ^. symbol), Atomic ")"])
  angAccelU angAccelEqn

angAccelEqn :: Expr
angAccelEqn = Deriv Total (FCall (C angVel) [C time]) (C time)

dd7descr :: Sentence
dd7descr = (angAccel ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (time ^. term) +:+
  P (time ^. symbol) +:+ sParen (Sy (time ^. unit)) `sC`
  S "also equal to the derivative of its" +:+ ((angVel ^. term)) +:+
  S "with respect to" +:+ (time ^. term) +:+ P (time ^. symbol)

-- DD8 : Impulse for collision response --

-- Currently a super crude implementation requiring lots of custom chunks;
-- need norms and cross products

dd8impulse :: QDefinition
dd8impulse = fromEqn "dd8impulse" dd8descr lJ impulseU impulseEqn

-- The last two terms in the denominator should be cross products.
impulseEqn :: Expr
impulseEqn = ((Neg ((Int 1) + (C restitutionCoef))) * (C initRelVel) :.
  (C normalVect)) / (((((Int 1) / (C mass_A))) + ((Int 1) / (C mass_B))) *
  ((C normalLen) :^ (Int 2)) +
  (((C perpLen_A) :^ (Int 2)) / (C momtInert_A)) +
  (((C perpLen_B) :^ (Int 2))/ (C momtInert_B)))

--NOTE: Removed an extra "the" that was showing up in the output.
dd8descr :: Sentence
dd8descr = (impulseScl ^. term) +:+ S "used to determine" +:+
  (collision ^. term) +:+ S "response between two" +:+ 
  irregPlur (rigidBody ^. term)
