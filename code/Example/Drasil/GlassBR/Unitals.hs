module Drasil.GlassBR.Unitals where --whole file is used

import Language.Drasil
import Control.Lens ((^.))
import Prelude hiding (log, sqrt)

import Drasil.GlassBR.Units (sFlawPU)
import Drasil.GlassBR.Concepts (annealedGlass, aR, fullyTGlass, glassTypeFac,
  heatSGlass, loadDurFactor, iGlass, lGlass, lResistance, lShareFac, nFL, 
  stdOffDist, glaPlane, responseTy)

import Data.Drasil.SI_Units
import Data.Drasil.Constraints
import Data.Drasil.Utils (mkDataDef, getES)
import Data.Drasil.SentenceStructures (foldlSent, displayConstrntsAsSet,
  foldlsC, foldlOptions)

--FIXME: Many of the current terms can be separated into terms and defns?

{--}

glassBRSymbolsWithDefns :: [UnitalChunk]
glassBRSymbolsWithDefns = [mod_elas]

mod_elas :: UnitalChunk
mod_elas    = uc' "mod_elas"     (nounPhraseSP "modulus of elasticity of glass")
  "The ratio of tensile stress to tensile strain of glass." cE pascal

{--}

gbConstrained :: [ConstrWrapper]

gbConstrained = (map cnstrw gbInputsWUncrtn) ++ 
  (map cnstrw gbInputsWUnitsUncrtn) ++ [cnstrw prob_br]

plate_len, plate_width, char_weight, standOffDist :: UncertQ
pb_tol, tNT :: UncertainChunk
glass_type, nom_thick :: ConstrainedChunk

{--}

defaultUncrt :: Double
defaultUncrt = 0.1

gbInputs :: [QWrapper]
gbInputs = (map qs gbInputsWUnitsUncrtn) ++ (map qs gbInputsWUncrtn) ++ 
  (map qs gbInputsNoUncrtn) ++ map qs sdVector

--inputs with units and uncertainties
gbInputsWUnitsUncrtn :: [UncertQ]
gbInputsWUnitsUncrtn = [plate_len, plate_width, standOffDist, char_weight]

--inputs with uncertainties and no units
gbInputsWUncrtn :: [UncertainChunk]
gbInputsWUncrtn = [pb_tol, tNT]

--inputs with no uncertainties
gbInputsNoUncrtn :: [ConstrainedChunk]
gbInputsNoUncrtn = [glass_type, nom_thick]

gbInputDataConstraints :: [UncertainWrapper]
gbInputDataConstraints = (map uncrtnw gbInputsWUnitsUncrtn) ++ 
  (map uncrtnw gbInputsWUncrtn)
  

plate_len = uqcND "plate_len" (nounPhraseSP "plate length (long dimension)")
  lA metre Real 
  [ gtZeroConstr,
    physc $ \c -> (c / (C plate_width)) :> (Dbl 1),
    sfwrc $ \c -> (C dim_min) :<= c,
    sfwrc $ \c -> c :<= (C dim_max),
    sfwrc $ \c -> (c / (C plate_width)) :< (C ar_max) ] (Dbl 1.5) defaultUncrt

plate_width = uqcND "plate_width" (nounPhraseSP "plate width (short dimension)")
  lB metre Real
  [ gtZeroConstr,
    physc $ \c -> c :< (C plate_len),
    sfwrc $ \c -> (C dim_min) :<= c,
    sfwrc $ \c -> c :<= (C dim_max),
    sfwrc $ \c -> ((C plate_len) / c) :< (C ar_max) ] (Dbl 1.2) defaultUncrt

pb_tol = uvc "pb_tol" (nounPhraseSP "tolerable probability of breakage") 
  (sub cP (Atomic "btol")) Real
  [ gtZeroConstr,
    physc $ \c -> c :< (Dbl 1) ] (Dbl 0.008) (0.001)

char_weight = uqcND "char_weight" (nounPhraseSP "charge weight") 
  lW kilogram Real
  [ physc $ \c -> c :>= (Dbl 0),
    sfwrc $ \c -> (C cWeightMin) :<= c,
    sfwrc $ \c -> c :<= (C cWeightMax) ] (Dbl 42) defaultUncrt

tNT = uvc "tNT" (nounPhraseSP "TNT equivalent factor")
  (Atomic "TNT") Real
  [ gtZeroConstr ] (1) defaultUncrt

standOffDist = uqcND "standOffDist" (nounPhraseSP "stand off distance") 
  (Atomic "SD") metre Real
  [ gtZeroConstr,
    sfwrc $ \c -> (C sd_min) :< c,
    sfwrc $ \c -> c :< (C sd_max) ] (Dbl 45) defaultUncrt
--FIXME: ^ incorporate definition in here?

--FIXME: Issue #309
nom_thick = cuc "nom_thick" 
  (nounPhraseSent $ S "nominal thickness" +:+ displayConstrntsAsSet 
    nom_thick (map show nominalThicknesses))
  lT millimetre ({-DiscreteD nominalThicknesses-} Rational) 
  [ physc $ \c -> createCnstrnts c (map show nominalThicknesses) ] (V "8.0") --FIXME: no typical value!

glass_type  = cvc "glass_type" (nounPhraseSent $ phrase glassTy +:+ 
    displayConstrntsAsSet glass_type glassTypeAbbrsStr)
  lG ({-DiscreteS glassTypeAbbrsStr-} String)
  [ physc $ \c -> createCnstrnts c glassTypeAbbrsStr] (V "HS") --FIXME: no typical value!

{--}

gbOutputs :: [QWrapper]
gbOutputs = map qs [is_safe1, is_safe2] ++ map qs [prob_br]

prob_br :: ConstrainedChunk
prob_br = cvc "prob_br" (nounPhraseSP "probability of breakage")
  (sub cP lB) Rational
  [ physc $ \c -> (Dbl 0) :< c,
    physc $ \c -> c :< (Dbl 1) ] (Dbl 0.4)
  --FIXME: no typical value!

{--}

gBRSpecParamVals :: [QDefinition]
gBRSpecParamVals = [dim_max, dim_min, ar_max, cWeightMax, cWeightMin,
  sd_min, sd_max]

dim_max, dim_min, ar_max, cWeightMax, cWeightMin, sd_min,
  sd_max :: QDefinition

dim_max     = mkDataDef (unitary "dim_max"
  (nounPhraseSP "maximum value for one of the dimensions of the glass plate") 
  (sub lD (Atomic "max")) millimetre Real) (Dbl 5)

dim_min     = mkDataDef (unitary "dim_min"
  (nounPhraseSP "minimum value for one of the dimensions of the glass plate") 
  (sub lD (Atomic "min")) millimetre Real) (Dbl 0.1)

ar_max     = mkDataDef (vc "ar_max"
  (nounPhraseSP "maximum aspect ratio")
  (sub (Atomic "AR") (Atomic "max")) Rational) (Dbl 5)

cWeightMax = mkDataDef (unitary "cWeightMax" 
  (nounPhraseSP "maximum permissible input charge weight")
  (sub (eqSymb char_weight) (Atomic "max")) kilogram Rational) (Dbl 910)

cWeightMin = mkDataDef (unitary "cWeightMin"
  (nounPhraseSP "minimum permissible input charge weight")
  (sub (eqSymb char_weight) (Atomic "min")) kilogram Rational) (Dbl 4.5)

sd_min     = mkDataDef (unitary "sd_min"
  (nounPhraseSP "minimum stand off distance permissible for input") 
  (sub (eqSymb standOffDist) (Atomic "min")) metre Real) (Dbl 6)

sd_max     = mkDataDef (unitary "sd_max"
  (nounPhraseSP "maximum stand off distance permissible for input")
  (sub (eqSymb standOffDist) (Atomic "max")) metre Real) (Dbl 130)

{--}

glassBRSymbols :: [UnitaryChunk]
glassBRSymbols = [act_thick, sflawParamK, sflawParamM, demand, load_dur,
  eqTNTWeight]

act_thick, sflawParamK, sflawParamM, demand, sdx, sdy, sdz, load_dur,
  eqTNTWeight :: UnitaryChunk

act_thick   = unitary "act_thick"   (nounPhraseSP "actual thickness")
  lH metre Rational

demand      = unitary "demand"      (nounPhraseSP "applied load (demand)")
  lQ kilopascal Rational --correct Space used?

eqTNTWeight = unitary "eqTNTWeight" 
  (nounPhraseSP "explosive mass in equivalent weight of TNT")
  (sub (eqSymb char_weight) (eqSymb tNT)) kilogram Real

load_dur    = unitary "load_dur"    (nounPhraseSP "duration of load")
  (sub lT lD) second Real

sdx         = unitary "sdx" (nounPhraseSP "stand off distance (x-component)")
  (sub (eqSymb standOffDist) lX) metre Real

sdy         = unitary "sdy" (nounPhraseSP "stand off distance (y-component)")
  (sub (eqSymb standOffDist) lY) metre Real

sdz         = unitary "sdz" (nounPhraseSP "stand off distance (z-component)")
  (sub (eqSymb standOffDist) lZ) metre Real

sflawParamK = unitary "sflawParamK" (nounPhraseSP "surface flaw parameter") --parameterize?
  lK sFlawPU Real

sflawParamM = unitary "sflawParamM" (nounPhraseSP "surface flaw parameter") --parameterize?
  lM sFlawPU Real

{-Quantities-}

glassBRUnitless :: [VarChunk]
glassBRUnitless = [risk_fun, is_safe1, is_safe2, stressDistFac, sdf_tol,
  dimlessLoad, tolLoad, lRe, loadSF, gTF, lDurFac, nonFactorL, aspectR]

aspectR, risk_fun, is_safe1, is_safe2, stressDistFac, sdf_tol,
  dimlessLoad, tolLoad, lRe, loadSF, gTF, lDurFac, nonFactorL :: VarChunk

aspectR       = makeVC "aspectR"     (aR ^. term) (Atomic "AR")

dimlessLoad   = makeVC "dimlessLoad" (nounPhraseSP "dimensionless load") 
  (hat lQ)

gTF           = vc "gTF"             (glassTypeFac ^. term) (Atomic "GTF") Integer

is_safe1      = vc "is_safe1"        (nounPhraseSP $ "true when calculated" ++
  " probability is less than tolerable probability")
  (Concat [Atomic "is", Special UScore, Atomic "safe1"]) Boolean

is_safe2      = vc "is_safe2"        (nounPhraseSP $ "true when load resistance"
  ++ " (capacity) is greater than load (demand)")
  (Concat [Atomic "is", Special UScore, Atomic "safe2"]) Boolean

lDurFac       = vc'' (loadDurFactor) (Atomic "LDF") Real

loadSF        = vc'' (lShareFac) (Atomic "LSF") Natural

lRe           = vc'' (lResistance) (Atomic "LR") Real

nonFactorL    = vc'' (nonFactoredL) (Atomic "NFL") Real

risk_fun      = makeVC "risk_fun"    (nounPhraseSP "risk of failure") cB

sdf_tol       = makeVC "sdf_tol"     (nounPhraseSP $ "stress distribution" ++
  " factor (Function) based on Pbtol") 
  (sub (eqSymb stressDistFac) (Atomic "tol"))

stressDistFac = makeVC "stressDistFac" (nounPhraseSP $ "stress distribution" 
  ++ " factor (Function)") cJ

tolLoad       = makeVC "tolLoad"       (nounPhraseSP "tolerable load")
  (sub (eqSymb dimlessLoad) (Atomic "tol"))


terms :: [ConceptChunk]
terms = [aspectRatio, glBreakage, lite, glassTy,
  annealedGl, fTemperedGl, hStrengthGl, glTyFac, lateral, load, specDeLoad,
  loadResis, longDurLoad, nonFactoredL, glassWL, shortDurLoad, loadShareFac,
  probBreak, specA, blastResisGla, eqTNTChar, sD]

aspectRatio, glBreakage, lite, glassTy, annealedGl, fTemperedGl, hStrengthGl,
  glTyFac, lateral, load, specDeLoad, loadResis, longDurLoad, nonFactoredL,
  glassWL, shortDurLoad, loadShareFac, probBreak, specA, blastResisGla, eqTNTChar,
  sD, blast, blastTy, glassGeo, capacity, demandq, safeMessage, notSafe, bomb,
  explosion :: ConceptChunk

annealedGl    = cc annealedGlass
  ("A flat, monolithic, glass lite which has uniform thickness where the " ++
    "residual surface stresses are almost zero, as defined in [5]." {-astm_C1036-})
aspectRatio   = cc aR
  ("The ratio of the long dimension of the glass to the short dimension of " ++
    "the glass. For glass supported on four sides, the aspect ratio is " ++
    "always equal to or greater than 1.0. For glass supported on three " ++
    "sides, the ratio of the length of one of the supported edges " ++
    "perpendicular to the free edge, to the length of the free edge, is " ++
    "equal to or greater than 0.5.")
blast         = dcc "blast"       (nounPhraseSP "blast")
  "any kind of man-made explosion"
blastResisGla = dcc "blastResisGla"    (nounPhraseSP "blast resistant glazing")
  ("Glazing that provides protection against air blast pressure generated " ++
    "by explosions.")
blastTy       = dcc "blastTy"     (nounPhraseSP "blast type")
  ("The blast type input includes parameters like weight of charge, TNT " ++
    "equivalent factor and stand off distance from the point of explosion.")
bomb          = dcc "bomb"        (nounPhraseSP "bomb") ("a container filled " ++
  "with a destructive substance designed to exlode on impact or via detonation")
capacity      = dcc "capacity"    (nounPhraseSP "capacity")
  "the load resistance calculated"
demandq       = dcc "demandq"     (nounPhraseSP "demand") 
  "3 second duration equivalent pressure"
eqTNTChar     = dcc "eqTNTChar"   (nounPhraseSP "equivalent TNT charge mass")
  ("Mass of TNT placed on the ground in a hemisphere that represents the " ++
    "design explosive threat.")
explosion     = dcc "explosion"   (nounPhraseSP "explosion") 
  "a destructive shattering of something"
fTemperedGl   = cc fullyTGlass
  ("A flat and monolithic, glass lite of uniform thickness that has been " ++
    "subjected to a special heat treatment process where the residual " ++
    "surface compression is not less than 69 MPa (10 000 psi) or the edge " ++
    "compression not less than 67 MPa (9700 psi), as defined in [6]." {-astm_C1048-})
glassGeo      = dccWDS "glassGeo"    (nounPhraseSP "glass geometry")
  (S "The glass geometry based inputs include the dimensions of the" +:+ 
    phrase glaPlane `sC` phrase glassTy `sC` S "and" +:+.  phrase responseTy)
glassTy       = dcc "glassTy"     (cn' "glass type") "type of glass"
glassWL       = dcc "glassWL"     (nounPhraseSP "glass weight load")
  ("The dead load component of the glass weight.")
glBreakage    = dcc "glBreakage"  (nounPhraseSP "glass breakage")
  ("The fracture or breakage of any lite or ply in monolithic, laminated, " ++
    "or insulating glass.")
glTyFac       = cc' glassTypeFac
  (foldlSent [S "A multiplying factor for adjusting the", (getAcc lResistance), 
  S "of different glass type, that is,", foldlOptions glassTypeAbbrs
  `sC` S "in monolithic glass" `sC` (getAcc lGlass), sParen (titleize lGlass) `sC`
   S "or", (getAcc iGlass), sParen (titleize iGlass), S "constructions"])
hStrengthGl   = cc heatSGlass
  ("A flat, monolithic, glass lite of uniform thickness that has been " ++
    "subjected to a special heat treatment process where the residual " ++
    "surface compression is not less than 24 MPa (3500psi) or greater " ++
    "than 52 MPa (7500 psi), as defined in [6]." {-astm_C1048-})
lateral       = dcc "lateral"     (nounPhraseSP "lateral") 
  "Perpendicular to the glass surface."
lite          = dcc "lite"        (cn' "lite")
  ("Pieces of glass that are cut, prepared, and used to create the window " ++
    "or door.")
load          = dcc "load"        (nounPhraseSP "load") 
  "A uniformly distributed lateral pressure."
loadResis     = cc lResistance
  ("The uniform lateral load that a glass construction can sustain based " ++
    "upon a given probability of breakage and load duration as defined in " ++
    "[4 (pg. 1, 53)], following A2 and A1 respectively." {-astm_LR2009-})
loadShareFac  = cc' lShareFac
  (foldlSent [S "A multiplying factor derived from the load sharing between the",
  S "double glazing, of equal or different thickness's and types (including the",
  S "layered behaviour of", (getAcc lGlass), S "under long duration",
  S "loads), in a sealed", (getAcc iGlass), S "unit"])
longDurLoad   = dcc "longDurLoad"        (nounPhraseSP "long duration load")
  ("Any load lasting approximately 30 days.")
nonFactoredL  = cc' nFL
  (foldlSent [S "Three second duration uniform load associated with a", 
    S "probability of breakage less than or equal to 8", (plural lite),
    S "per 1000 for monolithic", (getAcc annealedGlass), S "glass"])
notSafe       = dcc "notSafe"     (nounPhraseSP "not safe")
  ("For the given input parameters, the glass is NOT considered safe.")
probBreak     = cc prob_br
  ("The fraction of glass lites or plies that would break at the first " ++
    "occurrence of a specified load and duration, typically expressed " ++
    "in lites per 1000.")
safeMessage   = dcc "safeMessage" (nounPhraseSP "safe")
  ("For the given input parameters, the glass is considered safe.")
sD            = cc' stdOffDist
  (S "The distance from the glazing surface to the centroid of a hemispherical"
   +:+ S "high explosive charge. It is represented by the coordinates" +:+.
   sParen (sdVectorSent))
shortDurLoad  = dcc "shortDurLoad"       (nounPhraseSP "short duration load")
  "Any load lasting 3s or less."
specA         = dcc "specA"       (nounPhraseSP "specifying authority")
  ("The design professional responsible for interpreting applicable " ++
    "regulations of authorities having jurisdiction and considering " ++
    "appropriate site specific factors to determine the appropriate " ++
    "values used to calculate the specified design load, and furnishing" ++
    " other information required to perform this practice.")
specDeLoad    = dcc "specDeLoad"  (nounPhraseSP "specified design load")
  ("The magnitude in kPa (psf), type (for example, wind or snow) and " ++
    "duration of the load given by the specifying authority.")

{--}


{--}

--Constants--

gbConstants :: [QDefinition]
gbConstants = [constant_M, constant_K, constant_ModElas, constant_LoadDur, constant_LoadDF, constant_LoadSF]
                ++ gBRSpecParamVals 

constant_M, constant_K, constant_ModElas, constant_LoadDur, constant_LoadDF, constant_LoadSF :: QDefinition
constant_K       = mkDataDef sflawParamK  $ (Grouping (Dbl 2.86)) * (10 :^ (Neg 53))
constant_M       = mkDataDef sflawParamM  $ 7
constant_ModElas = mkDataDef mod_elas     $ (Grouping (Dbl 7.17)) * (10 :^ 7)
constant_LoadDur = mkDataDef load_dur     $ 3
constant_LoadDF  = mkDataDef lDurFac      $ (Grouping ((C load_dur) / (60))) :^ ((C sflawParamM) / (16))
constant_LoadSF  = mkDataDef loadSF       $ 1
--Equations--

sdWithEqn :: QDefinition
sdWithEqn = mkDataDef standOffDist sdCalculation

sdCalculation :: Relation
sdCalculation = euclidean (map C sdVector)

sdVectorSent :: Sentence
sdVectorSent = foldlsC (map (getES) sdVector)

sdVector :: [UnitaryChunk]
sdVector = [sdx, sdy, sdz]

--

wtntWithEqn :: QDefinition
wtntWithEqn = mkDataDef eqTNTWeight wtntCalculation

wtntCalculation :: Relation
--wtntCalculation = (C eqTNTWeight) := (C char_weight) * (C tNT)
wtntCalculation = (C char_weight) * (C tNT)
--

aspectRWithEqn :: QDefinition
aspectRWithEqn = mkDataDef aspectR aspectRCalculation

aspectRCalculation :: Relation
aspectRCalculation = (C aspectR) := (C plate_len)/(C plate_width)

--
--Pulled to be used in "Terms And Definitions" Section--
termsWithDefsOnly, termsWithAccDefn, loadTypes, glassTypes :: [ConceptChunk]

glassTypes = [annealedGl, fTemperedGl, hStrengthGl]
termsWithDefsOnly = [glBreakage, lateral, lite, specA, blastResisGla,
  eqTNTChar]
termsWithAccDefn  = [sD, loadShareFac, glTyFac, aspectRatio]
loadTypes = [loadResis, nonFactoredL, glassWL, shortDurLoad,
  specDeLoad, longDurLoad] 

--Defined for DataDefs.hs and this file only--
actualThicknesses :: [Double]
actualThicknesses = map snd glassThickness

nominalThicknesses :: [Double]
nominalThicknesses = map fst glassThickness 

glassTypeFactors :: [Integer]
glassTypeFactors = map fst glassType

glassTypeAbbrs :: [Sentence]
glassTypeAbbrs = map S glassTypeAbbrsStr

glassTypeAbbrsStr :: [String]
glassTypeAbbrsStr = map snd glassType 

type GlassType = [(Integer, String)] -- [(Factor, Abbr)]
type GlassThickness = [(Double, Double)] --[(Nominal, Actual)]

glassType :: GlassType
glassType = [(1, "AN"), (4, "FT"), (2, "HS")]

glassThickness :: GlassThickness
glassThickness =   
  [(2.5, 2.16),
  (2.7, 2.59),
  (3.0, 2.92),
  (4.0, 3.78),
  (5.0, 4.57),
  (6.0, 5.56),
  (8.0, 7.42),
  (10.0, 9.02),
  (12.0, 11.91),
  (16.0, 15.09),
  (19.0, 18.26),
  (22.0, 21.44)]

--Below are present in this file temporarily--
lateralLoad :: NamedChunk
lateralLoad  = compoundNC lateral load