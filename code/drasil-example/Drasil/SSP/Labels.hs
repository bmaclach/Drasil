module Drasil.SSP.Labels where

import Language.Drasil
import qualified Data.Map as Map
import Drasil.DocLang.SRS as SRS (physSystLabel)

sspLabelSet :: [Label]
sspLabelSet = [slipSurfaceL, geoSlopeMatL, soilLayerHomoL, soilLayerIsoL,
    intersliceNormL, baseNormShearForL, stressStrainCurveL, planeStrainL,
    effectiveNormL, surfaceBaseSliceL, sliceWghtL, baseWtrFL, surfWtrFL,
    intersliceWtrFL, angleAL, angleBL, lengthBL,
    lengthLbL, lengthLsL, seismicLoadFL, surfLoadsL, intrsliceFL, resShearWOL,
    mobShearWOL, displcmntRxnFL, displcmntBaselL, netFDsplcmntEqbmL,
    shearStiffnessL, soilStiffnessL, genDef1Label, genDef2Label,
    genDef3Label, genDef4Label, genDef5Label, genDef6Label, 
    genDef7Label, genDef8Label, genDef9Label, genDef10Label,
    fctSftyL, nrmShrForL, inslideFxL, forDisEqlbL, rfemFoSL, crtSlpIdL,
    l1, l2, l3, l4, l5, physSystLabel]

sspLabelMap :: LabelMap
sspLabelMap = Map.fromList $ map getLabelPair sspLabelSet

-- Assumptions
slipSurfaceL, geoSlopeMatL, soilLayerHomoL, soilLayerIsoL,
    intersliceNormL, baseNormShearForL, stressStrainCurveL, planeStrainL,
    effectiveNormL, surfaceBaseSliceL :: Label
slipSurfaceL                = mkLabelRAAssump' "Slip-Surface-Concave"
geoSlopeMatL                = mkLabelRAAssump' "Geo-Slope-Mat-Props-of-Soil-Inputs"
soilLayerHomoL              = mkLabelRAAssump' "Soil-Layer-Homogeneous"
soilLayerIsoL               = mkLabelRAAssump' "Soil-Layers-Isotropic"
intersliceNormL             = mkLabelRAAssump' "Interslice-Norm-Shear-Forces-Linear"
baseNormShearForL           = mkLabelRAAssump' "Base-Norm-Shear-Forces-Linear-on-FS"
stressStrainCurveL          = mkLabelRAAssump' "Stress-Strain-Curve-interslice-Linear"
planeStrainL                = mkLabelRAAssump' "Plane-Strain-Conditions"
effectiveNormL              = mkLabelRAAssump' "Effective-Norm-Stress-Large"
surfaceBaseSliceL           = mkLabelRAAssump' "Surface-Base-Slice-between-Interslice-Straight-Lines"

-- Data Definition
sliceWghtL, baseWtrFL, surfWtrFL, intersliceWtrFL, angleAL, angleBL, lengthBL,
    lengthLbL, lengthLsL, seismicLoadFL, surfLoadsL, intrsliceFL, resShearWOL,
    mobShearWOL, displcmntRxnFL, displcmntBaselL, netFDsplcmntEqbmL,
    shearStiffnessL, soilStiffnessL :: Label
sliceWghtL        = mkLabelSame "W_i" (Def DD)
baseWtrFL         = mkLabelSame "U_b,i" (Def DD)
surfWtrFL         = mkLabelSame "U_t,i" (Def DD)
intersliceWtrFL   = mkLabelSame "H_i" (Def DD)
angleAL           = mkLabelSame "alpha_i" (Def DD)
angleBL           = mkLabelSame "beta_i" (Def DD)
lengthBL          = mkLabelSame "b_i" (Def DD)
lengthLbL         = mkLabelSame "l_b,i" (Def DD)
lengthLsL         = mkLabelSame "l_s,i" (Def DD)
seismicLoadFL     = mkLabelSame "K_c" (Def DD)
surfLoadsL        = mkLabelSame "Q_i" (Def DD)
intrsliceFL       = mkLabelSame "X_i" (Def DD)
resShearWOL       = mkLabelSame "R_i" (Def DD)
mobShearWOL       = mkLabelSame "T_i" (Def DD)
displcmntRxnFL    = mkLabelSame "pressure" (Def DD)
displcmntBaselL   = mkLabelSame "pressure" (Def DD)
netFDsplcmntEqbmL = mkLabelSame "netFDsplcmntEqbm" (Def DD)
shearStiffnessL   = mkLabelSame "K_bt,i" (Def DD)
soilStiffnessL    = mkLabelSame "K_bn,i" (Def DD)


-- General Definations
genDef1Label, genDef2Label, genDef3Label, genDef4Label, genDef5Label, genDef6Label, 
    genDef7Label, genDef8Label, genDef9Label, genDef10Label :: Label

genDef1Label  = mkLabelSame "normForcEq"  (Def General)
genDef2Label  = mkLabelSame "bsShrFEq"    (Def General)
genDef3Label  = mkLabelSame "resShr"      (Def General)
genDef4Label  = mkLabelSame "mobShr"      (Def General)
genDef5Label  = mkLabelSame "normShrR"    (Def General)
genDef6Label  = mkLabelSame "momentEql"   (Def General)
genDef7Label  = mkLabelSame "netForcex"   (Def General)
genDef8Label  = mkLabelSame "netForcey"   (Def General)
genDef9Label  = mkLabelSame "hookesLaw2d" (Def General)
genDef10Label = mkLabelSame "displVect"   (Def General)

-- Instance Models
fctSftyL, nrmShrForL, inslideFxL, forDisEqlbL, rfemFoSL, crtSlpIdL :: Label
fctSftyL = mkLabelSame "fctSfty"    (Def Instance)
nrmShrForL = mkLabelSame "nrmShrFor"  (Def Instance)
inslideFxL = mkLabelSame "intsliceFs"  (Def Instance)
forDisEqlbL = mkLabelSame "forDisEqlb" (Def Instance)
rfemFoSL = mkLabelSame "rfemFoS"    (Def Instance)
crtSlpIdL = mkLabelSame "crtSlpId"   (Def Instance)

-- Theory Models
l1, l2, l3, l4, l5 :: Label
l1 = mkLabelSame "factOfSafety" (Def TM)
l2 = mkLabelSame "equilibrium"  (Def TM)
l3 = mkLabelSame "mcShrStrgth"  (Def TM)
l4 = mkLabelSame "effStress"    (Def TM)
l5 = mkLabelSame "hookesLaw"    (Def TM)

