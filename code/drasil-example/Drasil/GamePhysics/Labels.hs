module Drasil.GamePhysics.Labels where

import Language.Drasil
import qualified Data.Map as Map

gameLabelSet :: [Label]
gameLabelSet = [newtonSLL, newtonTLL, newtonLUGL, chaslesThmL,  newtonSLRL,
  transMotL, rotMotL, col2DL]

gameLabelMap :: LabelMap
gameLabelMap = Map.fromList $ map getLabelPair gameLabelSet

-- Theory Models
newtonSLL, newtonTLL, newtonLUGL, chaslesThmL,  newtonSLRL:: Label
newtonSLL = mkLabelRA' "newtonSL" "NewtonSecLawMot" (Def TM)
newtonTLL = mkLabelRA' "newtonTL" "NewtonThirdLawMot" (Def TM)
newtonLUGL = mkLabelRA' "newtonLUG" "UniversalGravLaw" (Def TM)
chaslesThmL = mkLabelRA' "chaslesThm" "ChaslesTheorem" (Def TM)
newtonSLRL = mkLabelRA' "newtonSLR" "NewtonSecLawRotMot" (Def TM)

-- Instance Models
transMotL, rotMotL, col2DL :: Label
transMotL = mkLabelSame "transMot" (Def Instance)
rotMotL   = mkLabelSame "rotMot" (Def Instance)
col2DL    = mkLabelSame "col2D" (Def Instance)
