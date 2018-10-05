module Drasil.GlassBR.Labels where

import Language.Drasil
import qualified Data.Map as Map


glassLabelSet :: [Label]
glassLabelSet = [glassTypeL, glassConditionL, glassLiteL, 
  probOfBreakL, calOfCapacityL, calOfDemandL, l1, l2,
  rbrtsnL, astm2009L, astm2016L, astm2012L, beason1998L,
  riskL, hFromtL, loadDFL, strDisFacL, nonFLL, glaTyFacL, 
  dimLLL, tolPreL, tolStrDisFacL, standOffDisL, aspRatL]

-- Assumptions
glassTypeL, glassConditionL, glassLiteL :: Label

glassTypeL      = mkLabelRAAssump' "glassType"
glassConditionL = mkLabelRAAssump' "glassCondition"
glassLiteL      = mkLabelRAAssump' "glassLite"

-- Instance Models
probOfBreakL, calOfCapacityL, calOfDemandL :: Label

probOfBreakL   = mkLabelSame "probOfBreak"   (Def Instance)
calOfCapacityL = mkLabelSame "calofCapacity" (Def Instance)
calOfDemandL   = mkLabelSame "calofDemand"   (Def Instance)

-- Data Definition
riskL, hFromtL, loadDFL, strDisFacL, nonFLL, glaTyFacL, 
  dimLLL, tolPreL, tolStrDisFacL, standOffDisL, aspRatL :: Label
riskL       = mkLabelSame "risk_fun" (Def DD)
hFromtL     = mkLabelSame "min_thick" (Def DD)
loadDFL     = mkLabelSame "loadDurFactor" (Def DD)
strDisFacL  = mkLabelSame "stressDistFac" (Def DD)
nonFLL      = mkLabelSame "nFL" (Def DD)
glaTyFacL   = mkLabelSame "gTF" (Def DD) 
dimLLL      = mkLabelSame "dimlessLoad" (Def DD)
tolPreL     = mkLabelSame "tolLoad" (Def DD)
tolStrDisFacL = mkLabelSame "sdf_tol" (Def DD)
standOffDisL = mkLabelSame "standOffDist" (Def DD)
aspRatL     = mkLabelSame "aspect_ratio" (Def DD)

-- Cite
rbrtsnL, astm2009L, astm2016L, astm2012L, beason1998L :: Label
rbrtsnL     = mkLabelRA' "rbrtsn2012" "rbrtsn2012" Cite
astm2009L   = mkLabelRA' "astm2009" "astm2009" Cite
astm2016L   = mkLabelRA' "astm2016" "astm2016" Cite
astm2012L   = mkLabelRA' "astm2012" "astm2012" Cite
beason1998L = mkLabelRA' "beasonEtAl1998" "beasonEtAl1998" Cite

l1, l2 :: Label
l1 = mkLabelSame "safetyReqLR" (Def TM)
l2 = mkLabelSame "safetyReqPb" (Def TM)

--LabelMap = Map.Map UID Label
glassLabelMap :: LabelMap
glassLabelMap = Map.fromList $ map getLabelPair glassLabelSet