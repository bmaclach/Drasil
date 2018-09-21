module Drasil.GlassBR.Labels where

import Language.Drasil
import qualified Data.Map as Map


glassLabelSet :: [Label]
glassLabelSet = [glassTypeL, glassConditionL, glassLiteL, 
  probOfBreakL, calOfCapacityL, calOfDemandL]

-- Assumptions
glassTypeL, glassConditionL, glassLiteL :: Label

glassTypeL      = mkLabelRAAssump' "glassType"
glassConditionL = mkLabelRAAssump' "glassCondition"
glassLiteL      = mkLabelRAAssump' "glassLite"

-- Instance Models
probOfBreakL, calOfCapacityL, calOfDemandL :: Label

probOfBreakL   = mkLabelSame "probOfBreak"   (Def Instance)
calOfCapacityL = mkLabelSame "calofCapacity" (Def Instance)
calOfDemandL   = mkLabelSame "calOfDemand"   (Def Instance)

--LabelMap = Map.Map UID Label
glassLabelMap :: LabelMap
glassLabelMap = Map.fromList $ map getLabelPair glassLabelSet