module Drasil.SSP.DataCons where 

import Language.Drasil (LabelledContent)

import Drasil.DocLang (inDataConstTbl, outDataConstTbl)

import Drasil.SSP.Unitals (inputsWUncrtn, outputs)

{-input and output tables-}
data_constraint_Table2, data_constraint_Table3 :: LabelledContent
data_constraint_Table2 = inDataConstTbl inputsWUncrtn --FIXME: issue #295
data_constraint_Table3 = outDataConstTbl outputs