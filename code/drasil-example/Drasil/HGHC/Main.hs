module Main (main) where

-- import Language.Drasil.Code (Choices(..), Comments(..), ConstraintBehaviour(..), 
--   ImplementationType(..), Lang(..), Logging(..), Structure(..))
import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocType(SRS, Website), DocSpec(DocSpec))

import Drasil.HGHC.HGHC (srsBody, printSetting)

{- When we want to actually generate code from this again, uncomment
thisChoices :: Choices
thisChoices = Choices {
  lang             = [Python, Cpp, CSharp, Java],
  impType          = Program,
  logFile          = "log.txt",
  logging          = LogNone,
  comments         = CommentNone, 
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure   = AsClass
} -}
  
main :: IO ()            
main = do
  gen (DocSpec Website "Tiny_SRS") srsBody printSetting
  gen (DocSpec SRS "Tiny_SRS")     srsBody printSetting
  --genCode thisChoices thisCode
