module Main (main) where

-- import Language.Drasil.Code (Choices(..), Comments(..), ConstraintBehaviour(..), 
--   ImplementationType(..), Lang(..), Logging(..), Structure(..))
import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocType(SRS, Website), DocSpec(DocSpec))

import Drasil.SSP.Body (srs, printSetting)

{- for when we can generate code again, comment this out
sspChoices :: Choices
sspChoices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,         -- LogNone, LogFunc
  comments = CommentNone,    -- CommentNone, CommentFunc
  onSfwrConstraint = Warning,  -- Warning, Exception
  onPhysConstraint = Warning,  -- Warning, Exception
  inputStructure = Loose    -- Loose, AsClass
} -} 
       
main :: IO ()            
main = do
  gen (DocSpec Website "SSP_SRS") srs printSetting
  gen (DocSpec SRS "SSP_SRS")     srs printSetting
  --genCode ssp_code
