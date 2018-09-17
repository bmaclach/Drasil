{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
module Language.Drasil.Label.Core where

import Control.Lens (makeLenses, Lens')
import Language.Drasil.UID (UID)
import Language.Drasil.Chunk.ShortName (ShortName, HasShortName(shortname))
import Language.Drasil.RefTypes (RefType)
import qualified Data.Map as Map

-- import reference address from Language.Drasil.References?
data LblType = RefAdd String | MetaLink String | URI String

type LabelMap = Map.Map UID Label

-- Used for referencing; has to be pure ASCII
data Label = Lbl
  { _uniqueID :: UID --internal, unique
  , _lblType  :: LblType
  , _sn       :: ShortName
  , rftype    :: RefType --FIXME: HACK; ONLY USED FOR DIRECTLY REFERENCING LABELS PROPERLY in Reference.hs (see #971)
  }
makeLenses ''Label

instance HasShortName Label where shortname = sn

-- Label Map --
class HasLabelTable s where
	labelTable :: Lens' s LabelMap

labelLookup :: UID -> LabelMap -> Label
labelLookup u l = getL $ Map.lookup u l
  where getL = maybe (error $ "Label: " ++ u ++ " not found in LabelMap") id

getAdd :: LblType -> String
getAdd (RefAdd s)   = s
getAdd (MetaLink s) = s
getAdd (URI s)      = s
