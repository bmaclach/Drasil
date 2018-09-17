{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
module Language.Drasil.Label.Core where

import Control.Lens (makeLenses, Lens')

import Language.Drasil.Classes (HasUID(uid))
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

instance HasUID       Label where uid       = uniqueID
instance HasShortName Label where shortname = sn
instance HasRefAddress Label where getRefAdd = lblType

-- Label Map --
class HasLabelTable s where
	labelTable :: Lens' s LabelMap

-- | For those things which "have a label"
class HasLabel c where
  getLabel      :: Lens' c Label
 
class MayHaveLabel c where
  getMaybeLabel :: c -> Maybe Label

 -- IsLabel is associated with String rendering
class (HasLabel u, HasUID u) => IsLabel u where

-- HasRefAddress is associated with the HasLabel class due to
-- the current definition of a Label
class HasRefAddress b where
  getRefAdd :: Lens' b LblType

labelLookup :: UID -> LabelMap -> Label
labelLookup u l = getL $ Map.lookup u l
  where getL = maybe (error $ "Label: " ++ u ++ " not found in LabelMap") id

getAdd :: LblType -> String
getAdd (RefAdd s)   = s
getAdd (MetaLink s) = s
getAdd (URI s)      = s
