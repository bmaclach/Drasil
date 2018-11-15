{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.InstanceModel
  ( InstanceModel
  , im, im', im'', im'''
  , inCons, outCons, imOutput, imInputs -- FIXME, these should be done via lenses
  , Constraints
  ) where

import Data.Drasil.IdeaDicts (softEng)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Chunk.Quantity (QuantityDict)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA), Quantity,
  Definition(defn),ConceptDomain(cdom), Concept, ExprRelat(relat),
  HasDerivation(derivations), HasReference(getReferences), HasAdditionalNotes(getNotes),
  HasLabel(getLabel), HasSymbol(symbol), HasSpace(typ), HasShortName(shortname))
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.Development.Unit (MayHaveUnit(getUnit))
import Language.Drasil.Expr (Relation)
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Label (mkLabelSame)
import Language.Drasil.RefTypes (RefType(..), DType(..), Reference)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict)
import Language.Drasil.Chunk.NamedIdea (IdeaDict, mkIdea)
import Language.Drasil.NounPhrase (cn')

import Control.Lens (makeLenses, view)

type Inputs = [QuantityDict]
type Output = QuantityDict

-- All constraints in an InstanceModel are always 'Assumed' !
type Constraints = [Relation]

type OutputConstraints = Constraints
type InputConstraints  = Constraints

-- | An Instance Model is a RelationConcept that may have specific input/output
-- constraints. It also has attributes like derivation, source, etc.
data InstanceModel = IM { _rc :: RelationConcept
                        , _imInputs :: Inputs
                        , _inCons :: InputConstraints
                        , _imOutput :: Output
                        , _outCons :: OutputConstraints
                        , _ref :: [Reference]
                        , _deri :: Derivation
                        , _lb :: Label
                        , _notes :: [Sentence]
                        , ci :: CI
                        }
makeLenses ''InstanceModel

instance HasUID             InstanceModel where uid = rc . uid
instance NamedIdea          InstanceModel where term = rc . term
instance Idea               InstanceModel where getA = getA . view rc
instance Concept            InstanceModel where
instance Definition         InstanceModel where defn = rc . defn
instance ConceptDomain      InstanceModel where cdom = rc . cdom
instance ExprRelat          InstanceModel where relat = rc . relat
instance HasDerivation      InstanceModel where derivations = deri
instance HasReference       InstanceModel where getReferences = ref
instance HasLabel           InstanceModel where getLabel = lb
instance HasShortName       InstanceModel where shortname = lb . shortname
instance HasAdditionalNotes InstanceModel where getNotes = notes
instance HasSymbol          InstanceModel where symbol = symbol . view imOutput -- ???
instance HasSpace           InstanceModel where typ = imOutput . typ
instance Quantity           InstanceModel where
instance MayHaveUnit        InstanceModel where getUnit = getUnit . view imOutput

instanceMod :: CI
instanceMod    = commonIdeaWithDict "instanceMod"    (cn' "Instance Model")                    "IM"        [softEng]

-- | Smart constructor for instance models; no derivations or notes
im :: RelationConcept -> Inputs -> InputConstraints -> Output ->
  OutputConstraints -> [Reference] -> Label -> InstanceModel
im rcon i ic o oc src sn = IM rcon i ic o oc src [] sn [] instanceMod

-- | Same as `im`, with an additional field for notes to be passed in; no derivation
im' :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> [Reference] -> Label -> [Sentence] -> InstanceModel
im' rcon i ic o oc src lbe addNotes = IM rcon i ic o oc src [] lbe addNotes instanceMod

-- | im but with everything defined
im'' :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> [Reference] -> Derivation -> String -> [Sentence] -> InstanceModel
im'' rcon i ic o oc src der sn addNotes = IM rcon i ic o oc src der (mkLabelSame sn (Def Instance))
 addNotes instanceMod

-- | im with no notes
im''' :: RelationConcept -> Inputs -> InputConstraints -> Output ->
  OutputConstraints -> [Reference] -> Derivation -> String -> InstanceModel
im''' rcon i ic o oc src der sn = IM rcon i ic o oc src der 
  (mkLabelSame sn (Def Instance)) [] instanceMod
