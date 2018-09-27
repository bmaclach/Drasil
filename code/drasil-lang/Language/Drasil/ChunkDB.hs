{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.ChunkDB 
  ( ChunkDB, cdb
  , HasSymbolTable(..), symbolMap, symbLookup, getUnitLup
  , HasTermTable(..), termLookup
  , HasDefinitionTable(..), conceptMap, defLookup
  , HasUnitTable(..), unitMap, collectUnits, TraceMap,
  traceLookup, HasTraceTable(..), generateRefbyMap, RefbyMap,
  refbyLookup, HasRefbyTable(..)
  ) where

import Control.Lens ((^.), Lens', makeLenses)
import Data.Maybe (maybeToList)
import Data.List (nub, concat, union, elem, length, zip)
import Data.Tuple (fst, snd)
import Language.Drasil.UID (UID)
import Language.Drasil.Classes (Concept, ConceptDomain, HasUID(uid), Idea, 
    IsUnit, HasDerivation(derivations)
  , HasAdditionalNotes(getNotes))
import Language.Drasil.Chunk.NamedIdea (IdeaDict, nw)
import Language.Drasil.Chunk.Quantity (Quantity, QuantityDict, qw)
import Language.Drasil.Chunk.Concept (ConceptChunk, cw)
import Language.Drasil.Development.Unit(UnitDefn, MayHaveUnit(getUnit), unitWrapper)
import qualified Data.Map as Map
import Language.Drasil.Label.Core (Label, LabelMap, labelLookup)
import Language.Drasil.Sentence.Extract(lnames)

-- The misnomers below are not actually a bad thing, we want to ensure data can't
-- be added to a map if it's not coming from a chunk, and there's no point confusing
-- what the map is for. One is for symbols + their units, and the others are for
-- what they state.

-- | A bit of a misnomer as it's really a map of all quantities, for retrieving
-- symbols and their units.
type SymbolMap  = Map.Map UID QuantityDict

-- | A map of all concepts, normally used for retrieving definitions.
type ConceptMap = Map.Map UID ConceptChunk

-- | A map of all the units used. Should be restricted to base units/synonyms.
type UnitMap = Map.Map UID UnitDefn

-- | Again a bit of a misnomer as it's really a map of all NamedIdeas.
-- Until these are built through automated means, there will
-- likely be some 'manual' duplication of terms as this map will contain all
-- quantities, concepts, etc.
type TermMap = Map.Map UID IdeaDict

type TraceMap = Map.Map UID [Label]

type RefbyMap = Map.Map UID [Label]

-- | Smart constructor for a 'SymbolMap'
symbolMap :: (Quantity c) => [c] -> SymbolMap
symbolMap = Map.fromList . map (\x -> (x ^. uid, qw x))

-- | Smart constructor for a 'TermMap'
termMap :: (Idea c) => [c] -> TermMap
termMap = Map.fromList . map (\x -> (x ^. uid, nw x))

-- | Smart constructor for a 'ConceptMap'
conceptMap :: (Concept c) => [c] -> ConceptMap
conceptMap = Map.fromList . map (\x -> (x ^. uid, cw x))

-- | Smart constructor for a 'UnitMap'
unitMap :: (IsUnit u, ConceptDomain u) => [u] -> UnitMap
unitMap = Map.fromList . map (\x -> (x ^. uid, unitWrapper x))

-- | Looks up an uid in the symbol table. If nothing is found, an error is thrown
symbLookup :: UID -> SymbolMap -> QuantityDict
symbLookup c m = getS $ Map.lookup c m
  where getS = maybe (error $ "Symbol: " ++ c ++ " not found in SymbolMap") id

--- SYMBOL TABLE ---
class HasSymbolTable s where
  symbolTable :: Lens' s SymbolMap

--- TERM TABLE ---
class HasTermTable s where
  termTable :: Lens' s TermMap
  
--- DEFINITION TABLE ---
class HasDefinitionTable s where
  defTable :: Lens' s ConceptMap

--- UNIT TABLE ---
class HasUnitTable s where
  unitTable :: Lens' s UnitMap

-- TRACE TABLE --
class HasTraceTable s where
  traceTable :: Lens' s TraceMap

-- Refby TABLE --
class HasRefbyTable s where
  refbyTable :: Lens' s RefbyMap

-- | Gets a unit if it exists, or Nothing.        
getUnitLup :: HasSymbolTable s => (HasUID c, MayHaveUnit c) => c -> s -> Maybe UnitDefn
getUnitLup c m = getUnit $ symbLookup (c ^. uid) (m ^. symbolTable)

-- | Looks up an uid in the term table. If nothing is found, an error is thrown
termLookup :: (HasUID c) => c -> TermMap -> IdeaDict
termLookup c m = getT $ Map.lookup (c ^. uid) m
  where getT = maybe (error $ "Term: " ++ (c ^. uid) ++ " not found in TermMap") id

-- | Looks up a uid in the definition table. If nothing is found, an error is thrown.
defLookup :: UID -> ConceptMap -> ConceptChunk
defLookup u m = getC $ Map.lookup u m
  where getC = maybe (error $ "Concept: " ++ u ++ " not found in ConceptMap") id

-- | Our chunk databases. Should contain all the maps we will need.
data ChunkDB = CDB { _csymbs :: SymbolMap
                   , _cterms :: TermMap 
                   , _cdefs  :: ConceptMap
                   , _cunitDB :: UnitMap
                   , _ctrace :: TraceMap
                   , _crefby :: RefbyMap
                   } --TODO: Expand and add more databases
makeLenses ''ChunkDB

-- | Smart constructor for chunk databases. Takes a list of Quantities 
-- (for SymbolTable), NamedIdeas (for TermTable), Concepts (for DefinitionTable),
-- and Units (for UnitTable)
cdb :: (Quantity q, Idea t, Concept c, IsUnit u,
        ConceptDomain u) => [q] -> [t] -> [c] -> [u] -> TraceMap -> RefbyMap -> ChunkDB
cdb s t c u tc rfm = CDB (symbolMap s) (termMap t) (conceptMap c) (unitMap u) tc rfm

----------------------
instance HasSymbolTable     ChunkDB where symbolTable = csymbs
instance HasTermTable       ChunkDB where termTable   = cterms
instance HasDefinitionTable ChunkDB where defTable    = cdefs
instance HasUnitTable       ChunkDB where unitTable   = cunitDB
instance HasTraceTable      ChunkDB where traceTable  = ctrace
instance HasRefbyTable      ChunkDB where refbyTable  = crefby

collectUnits :: HasSymbolTable s => (HasUID c, Quantity c) => s -> [c] -> [UnitDefn]
collectUnits m symb = map unitWrapper $ concatMap maybeToList $ map (\x -> getUnitLup x m) symb

traceLookup :: UID -> TraceMap -> [Label]
traceLookup c m = getT $ Map.lookup c m
  where getT = maybe (error $ "References related to : " ++ c ++ " not found in TraceMap") id

{--generateIndex :: TraceMap -> RefbyMap
generateIndex tm = foldl Map.union Map.empty $ map (\x -> Map.singleton x [])
 (nub $ map (^. uid) $ concat $ Map.elems tm)--}

generateRefbyMap :: TraceMap -> LabelMap -> RefbyMap
generateRefbyMap tm lm = Map.fromList $ listgrow 0 (generateIndex tm lm) tm 0 lm

generateIndex :: TraceMap -> LabelMap -> [(UID, [Label])]
generateIndex tm lm = zip (nub $ map (^. uid) $ concat $ Map.elems tm) [[]]


-- initial counter number (-> size of list) -> complete tracemap -> new list
listgrow :: Int  -> [(UID, [Label])] -> TraceMap -> Int-> LabelMap -> [(UID, [Label])]
listgrow size1 old tm size2 lm = if size2 + 1 /= length (snd $ Map.elemAt size1 tm)
  then listgrow size1 (lookuppair old (fst $ Map.elemAt size1 tm) (((snd $ Map.elemAt size1 tm) !! size2) ^. uid) lm) tm (size2+1) lm
  else if size1 + 1 /= Map.size(tm)
    then listgrow (size1+1) old tm 0 lm
    else old

  -- old pair -> uid has to be added -> index uid  -> new pair
lookuppair :: [(UID, [Label])] -> UID -> UID -> LabelMap -> [(UID, [Label])]
lookuppair ((a, b):tl1) c d lm = if a == d 
  then (a, b ++ [labelLookup c lm]):tl1
  else (a, b):(lookuppair tl1 c d lm)
lookuppair [] c d lm = []

--elemAt :: Int -> Map k a -> (k, a).     to get k and a = [list]
--size :: Map k a -> Int
-- get a.list uid -> [uid]
-- lookup [uid] in indexed-refbymap 

{--generateRefby :: Int -> TraceMap -> RefbyMap
generateRefby tm = --}

refbyLookup :: UID -> RefbyMap -> [Label]
refbyLookup c m = getT $ Map.lookup c m
  where getT = maybe (error $ "References related to : " ++ c ++ " not found in RefbyMap") id

