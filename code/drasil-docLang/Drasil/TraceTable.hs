{-# LANGUAGE GADTs, TemplateHaskell #-}
---------------------------------------------------------------------------
-- | Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.
module Drasil.TraceTable where

import Control.Lens ((^.),makeLenses)
import Drasil.DocumentLanguage
import Language.Drasil
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Drasil.SentenceStructures (foldlSent)


{--type TheoryModelMap    = Map.Map TheoryModel [Label]
type GenDefnMap        = Map.Map GenDefn [Label]
type DataDefinitionMap = Map.Map DataDefinition [Label]
type InstanceModelMap  = Map.Map InstanceModel [Label]


data TraceMap = TMap { _ttm :: TheoryModelMap
                   , _tgd   :: GenDefnMap 
                   , _tdd   :: DataDefinitionMap
                   , _tim   :: InstanceModelMap
                   } --TODO: Expand and add more databases
makeLenses ''TraceMap


theoryModelMap :: LabelMap -> [TheoryModel] -> TheoryModelMap
theoryModelMap lm = Map.fromList . map (\x -> (x, lnames (extractSFromTM x) lm)) . (sortBy uidSort)

genDefnMap :: LabelMap -> [GenDefn] -> GenDefnMap
genDefnMap lm = Map.fromList . map (\x -> (x, lnames (extractSFromGD x) lm)) . (sortBy uidSort)

dataDefinitionMap :: LabelMap -> [DataDefinition] -> DataDefinitionMap
dataDefinitionMap lm = Map.fromList . map (\x -> (x, lnames (extractSFromDD x) lm)) . (sortBy uidSort)

instanceModelMap :: LabelMap -> [InstanceModel] -> InstanceModelMap
instanceModelMap lm = Map.fromList . map (\x -> (x, lnames (extractSFromIM x) lm)) . (sortBy uidSort)

traceMap :: [TheoryModel] -> [GenDefn] -> [DataDefinition] -> [InstanceModel]
 -> LabelMap -> TraceMap
traceMap s t c u l = TMap (theoryModelMap l s) (genDefnMap l t)
 (dataDefinitionMap l c) (instanceModelMap l u)--}

type TraceMap = Map.Map UID [Label]

traceMapsub1 :: (HasUID l, HasAdditionalNotes l) => LabelMap -> [l] -> TraceMap
traceMapsub1 lm = Map.fromList . map (\x -> ((x ^. uid), lnames (extractSFromNotes x) lm))

traceMapsub2 :: (HasUID l, HasDerivation l) => LabelMap -> [l] -> TraceMap
traceMapsub2 lm = Map.fromList . map (\x -> ((x ^. uid), lnames (extractSFromDeriv x) lm))

traceMap :: (HasUID l, HasDerivation l, HasAdditionalNotes l) => LabelMap -> [l] -> TraceMap
traceMap lm l = Map.union (traceMapsub1 lm l) (traceMapsub2 lm l)

getTraceMapFromDocSec :: [DocSection] -> SSDSec
getTraceMapFromDocSec ((SSDSec ssd):_)  = ssd
getTraceMapFromDocSec  (hd:tl)          = getTraceMapFromDocSec tl
getTraceMapFromDocSec []                = error "No SSDSec found."

getTraceMapFromSSDSec :: SSDSec -> [SSDSub]
getTraceMapFromSSDSec (SSDProg s)       = s
getTraceMapFromSSDSec _                 = error "No SSDSub found."

getTraceMapFromSSDSub :: [SSDSub] -> SolChSpec
getTraceMapFromSSDSub ((SSDSolChSpec s):_) = s
getTraceMapFromSSDSub (hd:tl)              = getTraceMapFromSSDSub tl
getTraceMapFromSSDSub _                    = error "No SolChSpec found."

getTraceMapFromSolCh :: SolChSpec -> [SCSSub]
getTraceMapFromSolCh (SCSProg sub) = sub
getTraceMapFromSolCh _ = error "No SCSSub found."

getTraceMapFromTM :: [SCSSub] -> [TheoryModel]
getTraceMapFromTM ((TMs _ tm):_)      = tm
getTraceMapFromTM  (hd:tl)            = getTraceMapFromTM tl
getTraceMapFromTM []                  = []

getTraceMapFromGD :: [SCSSub] -> [GenDefn]
getTraceMapFromGD ((GDs _ gd _):_)      = gd
getTraceMapFromGD  (hd:tl)              = getTraceMapFromGD tl
getTraceMapFromGD []                    = []

getTraceMapFromDD :: [SCSSub] -> [DataDefinition]
getTraceMapFromDD ((DDs _ dd _):_)      = dd
getTraceMapFromDD  (hd:tl)              = getTraceMapFromDD tl
getTraceMapFromDD []                    = []

getTraceMapFromIM :: [SCSSub] -> [InstanceModel]
getTraceMapFromIM ((IMs _ im _):_)      = im
getTraceMapFromIM  (hd:tl)              = getTraceMapFromIM tl
getTraceMapFromIM []                    = []

extractSFromNotes :: HasAdditionalNotes l => l -> Sentence
extractSFromNotes c = foldlSent $ fromMaybe [] (c ^. getNotes)

extractSFromDeriv :: HasDerivation l => l -> Sentence
extractSFromDeriv c = foldlSent (c ^. derivations)

{--extractSFromTM :: TheoryModel -> Sentence
extractSFromTM t = foldlSent $ fromMaybe [] (t ^. getNotes) 

extractSFromGD :: GenDefn -> Sentence
extractSFromGD g =  foldlSent ((fromMaybe [] (g ^. getNotes)) ++ (g ^. derivations))

extractSFromDD :: DataDefinition -> Sentence
extractSFromDD d = foldlSent ((fromMaybe [] (d ^. getNotes)) ++ (d ^. derivations))

extractSFromIM :: InstanceModel -> Sentence
extractSFromIM i = foldlSent ((fromMaybe [] (i ^. getNotes)) ++ (i ^. derivations))--}

getSCSSub :: [DocSection] -> [SCSSub]
getSCSSub a = getTraceMapFromSolCh $ getTraceMapFromSSDSub $ getTraceMapFromSSDSec
 $ getTraceMapFromDocSec a











