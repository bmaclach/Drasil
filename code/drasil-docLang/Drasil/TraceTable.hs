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
import Data.Maybe (fromMaybe)

type TheoryModelMap    = Map.Map TheoryModel [Label]
type GenDefnMap        = Map.Map GenDefn [Label]
type DataDefinitionMap = Map.Map DataDefinition [Label]
type InstanceModelMap  = Map.Map InstanceModel [Label]

data TraceMap = TMap { _ttm :: TheoryModelMap
                   , _tgd   :: GenDefnMap 
                   , _tdd   :: DataDefinitionMap
                   , _tim   :: InstanceModelMap
                   } --TODO: Expand and add more databases
makeLenses ''TraceMap


theoryModelMap :: [TheoryModel] -> LabelMap -> TheoryModelMap
theoryModelMap = Map.fromList . map (\x y -> (x, lnames (extractSFromTM x) y))

genDefnMap :: [GenDefn] -> LabelMap -> GenDefnMap
genDefnMap = Map.fromList . map (\x y -> (x, lnames (extractSFromGD x) y))

dataDefinitionMap :: [DataDefinition] -> LabelMap -> DataDefinitionMap
dataDefinitionMap = Map.fromList . map (\x y -> (x, lnames (extractSFromDD x) y))

instanceModelMap :: [InstanceModel] -> LabelMap -> InstanceModelMap
instanceModelMap = Map.fromList . map (\x y -> (x, lnames (extractSFromIM x) y))

traceMap :: [TheoryModel] -> [GenDefn] -> [DataDefinition] -> [InstanceModel]
 -> LabelMap -> TraceMap
traceMap s t c u l = TMap (theoryModelMap s l) (genDefnMap t l)
 (dataDefinitionMap c l) (instanceModelMap u l)

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

extractSFromTM :: TheoryModel -> [Sentence]
extractSFromTM t = fromMaybe [] (t ^. getNotes) 

extractSFromGD :: GenDefn -> [Sentence]
extractSFromGD g = (fromMaybe [] (g ^. getNotes)) ++ (g ^. derivations)

extractSFromDD :: DataDefinition -> [Sentence]
extractSFromDD d = (fromMaybe [] (d ^. getNotes)) ++ (d ^. derivations)

extractSFromIM :: InstanceModel -> [Sentence]
extractSFromIM i = (fromMaybe [] (i ^. getNotes)) ++ (i ^. derivations)

getSCSSub :: [DocSection] -> [SCSSub]
getSCSSub a = getTraceMapFromSolCh $ getTraceMapFromSSDSub $ getTraceMapFromSSDSec
 $ getTraceMapFromDocSec a











