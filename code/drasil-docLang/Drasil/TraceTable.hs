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

{--type TraceMap = Map.Map UID [Label]--}

traceMapsub1 :: (HasUID l, HasAdditionalNotes l) => LabelMap -> [l] -> TraceMap
traceMapsub1 lm = Map.fromList . map (\x -> ((x ^. uid ++ "Label"), lnames' lm (extractSFromNotes x)))

traceMapsub2 :: (HasUID l, HasDerivation l) => LabelMap -> [l] -> TraceMap
traceMapsub2 lm = Map.fromList . map (\x -> ((x ^. uid ++ "Label"), lnames' lm (extractSFromDeriv x)))

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
getTraceMapFromTM []                  = error "No TM found."

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

extractSFromNotes :: HasAdditionalNotes l => l -> [Sentence]
extractSFromNotes c = fromMaybe (error "No sentence collected") (c ^. getNotes)

extractSFromDeriv :: HasDerivation l => l -> [Sentence]
extractSFromDeriv c = (c ^. derivations)

getSCSSub :: [DocSection] -> [SCSSub]
getSCSSub a = getTraceMapFromSolCh $ getTraceMapFromSSDSub $ getTraceMapFromSSDSec
 $ getTraceMapFromDocSec a

generateTraceMap :: [DocSection] -> LabelMap -> TraceMap
generateTraceMap a lm = mergeMaps [(traceMapsub1 lm (getTraceMapFromTM $ getSCSSub a)), 
  (traceMap lm (getTraceMapFromGD $ getSCSSub a)), (traceMap lm (getTraceMapFromDD $ getSCSSub a)),
  (traceMap lm (getTraceMapFromIM $ getSCSSub a))]

mergeMaps :: [TraceMap] -> TraceMap
mergeMaps = foldl Map.union Map.empty











