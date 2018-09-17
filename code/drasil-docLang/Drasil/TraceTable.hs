{-# LANGUAGE GADTs #-}
---------------------------------------------------------------------------
-- | Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.
module Drasil.TraceTable where

import Control.Lens ((^.))
import Drasil.DocumentLanguage
import Language.Drasil
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type TraceMap = Map.Map Label [Label]

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








