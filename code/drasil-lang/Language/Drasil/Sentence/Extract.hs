module Language.Drasil.Sentence.Extract(sdep, snames, lnames) where

import Data.List (nub)
import Language.Drasil.Spec(Sentence(..))
import Language.Drasil.Expr.Extract(names)
import Language.Drasil.Label.Core(Label, labelLookup, LabelMap)
-- | Generic traverse of all positions that could lead to names from sentences
snames   :: Sentence -> [String]
snames (Ch a)        = [a]
snames (Sy _)        = []
snames (S _)         = []
snames (Sp _)        = []
snames (P _)         = []
snames (Ref _ _ _ _) = []
snames ((:+:) a b)   = (snames a) ++ (snames b)
snames (Quote a)     = snames a
snames (E a)         = names a
snames (EmptyS)      = []
-----------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above
sdep :: Sentence -> [String]
sdep = nub . snames

-- | Generic traverse of all positions that could lead to labels from sentences
lnames   :: Sentence -> LabelMap -> [Label]
lnames (Ch a)        l = []
lnames (Sy _)        l = []
lnames (S _)         l = []
lnames (Sp _)        l = []
lnames (P _)         l = []
lnames (Ref u _ _ _) l = [labelLookup u l]
lnames ((:+:) a b)   l = (lnames a l) ++ (lnames b l)
lnames (Quote a)     l = lnames a l
lnames (E a)         l = []
lnames (EmptyS)      l = []
