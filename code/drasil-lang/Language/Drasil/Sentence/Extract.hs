module Language.Drasil.Sentence.Extract(sdep, snames, lnames, lnames') where

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
lnames   :: LabelMap -> Sentence -> [Label]
lnames l (Ch a)        = []
lnames l (Sy _)        = []
lnames l (S _)         = []
lnames l (Sp _)        = []
lnames l (P _)         = []
lnames l (Ref u _ _ _) = [labelLookup u l]
lnames l ((:+:) a b)   = (lnames l a) ++ (lnames l b)
lnames l (Quote a)     = []
lnames l (E a)         = []
lnames l (EmptyS)      = []

lnames'  :: LabelMap -> [Sentence] -> [Label]
lnames' l (hd:tl) = lnames l hd ++ lnames' l tl
lnames' l (hd:[]) = lnames l hd
lnames' l []      = []
