{-# LANGUAGE GADTs #-}
---------------------------------------------------------------------------
-- | Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.
module Drasil.DocumentLanguage where

import Drasil.DocumentLanguage.Definitions (Fields, ddefn, derivation, instanceModel, gdefn, tmodel,
  helperRefs)

import Language.Drasil hiding (Manual, Vector, Verb) -- Manual - Citation name conflict. FIXME: Move to different namespace
                                                     -- Vector - Name conflict (defined in file)
import Utils.Drasil

import Language.Drasil.Utils (sortBySymbol)
import Database.Drasil(SystemInformation(SI), asOrderedList, citeDB, conceptinsTable,
  termTable, unitTable, _authors, _concepts, _kind, _quants, _sys, _sysinfodb, _usedinfodb)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

import Control.Lens ((^.), over)
import qualified Data.Map as Map (elems)

import Drasil.Sections.TableOfAbbAndAcronyms (tableOfAbbAndAcronyms)
import Drasil.Sections.TableOfSymbols (table)
import Drasil.Sections.TableOfUnits (tableOfUnits)
import qualified Drasil.DocLang.SRS as SRS (appendix, dataDefn, genDefn,
  genSysDes, inModel, likeChg, unlikeChg, probDesc, reference, solCharSpec,
  stakeholder, thModel, tOfSymb, userChar, propCorSol, offShelfSol)
import qualified Drasil.Sections.AuxiliaryConstants as AC (valsOfAuxConstantsF)
import qualified Drasil.Sections.GeneralSystDesc as GSD (genSysF, genSysIntro,
  systCon, usrCharsF, sysContxt)
import qualified Drasil.Sections.Introduction as Intro (charIntRdrF,
  introductionSection, orgSec, purposeOfDoc, scopeOfRequirements)
import qualified Drasil.Sections.Requirements as R (reqF, fReqF, nfReqF)
import qualified Drasil.Sections.SpecificSystemDescription as SSD (assumpF,
  datConF, dataDefnF, genDefnF, inModelF, probDescF, solutionCharSpecIntro, 
  specSysDescr, thModF)
import qualified Drasil.Sections.Stakeholders as Stk (stakehldrGeneral,
  stakeholderIntro, tClientF, tCustomerF)
import qualified Drasil.Sections.TraceabilityMandGs as TMG (traceMGF)

import Data.Drasil.Concepts.Documentation (assumpDom, refmat)

import Data.Function (on)
import Data.List (nub, sortBy)

type System = Sentence
type DocKind = Sentence

{--}

type DocDesc = [DocSection]

-- | Document sections are either Reference, Introduction, or Specific
-- System Description sections (for now!)
data DocSection = RefSec RefSec
                | IntroSec IntroSec
                | StkhldrSec StkhldrSec
                | GSDSec GSDSec
                | SSDSec SSDSec
                | ReqrmntSec ReqrmntSec
                | LCsSec LCsSec
                | LCsSec' LCsSec'
                | UCsSec UCsSec
                | TraceabilitySec TraceabilitySec
                | AuxConstntSec AuxConstntSec
                | Bibliography
                | AppndxSec AppndxSec
                | ExistingSolnSec ExistingSolnSec

--FIXME: anything with 'Verb' in it should eventually go

{--}

-- | Reference section. Contents are top level followed by a list of subsections.
data RefSec = RefProg Contents [RefTab]

-- | Reference subsections
data RefTab where
  TUnits :: RefTab
  TUnits' :: [TUIntro] -> RefTab -- Customized intro
  TSymb :: [TSIntro] -> RefTab
  TSymb' :: LFunc -> [TSIntro] -> RefTab
  TAandA :: RefTab
  -- add more here

-- | For creating the table of symbols intro
data TSIntro = TypogConvention [TConvention] -- ^ Typographic conventions used
             | SymbOrder -- ^ Symbol ordering (defaults to alphabetical)
             | SymbConvention [Literature] -- ^ Symbol conventions match specified literature
             | TSPurpose -- ^ Purpose of the Table of Symbols

-- | Possible typographic conventions
data TConvention = Vector Emphasis -- ^ How vectors are emphasized
                 | Verb Sentence -- ^ Verbatim for specialized conventions

-- | How to handle Emphasis
data Emphasis = Bold
              | Italics

instance Show Emphasis where
  show Bold = "bold"
  show Italics = "italics"

-- | Types of literature
data Literature = Lit Topic -- ^ literature
                | Doc Topic -- ^ existing documentation for (singular topic)
                | Doc' Topic -- ^ existing documentation for (plural of topic)
                | Manual Topic -- ^ manual

type Topic = IdeaDict

-- | For creating the table of units intro
data TUIntro = System -- ^ System of units (defaults to SI)
             | Derived -- ^ Sentence about derived units being used alongside SI
             | TUPurpose -- ^ Purpose of the table of units

-- | Lens (lookup) functions (currently for TSymb)
data LFunc where
  Term :: LFunc
  Defn :: LFunc
  TermExcept :: [DefinedQuantityDict] -> LFunc
  DefnExcept :: [DefinedQuantityDict] -> LFunc
  TAD :: LFunc --Term and Definition

{--}

-- | Introduction section. Contents are top level followed by a list of
-- subsections.
data IntroSec = IntroProg Sentence Sentence [IntroSub]
  -- ^ Temporary, will be modified once we've figured out more about the section.

-- | Introduction subsections
data IntroSub where
  IPurpose :: Sentence -> IntroSub
  IScope   :: Sentence -> Sentence -> IntroSub
  IChar   :: [Sentence] -> [Sentence] -> [Sentence] -> IntroSub
  IOrgSec  :: Sentence -> CI -> Section -> Sentence -> IntroSub

{--}

-- | Stakeholders section
data StkhldrSec = StkhldrProg CI Sentence | StkhldrProg2 [StkhldrSub]

-- | Stakeholders subsections
data StkhldrSub where
  Client :: CI -> Sentence -> StkhldrSub
  Cstmr  :: CI -> StkhldrSub

{--}

data GSDSec = GSDProg [Section] Contents [Contents] [Section]
            | GSDProg2 [GSDSub]

data GSDSub where
  SysCntxt   :: [Contents] -> GSDSub --FIXME: partially automate
  UsrChars   :: [Contents] -> GSDSub
  SystCons   :: [Contents] -> [Section] -> GSDSub

{--}

-- | Specific System Description section . Contains a list of subsections.
newtype SSDSec = SSDProg [SSDSub]

-- | Specific system description subsections
data SSDSub where
  SSDSubVerb :: Section -> SSDSub
  SSDProblem :: ProblemDescription -> SSDSub
  SSDSolChSpec :: SolChSpec -> SSDSub

-- | Problem Description section
data ProblemDescription where
  PDProg :: Sentence -> CI -> Sentence -> [Section] -> ProblemDescription

-- | Solution Characteristics Specification section
data SolChSpec where
  SCSProg :: [SCSSub] -> SolChSpec

-- | Solution Characteristics Specification subsections
data SCSSub where
  Assumptions    :: SCSSub
  TMs            :: [Sentence] -> Fields  -> [TheoryModel] -> SCSSub
  GDs            :: [Sentence] -> Fields  -> [GenDefn] -> DerivationDisplay -> SCSSub
  DDs            :: [Sentence] -> Fields  -> [DataDefinition] -> DerivationDisplay -> SCSSub --FIXME: Need DD intro
  IMs            :: [Sentence] -> Fields  -> [InstanceModel] -> DerivationDisplay -> SCSSub
  Constraints    :: Sentence -> Sentence -> Sentence -> [LabelledContent] {-Fields  -> [UncertainWrapper] -> [ConstrainedChunk]-} -> SCSSub --FIXME: temporary definition?
--FIXME: Work in Progress ^
  CorrSolnPpties :: [Contents] -> SCSSub
data DerivationDisplay = ShowDerivation
                       | HideDerivation
{--}

newtype ReqrmntSec = ReqsProg [ReqsSub]

data ReqsSub where
  FReqsSub    :: [ConceptInstance] -> [LabelledContent] -> ReqsSub -- LabelledContent for tables
  NonFReqsSub :: [ConceptInstance] -> ReqsSub

{--}

newtype LCsSec = LCsProg [Contents] --FIXME:Should become [LikelyChanges]
newtype LCsSec' = LCsProg' [ConceptInstance]

{--}

newtype UCsSec = UCsProg [Contents]

{--}

data TraceabilitySec = TraceabilityProg [LabelledContent] [Sentence] [Contents] [Section]

{--}

-- | Off-The-Shelf Solutions section 
newtype ExistingSolnSec = ExistSolnProg [Contents]

{--}

-- | Values of Auxiliary Constants section
data AuxConstntSec = AuxConsProg CI [QDefinition]

{--}

newtype AppndxSec = AppndxProg [Contents]

{--}

-- | Creates a document from a document description and system information
mkDoc :: DocDesc -> (IdeaDict -> IdeaDict -> Sentence) -> SystemInformation -> Document
mkDoc l comb si@SI {_sys = sys, _kind = kind, _authors = authors} = Document
  (nw kind `comb` nw sys) (S $ manyNames authors) (mkSections si l)

-- | Helper for creating the document sections
mkSections :: SystemInformation -> DocDesc -> [Section]
mkSections si = map doit
  where
    doit :: DocSection -> Section
    doit (RefSec rs)         = mkRefSec si rs
    doit (IntroSec is)       = mkIntroSec si is
    doit (StkhldrSec sts)    = mkStkhldrSec sts
    doit (SSDSec ss)         = mkSSDSec si ss
    doit (AuxConstntSec acs) = mkAuxConsSec acs 
    doit Bibliography        = mkBib (citeDB si)
    doit (GSDSec gs')        = mkGSDSec gs'
    doit (ReqrmntSec r)      = mkReqrmntSec r
    doit (LCsSec lc')        = mkLCsSec lc'
    doit (LCsSec' lc)        = mkLCsSec' lc
    doit (UCsSec ulcs)       = mkUCsSec ulcs
    doit (TraceabilitySec t) = mkTraceabilitySec t
    doit (AppndxSec a)       = mkAppndxSec a
    doit (ExistingSolnSec o) = mkExistingSolnSec o


-- | Helper for creating the reference section and subsections
mkRefSec :: SystemInformation -> RefSec -> Section
mkRefSec si (RefProg c l) = section (titleize refmat) [c]
  (map (mkSubRef si) l) (makeSecRef "RefMat" "Reference Material") --DO NOT CHANGE LABEL OR THINGS WILL BREAK -- see Language.Drasil.Document.Extract
  where
    mkSubRef :: SystemInformation -> RefTab -> Section
    mkSubRef SI {_usedinfodb = db}  TUnits =
        tableOfUnits (sortBy comp_unitdefn $ map fst $ Map.elems $ db ^. unitTable) (tuIntro defaultTUI)
    mkSubRef SI {_usedinfodb = db} (TUnits' con) =
        tableOfUnits (sortBy comp_unitdefn $ map fst $ Map.elems $ db ^. unitTable) (tuIntro con)
    mkSubRef SI {_quants = v} (TSymb con) =
      SRS.tOfSymb 
      [tsIntro con,
                LlC $ table Equational (sortBySymbol
                $ filter (`hasStageSymbol` Equational) 
                (nub v))
                at_start] []
    mkSubRef SI {_concepts = cccs} (TSymb' f con) = mkTSymb cccs f con
    mkSubRef SI {_usedinfodb = db} TAandA =
      tableOfAbbAndAcronyms $ nub $ map fst $ Map.elems $ termTable db

-- | Helper for creating the table of symbols
mkTSymb :: (Quantity e, Concept e, Eq e, MayHaveUnit e) =>
  [e] -> LFunc -> [TSIntro] -> Section
mkTSymb v f c = SRS.tOfSymb [tsIntro c,
  LlC $ table Equational
    (sortBy (compsy `on` eqSymb) $ filter (`hasStageSymbol` Equational) (nub v))
    (lf f)] 
    []
  where lf Term = at_start
        lf Defn = (^. defn)
        lf (TermExcept cs) = \x -> if (x ^. uid) `elem` map (^. uid) cs then
          x ^. defn else at_start x --Compare chunk uids, since we don't
          --actually care about the chunks themselves in LFunc.
        lf (DefnExcept cs) = \x -> if (x ^. uid) `elem` map (^.uid) cs then
          at_start x else x ^. defn
        lf TAD = \tDef -> titleize tDef :+: S ":" +:+ (tDef ^. defn)

-- | table of symbols constructor
tsymb, tsymb' :: [TSIntro] -> RefTab
tsymb = TSymb 
-- ^ Default Term and given intro

tsymb' = TSymb' Defn
-- ^ Default Defn and given intro

-- | Custom table of symbols constructor
tsymb'' :: [TSIntro] -> LFunc -> RefTab
tsymb'' intro lfunc = TSymb' lfunc intro 
-- ^ Custom function and intro.

-- | table of symbols intro builder. Used by mkRefSec
tsIntro :: [TSIntro] -> Contents
tsIntro x = mkParagraph $ foldr ((+:+) . tsI) EmptyS x

-- | table of symbols intro writer. Translates a TSIntro to a list of Sentences
tsI :: TSIntro -> Sentence
tsI (TypogConvention ts) = typogConvention ts
tsI SymbOrder = S "The symbols are listed in alphabetical order."
tsI (SymbConvention ls) = symbConvention ls
tsI TSPurpose = S "The table that follows summarizes the symbols used in" +:+
  S "this document along with their units."

-- | typographic convention writer. Translates a list of typographic conventions
-- to a sentence
typogConvention :: [TConvention] -> Sentence
typogConvention [] = error "No arguments given for typographic conventions"
typogConvention ts = S "Throughout the document" `sC` makeSentence ts
  where makeSentence [x]     = tcon x :+: S "."
        makeSentence [x,y]   = tcon x +:+ S "and" +:+. tcon y
        makeSentence [x,y,z] = tcon x `sC` tcon y `sC` S "and" +:+. tcon z
        makeSentence (x:xs)  = tcon x `sC` makeSentence xs
        makeSentence  _      = error "How did you get here?"
        tcon (Vector emph)   = S ("symbols in " ++ show emph ++
                               " will represent vectors, and scalars otherwise")
        tcon (Verb s) = s

-- | symbolic convention writer.
symbConvention :: [Literature] -> Sentence
symbConvention [] = error "Attempting to reference no literature for SymbConvention"
symbConvention scs = S "The choice of symbols was made to be consistent with the" +:+
                      makeSentence scs
  where makeSentence [x]     = scon x :+: S "."
        makeSentence [x,y]   = scon x +:+ S "and with" +:+. scon y
        makeSentence [x,y,z] = scon x `sC` scon y `sC` S "and" +:+. scon z
        makeSentence (x:xs)  = scon x `sC` makeSentence xs
        makeSentence  _      = error "How did you get here?"
        scon (Lit x)         = phrase x +:+ S "literature"
        scon (Doc x)         = S "existing documentation for" +:+ phrase x
        scon (Doc' x)        = S "existing documentation for" +:+ plural x
        scon (Manual x)      = S "that used in the" +:+ phrase x +:+ S "manual"

-- | Table of units intro builder. Used by mkRefSec
tuIntro :: [TUIntro] -> Contents
tuIntro x = mkParagraph $ foldr ((+:+) . tuI) EmptyS x

-- | mkEnumSimple is a convenience function for converting lists into
-- Simple-type Enumerations.
mkEnumSimple :: (a -> ListTuple) -> [a] -> [Contents]
mkEnumSimple f = replicate 1 . UlC . ulcc . Enumeration . Simple . map f

-- | mkEnumSimpleD is a convenience function for transforming types which are
-- instances of the constraints Referable, HasShortName, and Definition, into
-- Simple-type Enumerations.
mkEnumSimpleD :: (Referable c, HasShortName c, Definition c) => [c] -> [Contents]
mkEnumSimpleD = mkEnumSimple $ mkListTuple (\x -> Flat $ x ^. defn)

-- | Creates a list tuple filling in the title with a ShortName and filling
-- reference information.
mkListTuple :: (Referable c, HasShortName c) => (c -> ItemType) -> c -> ListTuple
mkListTuple f x = (S . getStringSN $ shortname x, f x, Just $ refAdd x)

-- | table of units intro writer. Translates a TUIntro to a Sentence.
tuI :: TUIntro -> Sentence
tuI System  = 
  S "The unit system used throughout is SI (Système International d'Unités)."
tuI TUPurpose = 
  S "For each unit, the table lists the symbol, a description and the SI name."
tuI Derived = 
  S "In addition to the basic units, several derived units are also used."

-- | Default table of units intro contains the
defaultTUI :: [TUIntro]
defaultTUI = [System, Derived, TUPurpose]

mkIntroSec :: SystemInformation -> IntroSec -> Section
mkIntroSec si (IntroProg probIntro progDefn l) =
  Intro.introductionSection probIntro progDefn $ map (mkSubIntro si) l
  where
    mkSubIntro :: SystemInformation -> IntroSub -> Section
    mkSubIntro _ (IPurpose intro) = Intro.purposeOfDoc intro
    mkSubIntro SI {_sys = sys} (IScope main intendedPurp) =
      Intro.scopeOfRequirements main sys intendedPurp
    mkSubIntro SI {_sys = sys} (IChar assumed topic asset) =
      Intro.charIntRdrF sys assumed topic asset (SRS.userChar [] [])
    mkSubIntro _ (IOrgSec i b s t)  = Intro.orgSec i b s t
    -- FIXME: s should be "looked up" using "b" once we have all sections being generated

-- | Helper for making the 'Stakeholders' section
mkStkhldrSec :: StkhldrSec -> Section
mkStkhldrSec (StkhldrProg key details) = Stk.stakehldrGeneral key details
mkStkhldrSec (StkhldrProg2 l) = SRS.stakeholder [Stk.stakeholderIntro] $ map mkSubs l
  where
    mkSubs :: StkhldrSub -> Section
    mkSubs (Client kWrd details) = Stk.tClientF kWrd details
    mkSubs (Cstmr kWrd)          = Stk.tCustomerF kWrd

-- | Helper for making the 'General System Description' section
mkGSDSec :: GSDSec -> Section
mkGSDSec (GSDProg cntxt uI cnstrnts systSubSec) = GSD.genSysF cntxt uI cnstrnts systSubSec
mkGSDSec (GSDProg2 l) = SRS.genSysDes [GSD.genSysIntro] $ map mkSubs l
   where
     mkSubs :: GSDSub -> Section
     mkSubs (SysCntxt cs)            = GSD.sysContxt cs
     mkSubs (UsrChars intro)         = GSD.usrCharsF intro
     mkSubs (SystCons cntnts subsec) = GSD.systCon cntnts subsec

-- | Helper for making the 'Specific System Description' section
mkSSDSec :: SystemInformation -> SSDSec -> Section
mkSSDSec si (SSDProg l) =
  SSD.specSysDescr $ map (mkSubSSD si) l
  where
    mkSubSSD :: SystemInformation -> SSDSub -> Section
    mkSubSSD _ (SSDSubVerb s)        = s
    mkSubSSD sysi (SSDProblem pd)    = mkSSDProb sysi pd
    mkSubSSD sysi (SSDSolChSpec scs) = mkSolChSpec sysi scs

mkSSDProb :: SystemInformation -> ProblemDescription -> Section
mkSSDProb _ (PDProg start progName end subSec) =
  SSD.probDescF start progName end subSec

mkSolChSpec :: SystemInformation -> SolChSpec -> Section
mkSolChSpec si (SCSProg l) =
  SRS.solCharSpec [SSD.solutionCharSpecIntro (siSys si) inModSec] $
    map (mkSubSCS si) l
  where
    mkSubSCS :: SystemInformation -> SCSSub -> Section
    mkSubSCS _ (TMs _ _ [])   = error "There are no Theoretical Models"
    mkSubSCS _ (GDs _ _ [] _) = SSD.genDefnF []
    mkSubSCS _ (DDs _ _ [] _) = error "There are no Data Definitions"
    mkSubSCS _ (IMs _ _ [] _)  = error "There are no Instance Models"
    mkSubSCS si' (TMs intro fields ts) =
      SSD.thModF (siSys si') $ map mkParagraph intro ++ map (LlC . tmodel fields si') ts
    mkSubSCS si' (DDs intro fields dds ShowDerivation) = --FIXME: need to keep track of DD intro.
      SSD.dataDefnF EmptyS $ map mkParagraph intro ++ concatMap (\x -> (LlC $ ddefn fields si' x) : derivation x) dds
    mkSubSCS si' (DDs intro fields dds _) =
      SSD.dataDefnF EmptyS $ map mkParagraph intro ++ map (LlC . ddefn fields si') dds
    mkSubSCS si' (GDs intro fields gs' ShowDerivation) =
      SSD.genDefnF $ map mkParagraph intro ++ concatMap (\x -> (LlC $ gdefn fields si' x) : derivation x) gs'
    mkSubSCS si' (GDs intro fields gs' _) =
      SSD.genDefnF $ map mkParagraph intro ++ map (LlC . gdefn fields si') gs'
    mkSubSCS si' (IMs intro fields ims ShowDerivation) =
      SSD.inModelF pdStub ddStub tmStub (SRS.genDefn [] []) $ map mkParagraph intro ++
      concatMap (\x -> LlC (instanceModel fields si' x) : derivation x) ims
    mkSubSCS si' (IMs intro fields ims _) =
      SSD.inModelF pdStub ddStub tmStub (SRS.genDefn [] []) $ map mkParagraph intro ++
      map (LlC . instanceModel fields si') ims
    mkSubSCS si' Assumptions =
      SSD.assumpF tmStub gdStub ddStub imStub lcStub ucStub $ mkEnumSimpleD .
      map (`helperCI` si') . filter (\x -> sDom (cdom x) == assumpDom ^. uid) .
      asOrderedList $ _sysinfodb si' ^. conceptinsTable
      {-where
        -- Duplicated here to avoid "leaking" the definition from drasil-lang
        -- Commented out since its imported from drasil-database now
        sDom :: [UID] -> UID
        sDom [u] = u
        sDom u = error $ "Expected ConceptDomain to have a single domain, found " ++
          show (length u) ++ " instead."-}
    mkSubSCS _ (CorrSolnPpties cs)   = SRS.propCorSol cs []
    mkSubSCS _ (Constraints a b c d) = SSD.datConF a b c d
    inModSec = SRS.inModel [mkParagraph EmptyS] []
    --FIXME: inModSec should be replaced with a walk
    -- over the SCSProg and generate a relevant intro.
    -- Could start with just a quick check of whether or not IM is included and
    -- then error out if necessary.

helperCI :: ConceptInstance -> SystemInformation -> ConceptInstance
helperCI a c = over defn (\x -> foldlSent_ [x, helperRefs a c]) a
{--}

-- | Section stubs for implicit referencing
tmStub, gdStub, ddStub, imStub, lcStub, ucStub, pdStub:: Section
tmStub = SRS.thModel   [] []
gdStub = SRS.genDefn   [] []
ddStub = SRS.dataDefn  [] []
imStub = SRS.inModel   [] []
lcStub = SRS.likeChg   [] []
ucStub = SRS.unlikeChg [] []
pdStub = SRS.probDesc  [] []

-- | Helper for making the 'Requirements' section
mkReqrmntSec :: ReqrmntSec -> Section
mkReqrmntSec (ReqsProg l) = R.reqF $ map mkSubs l
  where
    mkSubs :: ReqsSub -> Section
    mkSubs (FReqsSub frs tbs) = R.fReqF (mkEnumSimpleD frs ++ map LlC tbs)
    mkSubs (NonFReqsSub nfrs) = R.nfReqF (mkEnumSimpleD nfrs)

{--}

-- | Helper for making the 'LikelyChanges' section
mkLCsSec :: LCsSec -> Section
mkLCsSec (LCsProg c) = SRS.likeChg c []

mkLCsSec' :: LCsSec' -> Section
mkLCsSec' (LCsProg' c) = SRS.likeChg (mkEnumSimpleD c) []

{--}

-- | Helper for making the 'UnikelyChanges' section
mkUCsSec :: UCsSec -> Section
mkUCsSec (UCsProg c) = SRS.unlikeChg c []

{--}

-- | Helper for making the 'Traceability Matrices and Graphs' section
mkTraceabilitySec :: TraceabilitySec -> Section
mkTraceabilitySec (TraceabilityProg refs trailing otherContents subSec) =
  TMG.traceMGF refs trailing otherContents subSec

{--}

-- | Helper for making the 'Off-the-Shelf Solutions' section
mkExistingSolnSec :: ExistingSolnSec -> Section
mkExistingSolnSec (ExistSolnProg cs) = SRS.offShelfSol cs [] 

{--}

-- | Helper for making the 'Values of Auxiliary Constants' section
mkAuxConsSec :: AuxConstntSec -> Section
mkAuxConsSec (AuxConsProg key listOfCons) = AC.valsOfAuxConstantsF key $ sortBySymbol listOfCons

{--}

-- | Helper for making the bibliography section
mkBib :: BibRef -> Section
mkBib bib = SRS.reference [UlC $ ulcc (Bib bib)] []

{--}

-- | Helper for making the 'Appendix' section
mkAppndxSec :: AppndxSec -> Section
mkAppndxSec (AppndxProg cs) = SRS.appendix cs []

{--}

-- Helper
siSys :: SystemInformation -> IdeaDict
siSys SI {_sys = sys} = nw sys
