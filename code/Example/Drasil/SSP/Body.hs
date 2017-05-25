module Drasil.SSP.Body where

import Control.Lens ((^.))
import Prelude hiding (id)

import Language.Drasil
import Data.Drasil.SI_Units 
import Data.Drasil.Authors

import Drasil.SSP.Defs
import Drasil.SSP.Units
import Drasil.SSP.Modules
import Drasil.SSP.Changes
import Drasil.SSP.Reqs
import qualified Drasil.SRS as SRS

import Drasil.ReferenceMaterial
import Drasil.DocumentLanguage
import Drasil.OrganizationOfSRS

import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Physics
import Data.Drasil.Concepts.PhysicalProperties
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Math hiding (constraint)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)
import Data.Drasil.Software.Products

import Data.Drasil.Quantities.SolidMechanics

import Data.Drasil.Utils

import Drasil.Template.MG
import Drasil.Template.DD

--type declerations for sections--
s2, s3, s4, s5, s6, s7 :: Section

s1_2_intro :: [TSIntro]

s2_1, s2_2, s2_3, s3_1, s3_2, s4_1, s4_1_1, s4_1_2,
  s4_1_3, s4_2, s4_2_1, s4_2_2, s4_2_3, s4_2_4,
  s4_2_5, s4_2_6, s5_1, s5_2 :: Section

s2_p1, s2_p2, s2_1_p1, s2_1_p2, s2_2_p1, s3_p1, s3_1_p1,
  s3_2_p1, s4_p1, s4_1_p1, s4_1_1_list, s4_1_2_p1, 
  s4_1_2_bullets, s4_1_2_p2, s4_1_2_fig1, s4_1_2_fig2, 
  s4_1_3_p1, s4_1_3_list, s4_2_p1, s4_2_1_p1, s4_2_1_list, 
  s4_2_2_p1, s4_2_3_p1, s4_2_4_p1, s4_2_5_p1, s4_2_5_p2,
  s4_2_5_p3, s5_p1, s5_1_list, s5_1_table,
  s5_2_p1, s7_list :: Contents

s4_2_2_tmods :: [Contents]

--Document Settup--
this_si :: [UnitDefn]
this_si = map UU [metre, degree] ++ map UU [newton, pascal]

ssp_si :: SystemInformation
ssp_si = SI ssa srs [henryFrankis]
  this_si sspSymbols (sspSymbols) acronyms 

mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro 
  [TUnits, tsymb s1_2_intro, TAandA]
  ) : map Verbatim [s2, s3, s4, s5, s6, s7]

ssp_srs, ssp_mg :: Document
ssp_srs = mkDoc mkSRS ssp_si
ssp_mg = mgDoc ssa (name henryFrankis) mgBod

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

-- SECTION 1 --
--automaticly generated in mkSRS 

-- SECTION 1.1 --
--automaticly generated in mkSRS 

-- SECTION 1.2 --
--automaticly generated in mkSRS using the intro bellow

s1_2_intro = [TSPurpose, TypogConvention [Verb $
  S "values with a subscript i implies that the value will" +:+
  S "be taken at and analyzed at a" +:+ (phrase slice) +:+ S "or" +:+ (phrase slice) +:+
  S "interface composing the total slip" +:+ (phrase $ mass ^. term)]]

-- SECTION 1.3 --
--automaticly generated in mkSRS 

-- SECTION 2 --
s2 = SRS.intro [s2_p1, s2_p2] [s2_1, s2_2, s2_3]

s2_p1 = Paragraph $ S "A" +:+ (phrase slope) +:+ S "of geological" +:+ 
  (phrase $ mass ^. term) `sC` S "composed of" +:+ (phrase soil) +:+ S "and rock," +:+
  S "is subject to the influence of gravity on the" +:+. (phrase $ mass ^. term) +:+
  S "For an unstable" +:+ (phrase slope) +:+ S "this can cause instability" +:+
  S "in the form of soil/rock movement. The effects of soil/rock movement" +:+
  S "can range from inconvenient to seriously hazardous, resulting in signifcant" +:+
  S "life and economic loses. Slope stability is of interest both when analyzing" +:+
  S "natural" +:+ (plural slope) `sC` S "and when designing an excavated" +:+. (phrase slope) +:+
  (at_start ssa) +:+ S "is the assessment of the safety of a" +:+ (phrase slope) `sC`
  S "identifying the" +:+ (phrase $ surface ^. term) +:+ S "most likely to" +:+
  S "experience slip and an index of it's relative stability known as the" +:+.
  (phrase $ fs_rc ^. term)

s2_p2 = Paragraph $ S "The following" +:+ (phrase section_) +:+
  S "provides an overview of the" +:+ (introduceAbb srs) +:+
  S "for a" +:+ (phrase ssa) +:+. (phrase problem) +:+ S "The developed" +:+
  (phrase $ program ^. term) +:+ S "will be referred to as the" +:+ (introduceAbb ssa) +:+.
  (phrase $ program ^. term) +:+ S "This" +:+ (phrase section_) +:+
  S "explains the purpose of this document," +:+ --FIXME: purpose, scope and organization have a similar pattern here
  S "the scope of the system, the organization of the document and" +:+
  S "the" +:+ (plural characteristic) +:+ S "of the intended readers."

-- SECTION 2.1 --
s2_1 = SRS.prpsOfDoc [s2_1_p1, s2_1_p2] []

s2_1_p1 = Paragraph $ S "The" +:+ (short ssa) +:+ (phrase $ program ^. term) +:+ 
  S "determines the" +:+ (phrase crtSlpSrf) `sC` S "and it's respective" +:+ 
  (phrase $ fs_rc ^. term) +:+ S "as a" +:+ (phrase method_) +:+ 
  S "of assessing the stability of a slope" +:+. (phrase design) +:+ 
  S "The" +:+ (phrase $ program ^. term) +:+ 
  S "is intended to be used as an educational tool for" +:+
  S "introducing" +:+ (phrase slope) +:+ S "stability issues, and will facilitate the" +:+
  S "analysis and" +:+ (phrase design) +:+ S "of a safe" +:+. (phrase slope)

s2_1_p2 = Paragraph $ S "This" +:+ (phrase document) +:+ S "will be used as a" +:+
  S "starting point for subsequent development phases, including" +:+
  S "writing the" +:+ (phrase desSpec) +:+ S "and the" +:+ (phrase software) +:+
  (phrase vav) +:+ S "plan. The" +:+ (phrase design) +:+ (phrase document) +:+
  S "will show how the" +:+ (plural requirement) +:+ S "are to be realized," +:+
  S "including decisions on the numerical algorithms and programming" +:+.
  (phrase environment) +:+ S "The" +:+ (phrase vav) +:+ S "plan will show the steps" +:+
  S "that will be used to increase confidence in the" +:+ (phrase softwareDoc) +:+
  S "and the implementation. Although the" +:+ (short srs) +:+ S "fits in a series of" +:+
  (plural document) +:+ S "that follow the so-called waterfall" +:+ (phrase model) `sC`
  S "the actual development process is not constrained in any way. Even when" +:+
  S "the waterfall" +:+ (phrase model) +:+ S "is not followed, as Parnas and Clements" +:+
  S "point out, the most logical way to present the" +:+ (phrase documentation) +:+
  S "is still to fake a rational" +:+ (phrase design) +:+ S "process."

-- SECTION 2.2 --
s2_2 = SRS.scpOfReq [s2_2_p1] []

s2_2_p1 = Paragraph $ S "The scope of the requirements is" +:+ --FIXME: somehow use scpOfReq with a "the"
  S "limited to stability analysis of a 2 dimensional" +:+ (phrase slope) `sC`
  S "composed of homogeneous" +:+. (plural soilLyr) +:+ S "Given appropriate" +:+
  S "inputs, the code for" +:+ (short ssa) +:+ S "will identify the most likely" +:+
  S "failure" +:+ (phrase $ surface ^. term) +:+ S "within the possible input range," +:+
  S "and find the" +:+ (phrase $ fs_rc ^. term) +:+ S "for the" +:+ (phrase slope) +:+
  S "as well as displacement of" +:+ (phrase soil) +:+ S "that will occur on the" +:+. (phrase slope)

-- SECTION 2.3 --
s2_3 = orgSecWTS start inModel s4_2_5 end 
  where start = S "The" +:+ (phrase organization) +:+
                S "of this" +:+ (phrase document) +:+ S "follows the template" +:+ 
                S "for an" +:+ (short srs) +:+ S "for" +:+ (phrase sciCompS) +:+
                S "proposed by Koothoor as well as Smith and Lai."
        end   = S "The" +:+ (plural inModel) +:+ S "provide the set of" +:+
                S "algebraic equations that must be solved iteratively to perform a" +:+
                (titleize morPrice) +:+ S "Analysis"
  
-- s2_3_p1 = Paragraph $ S "The" +:+ (phrase organization) +:+
  -- S "of this" +:+ (phrase document) +:+ S "follows the template" +:+ 
  -- S "for an" +:+ (short srs) +:+ S "for" +:+ (phrase sciCompS) +:+
  -- S "proposed by Koothoor as well as Smith and Lai." +:+ 
  -- S "The presentation follows the standard pattern of presenting" +:+
  -- S "goals" `sC` (plural theory) `sC` (plural definition) `sC`
  -- S "and" +:+. (plural assumption) +:+ S "For readers" +:+
  -- S "that would like a more bottom up approach, they can start" +:+
  -- S "reading the" +:+ (plural inModel) +:+ S "in" +:+ makeRef s4_2_5 +:+
  -- S "and trace back to find any additional" +:+ (phrase information) +:+
  -- S "they require. The" +:+ (plural inModel) +:+ S "provide the set of" +:+
  -- S "algebraic equations that must be solved iteratively to perform a" +:+
  -- (titleize morPrice) +:+ S "Analysis. The" +:+ (plural goalStmt) +:+
  -- S "are refined to the" +:+ (plural thModel) +:+ (sParen . makeRef) sec_TMs +:+ 
  -- S "and" +:+ (plural inModel) +:+. (sParen . makeRef) s4_2_5

-- SECTION 3 --
s3 = SRS.genSysDec [s3_p1] [s3_1, s3_2]

s3_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "provides general" +:+
  (phrase information) +:+ S "about the" +:+ (phrase system) `sC` S "identifies" +:+
  S "the interfaces between the" +:+ (phrase system) +:+ S "and its" +:+
  (phrase environment) `sC` S "and describes the" +:+ (plural userCharacteristic) +:+ 
  S "and the" +:+. (plural systemConstraint)

-- SECTION 3.1 --
s3_1 = SRS.userChar [s3_1_p1] []

s3_1_p1 = Paragraph $ S "The end" +:+ (phrase user) +:+ S "of" +:+ (short ssa) +:+
  S "should have an understanding of undergraduate Level 1 Calculus and" +:+
  (titleize physics) `sC` S "and be familiar with" +:+ (phrase soil) +:+
  S "and" +:+. (plural mtrlPrpty)

-- SECTION 3.2 --
s3_2 = SRS.sysCon [s3_2_p1] []
 
s3_2_p1 = Paragraph $ S "There are no" +:+. (plural systemConstraint)

-- SECTION 4 --
s4 = SRS.specSysDec [s4_p1] [s4_1, s4_2]

s4_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "first presents the" +:+
  (phrase problemDescription) `sC` S "which gives a high-level view of the" +:+
  (phrase problem) +:+ S "to be solved. This is followed by the" +:+
  (plural solutionCharSpec) `sC` S "which presents the" +:+ 
  (plural assumption) `sC` (plural theory) `sC` (plural definition) +:+
  S "and finally the" +:+ (plural inModel) +:+ S "that" +:+ (phrase model) +:+
  S "the" +:+. (phrase slope)

-- SECTION 4.1 --
s4_1 = SRS.probDesc [s4_1_p1] [s4_1_1, s4_1_2, s4_1_3]

s4_1_p1 = Paragraph $ (short ssa) +:+ S "is a computer" +:+ (phrase $ program ^. term) +:+
  S "developed to evaluate the" +:+ (phrase $ fs_rc ^. term) +:+ S "of a" +:+ 
  (phrase slope) :+: S "'s" +:+ (phrase slpSrf) +:+ --FIXME apostrophe on "slope's"
  S "and to calculate the displacement that the" +:+ (phrase slope) +:+ S "will experience."

-- SECTION 4.1.1 --
s4_1_1 = SRS.termogy [s4_1_1_list] []

s4_1_1_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (titleize $ fs_rc ^. term, 
      S "Stability metric. How likely a" +:+ (phrase slpSrf) +:+ S "is to experience" +:+
      S "failure through slipping."), 
  (titleize crtSlpSrf, 
      (at_start slpSrf) +:+ S "of the" +:+ (phrase slope) +:+ S "that has the lowest global" +:+
      (phrase $ fs_rc ^. term) `sC` S "and therefore most likely to experience failure."),
  (titleize $ stress ^. term,      stress ^. defn),
  (titleize $ strain ^. term,      strain ^. defn),
  (titleize $ normForce ^. term,   normForce ^. defn),
  (titleize $ shearForce ^. term,  shearForce ^. defn),
  (titleize $ tension ^. term,     tension ^. defn),
  (titleize $ compression ^. term, compression ^. defn),
  (S "Plane Strain", 
      S "The resultant stresses in one of the directions of a" +:+
      S "3 dimensional material can be approximated as 0. Results" +:+
      S "when the length of one dimension of the body dominates the" +:+
      S "others. Stresses in the dominate dimensions direction are" +:+
      S "the ones that can be approximated as 0.")
  ]

-- SECTION 4.1.2 --
s4_1_2 = SRS.physSyst [s4_1_2_p1, s4_1_2_bullets, s4_1_2_p2, s4_1_2_fig1, s4_1_2_fig2] []

s4_1_2_p1 = Paragraph $ S "Analysis of the" +:+ (phrase slope) +:+ S "is performed" +:+
  S "by looking at" +:+ (plural property) +:+ S "of the" +:+ (phrase slope) +:+
  S "as a series of" +:+ (phrase slice) +:+. (plural element) +:+ S "Some" +:+ (plural property) +:+
  S "are" +:+ (plural itslPrpty) `sC` S "and some are" +:+ (phrase slice) +:+ S "or" +:+
  (phrase slice) +:+ S "base properties." +:+ S "The index convention for referencing which" +:+
  (phrase intrslce) +:+ S "or" +:+ (phrase slice) +:+ S "is being used is shown in" +:+. 
  (makeRef fig_indexconv)

s4_1_2_bullets = Enumeration $ Bullet $ map Flat [
  ((at_start' itslPrpty) +:+ S "convention is noted by j. The end" +:+
    (plural itslPrpty) +:+ S "are usually not of interest" `sC` 
    S "therefore use the" +:+ (plural itslPrpty) +:+ S "from 1" +:+
    P (Special LEQ) +:+ S "i" +:+ P (Special LEQ) +:+. S "n-1"),
  ((at_start slice) +:+ S "properties convention is noted by i.")
  ]
  
s4_1_2_p2 = Paragraph $ S "A" +:+ (phrase $ fbd ^. term) +:+ S "of the forces" +:+
  S "acting on the" +:+ (phrase slice) +:+ S "is displayed in" +:+. (makeRef fig_forceacting)

s4_1_2_fig1 = fig_indexconv

fig_indexconv :: Contents
fig_indexconv = Figure (S "Index convention for numbering" +:+ (phrase slice) +:+ S "and" +:+
  (phrase intrslce) +:+ S "force variables") "IndexConvention.png"

s4_1_2_fig2 = fig_forceacting

fig_forceacting :: Contents
fig_forceacting = Figure (S "Forces acting on a" +:+ (phrase slice)) "ForceDiagram.png"

-- SECTION 4.1.3 --
s4_1_3 = SRS.goalStmt [s4_1_3_p1, s4_1_3_list] []

s4_1_3_p1 = Paragraph $ S "Given the geometry of the water" +:+
  S "table, the geometry of the layers composing the plane of a" +:+
  S "slope, and the" +:+ (plural mtrlPrpty) +:+ S "of the layers."

s4_1_3_list = Enumeration $ Simple $ mkEnumAbbrevList 1 (S "GS") [
  (S "Evaluate local and global" +:+ (plural $ fs_rc ^. term) +:+
      S "along a given" +:+. phrase slpSrf),
  (S "Identify the" +:+ (phrase crtSlpSrf) +:+ S "for the slope" `sC` 
      S "with the lowest" +:+. (phrase $ fs_rc ^. term)),
  (S "Determine the displacement of the" +:+. (phrase slope))
  ]

-- SECTION 4.2 --
s4_2 = SRS.solCharSpec [s4_2_p1] [s4_2_1, s4_2_2, s4_2_3, s4_2_4, s4_2_5, s4_2_6]

s4_2_p1 = Paragraph $ S "The" +:+ (plural inModel) +:+ S "that govern" +:+
  (short ssa) +:+ S "are presented in" +:+. makeRef s4_2_5 +:+
  S "The" +:+ (phrase information) +:+ S "to understand the meaning of the instance" +:+
  (plural model) +:+ S "and their derivation is also presented, so that the" +:+
  (plural inModel) +:+ S "can be verified."

-- SECTION 4.2.1 --
s4_2_1 = SRS.assump [s4_2_1_p1, s4_2_1_list] []

s4_2_1_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "simplifies the" +:+
  S "original" +:+ (phrase problem) +:+ S "and helps in developing the" +:+ (phrase thModel) +:+
  S "by filling in the missing" +:+ (phrase information) +:+ S "for the" +:+.
  (phrase physicalSystem) +:+ S "The numbers given in the square brackets refer to" +:+
  S "the" +:+ (phrase dataDefn) `sC` S "or the" +:+ (phrase inModel) `sC` S "in which the" +:+
  S "respective" +:+ (phrase assumption) +:+ S "is used."

s4_2_1_list = Enumeration $ Simple $ mkEnumAbbrevList 1 (S "A") [
  (S "The" +:+ (phrase slpSrf) +:+ S "is concave with respect to" +:+
           S "the" +:+. (phrase slopeSrf) +:+ S "The" +:+ P (coords ^. symbol) +:+ 
           S "coordinates of the failure" +:+ (phrase $ surface ^. term) +:+
           S "follow a monotonic function."),
  (S "The geometry of the" +:+ (phrase slope) `sC` S "and the" +:+
           (plural mtrlPrpty) +:+ S "of the" +:+ (plural soilLyr) +:+
           S "are given as inputs."),
  (S "The different layers of the" +:+ (phrase soil) +:+ S "are homogeneous," +:+
           S "with consistent" +:+ (plural soilPrpty) +:+ S "throughout," +:+
           S "and independent of dry or saturated" +:+ (plural condition) `sC`
           S "with the exception of" +:+ (phrase $ unit_ ^. term) +:+ S "weight."),
  ((at_start' soilLyr) +:+ S "are treated as if they have" +:+
           S "isotropic properties."),
  ((at_start intrslce) +:+ S "normal and shear forces have a" +:+
           S "linear relationship, proportional to a constant" +:+
           (sParen $ P $ lambda ^. symbol) +:+ S "and an" +:+
           (phrase intrslce) +:+ S "force function" +:+ (sParen $ P $ fi ^. symbol) +:+
           S "depending on x position."),
  ((at_start slice) +:+ S "to base normal and shear forces have" +:+
           S "a linear relationship, dependent on the" +:+
           (phrase $ fs_rc ^. term) +:+ (sParen $ P $ fs ^. symbol) `sC`
           S "and the Coulomb sliding law."),
  (S "The stress-strain curve for" +:+ (phrase intrslce) +:+
           S "relationships is linear with a constant" +:+. (phrase slope)),
  (S "The" +:+ (phrase slope) +:+ S "and" +:+ (phrase slpSrf) +:+
           S "extends far into and out of the geometry (z coordinate)." +:+
           S "This implies plane strain" +:+ (plural condition) `sC`
           S "making 2D analysis appropriate."),
  (S "The effective normal stress is large enough" +:+
           S "that the resistive shear to effective normal" +:+
           S "stress relationship can be approximated as a" +:+
           S "linear relationship."),
  (S "The" +:+ (phrase $ surface ^. term) +:+ S "and base of a" +:+
            (phrase slice) +:+ S "between" +:+ (phrase intrslce) +:+
            S "nodes are approximated as straight lines.")
  ]

-- SECTION 4.2.2 --
s4_2_2 = sec_TMs

sec_TMs :: Section
sec_TMs = SRS.thModel (s4_2_2_p1:s4_2_2_tmods) []

s4_2_2_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "focuses on the" +:+
  S "general equations and laws that" +:+ (short ssa) +:+
  S "is based on."

s4_2_2_tmods = map Definition [Theory fs_rc] --FIX fs_rc to use lowercase

-- SECTION 4.2.3 --
s4_2_3 = SRS.genDefn [s4_2_3_p1] []

s4_2_3_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "collects the laws and" +:+
  S "equations that will be used in deriving the" +:+ (plural dataDefn) `sC` S "which will" +:+
  S "in turn are used to build the" +:+. (plural inModel)

-- SECTION 4.2.4 --
s4_2_4 = SRS.dataDefn [s4_2_4_p1] []

s4_2_4_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "collects and defines all" +:+
  S "the" +:+ (plural datum) +:+ S "needed to build the" +:+. (plural inModel) +:+
  (at_start' definition) +:+ S "DD1 to DD8 are the force variables that" +:+
  S "can be solved by direct analysis of given inputs. The interslice" +:+ 
  S "forces DD9 are force variables that must be written" +:+ 
  S "in terms of DD1 to DD8 to solve."

-- SECTION 4.2.5 --
s4_2_5 = SRS.inModel [s4_2_5_p1,s4_2_5_p2,s4_2_5_p3] []

s4_2_5_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "transforms the" +:+
  (phrase problem) +:+ S "defined in" +:+ makeRef s4_1 +:+ S "into one which" +:+
  S "is expressed in mathematical terms. It used concrete symbols defined in" +:+
  makeRef s4_2_4 +:+ S "to replace the abstract" +:+ S "symbols in the" +:+
  (plural model) +:+ S "identified in" +:+ makeRef s4_2_2 +:+ S "and" +:+. makeRef s4_2_3

s4_2_5_p2 = Paragraph $ S "The" +:+ (titleize morPrice) +:+ (phrase method_) +:+ S "is a" +:+
  S "vertical slice, limit equilibrium" +:+ (phrase ssa) +:+ 
  S "method. Analysis is performed by breaking the assumed failure" +:+ 
  (phrase $ surface ^. term) +:+ S "into a series of vertical slices of mass. Static" +:+ 
  S "equilibrium analysis using two force equilibrium, and one" +:+ 
  S "moment equation as in T2. The" +:+ (phrase problem) +:+ S "is statically" +:+ --FIXME: T2,T3,GD5, DD1,DD9,DD10,DD11 should be references to other things in the body
  S "indeterminate with only these 3 equations and one constitutive" +:+ 
  S "equation (the Mohr Coulomb shear strength of T3)" +:+ 
  S "so the assumption of GD5 is used. Solving for force" +:+ 
  S "equilibrium allows" +:+ (plural definition) +:+ S "of all forces in terms of" +:+ 
  S "the physical properties of DD1 to DD9," +:+ 
  S "as done in DD10, DD11."

s4_2_5_p3 = Paragraph $ S "The values of the interslice normal force" +:+
  S "E the interslice normal/shear force magnitude ratio lambda," +:+ --FIXME: 'E' should be the symbol captital E, same with lambda
  S "and the" +:+ (titleize $ fs_rc ^. term) +:+ S "(FS)" `sC` S "are unknown." +:+ --FIXME: get the relation concept symbol 'FS' from factor of safety in Defs.hs
  S "Equations for the unknowns are written in terms of only the values" +:+ 
  S "in DD1 to DD9, the values of" +:+ (P $ ri ^. symbol) `sC` 
  S "and" +:+ (P $ ti ^. symbol) +:+ S "in DD10 and DD11, and each" +:+ --FIXME: DD10,DD11 should be references to other things in the body
  S "other. The relationships between the unknowns are non linear," +:+ 
  S "and therefore explicit equations cannot be derived and an" +:+ 
  S "iterative" +:+ (plural solution) +:+ S "method is required."

-- SECTION 4.2.6 --
s4_2_6 = datConF (makeRef s4_2_6Table2 +:+ S "and" +:+ makeRef s4_2_6Table3 +:+ S "show")
  EmptyS True EmptyS [s4_2_6Table2, s4_2_6Table3]

s4_2_6Table2, s4_2_6Table3 :: Contents --FIXME: actually create these table
s4_2_6Table2 = Table [] [] EmptyS True 
s4_2_6Table3 = Table [] [] EmptyS True

-- SECTION 5 --
s5 = SRS.require [s5_p1] [s5_1, s5_2]

s5_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "provides the" +:+
  (plural functionalRequirement) `sC` S "the business tasks that the" +:+
  (phrase software) +:+ S "is expected to complete, and the nonfunctional" +:+ 
  (plural requirement) `sC` S "the qualities that the" +:+ (phrase software) +:+ 
  S "is expected to exhibit."

-- SECTION 5.1 --
s5_1 = SRS.funcReq
  [s5_1_list, s5_1_table] []

s5_1_list = Enumeration $ Simple $ mkEnumAbbrevList 1 (S "R") [
  (S "Read the input file, and store the" +:+
        S "data. Necessary input data summarized in" +:+.
        (makeRef table_inputdata)),
  (S "Generate potential" +:+ (phrase crtSlpSrf) :+:
        S "'s for the input" +:+. (phrase slope)),
  (S "Test the" +:+ (plural slpSrf) +:+ S "to determine" +:+
        S "if they are physically realizable based" +:+
        S "on a set of pass or fail criteria."),
  (S "Prepare the" +:+ (plural slpSrf) +:+ S "for a" +:+ (phrase method_) +:+
        S "of" +:+ (plural slice) +:+ S "or limit equilibrium analysis."),
  (S "Calculate the" +:+ (plural $ fs_rc ^. term) +:+ S "of the" +:+. (plural slpSrf)),
  (S "Rank and weight the" +:+ (plural slope) +:+ S "based on their" +:+
        (phrase $ fs_rc ^. term) `sC` S "such that a" +:+ (phrase slpSrf) +:+
        S "with a smaller" +:+ (phrase $ fs_rc ^. term) +:+
        S "has a larger weighting."),
  (S "Generate new potential" +:+ (plural crtSlpSrf) +:+
        S "based on previously analysed" +:+ (plural slpSrf) +:+
        S "with low" +:+. (plural $ fs_rc ^. term)),
  (S "Repeat" +:+ (plural requirement) +:+ S "R3 to R7 until the" +:+
        S "minimum" +:+ (phrase $ fs_rc ^. term) +:+ S "remains approximately" +:+
        S "the same over a predetermined number of" +:+
        S "repetitions. Identify the" +:+ (phrase slpSrf) +:+
        S "that generates the minimum" +:+ (phrase $ fs_rc ^. term) +:+
        S "as the" +:+. (phrase crtSlpSrf)),
  (S "Prepare the" +:+ (phrase crtSlpSrf) +:+ S "for" +:+ (phrase method_) +:+ 
        S "of" +:+ (plural slice) +:+ S "or limit equilibrium analysis."),
  (S "Calculate the" +:+ (phrase $ fs_rc ^. term) +:+ S "of the" +:+
        (phrase crtSlpSrf) +:+ S "using the" +:+ (titleize morPrice) +:+.
        (phrase method_)),
  (S "Display the" +:+ (phrase crtSlpSrf) +:+ S "and the" +:+
        (phrase slice) +:+ (phrase element) +:+ S "displacements graphically." +:+
        S "Give the values of the" +:+ (plural $ fs_rc ^. term) +:+ S "calculated" +:+
        S "by the" +:+ (titleize morPrice) +:+. (phrase method_))
  ]
  
s5_1_table = table_inputdata

table_inputdata :: Contents
table_inputdata = Table [titleize symbol_, titleize' $ unit_ ^. term, titleize description]
  (mkTable
    [(\ch -> P $ ch ^. symbol),
     (\ch -> unwrap $ getUnit ch),
     (\ch -> phrase $ ch ^. term)]
    ((map cqs [coords, elastMod, cohesion]) ++ (map cqs [poissnsR]) ++ --this has to be seperate since poisson is a different type
    map cqs [fricAngle, dryWeight, satWeight, waterWeight]))
  (S "Input data") True
    where unwrap :: (Maybe UnitDefn) -> Sentence
          unwrap (Just a) = Sy (a ^. usymb)
          unwrap Nothing = EmptyS
 
-- SECTION 5.2 --
s5_2 = SRS.nonfuncReq [s5_2_p1] []

s5_2_p1 = Paragraph $ (short ssa) +:+ S "is intended to be an" +:+
  S "educational tool, therefore accuracy and performance speed" +:+
  S "are secondary" +:+ (phrase $ program ^. term) +:+ S "priorities to correctness," +:+
  S "understandability, reusability, and maintainability."

-- SECTION 6 --
s6 = SRS.likeChg [] []

-- References --
s7 = SRS.reference [s7_list] []

s7_list = Enumeration $ Simple $ map (\(a, b) -> (a, Flat b)) [ --FIXME: names should be in italics
  (S "[1]", S "Q.H. Qian D.Y. Zhu, C.F. Lee and G.R. Chen. A concise algorithm for computing" +:+
            S "the factor of safety using the morgensternprice method. Can. Geotech. J.," +:+
            S "(42):272-278, 19 February 2005."),
  (S "[2]", S "D.G. Fredlund and J.Krahn. Comparison of slope stability methods of" +:+
            S "analysis. Can. Geotech. J., (14):429-439, 4 April 1977."),
  (S "[3]", S "Nirmitha Koothoor. A document drive approach to certifying" +:+.
            (phrase sciCompS) +:+ S "Master's thesis, McMaster University," +:+
            S "Hamilton, Ontario, Canada, 2013."),
  (S "[4]", S "David L. Parnas and P.C. Clements. A rational design process: How" +:+
            S "and why to fake it. IEEE Transactions on Software Engineering," +:+
            S "12(2):251-257, February 1986."),
  (S "[5]", S "W. Spencer Smith and Lei Lai. A new requirements template for" +:+
            S "scientific computing. In J. Ralyt" :+: (F Acute 'e') `sC` S "P. Agerfalk, and N. Kraiem," +:+
            S "editors, Proceedings of the First International Workshopon" +:+
            S "Situational Requirements Engineering Processes - Methods," +:+
            S "Techniques and Tools to Support Situation-Specific Requirements" +:+
            S "Engineering Processes, SREP'05, pages 107-121, Paris, France," +:+
            S "2005. In conjunction with 13th IEEE International Requirements" +:+
            S "Engineering Conference."),
  (S "[6]", S "Dieter Stolle and Peijun Guo. Limit equilibrum" +:+ (phrase ssa) +:+
            S "using rigid finite elements. Can. Geotech. J., (45):653-662, 20 May 2008."),
  (S "[7]", S "Tony L.T Zhan Dao-Sheng Ling Yu-Chao Li, Yun-Min Chen and" +:+ 
            S "Peter John Cleall. An efficient approach for locating the" +:+
            (phrase crtSlpSrf) +:+ S "in" +:+ (plural ssa) +:+ S "using a" +:+
            S "real-coded genetic algorithm. Can. Geotech. J., (47):806-820," +:+
            S "25 June 2010.")]