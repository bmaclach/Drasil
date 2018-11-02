--{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.HTML.Print(genHTML) where

import Prelude hiding (print)
import Data.List (sortBy,partition,intersperse)
import Text.PrettyPrint hiding (render, Str)
import Numeric (showEFloat)
import Control.Arrow (second)
import Control.Lens ((^.), makeLenses, view, Lens')

import qualified Language.Drasil as L (People, Person, 
  CitationKind(Misc, Book, MThesis, PhDThesis, Article), 
  Symbol(Corners, Concat, Special, Atomic, Empty, Atop), USymb(US),
  DType(DD, TM, Instance, General), MaxWidthPercent, RefType(Link),
  Decoration(Prime, Hat, Vector), Document, HasDefinitionTable, HasSymbolTable,
  nameStr, rendPersLFM, rendPersLFM', rendPersLFM'', special)

import Language.Drasil.HTML.Monad (unPH)
import Language.Drasil.HTML.Helpers (em, wrap, refwrap, caption, image, div_tag,
  td, th, tr, bold, sub, sup, cases, fraction, reflink, reflinkURI, paragraph, h, html, body,
  author, article_title, title, linkCSS, head_tag)
import qualified Language.Drasil.Output.Formats as F

import Language.Drasil.Config (StyleGuide(APA, MLA, Chicago), bibStyleH)
import Language.Drasil.Printing.Import (makeDocument)
import Language.Drasil.Printing.AST (Spec, ItemType(Flat, Nested),  
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Expr, Fence(Curly, Paren, Abs, Norm),
  Ops(Prod, Inte, Mul, Summ, Or, Add, And, Subt, Iff, Impl, GEq, LEq, Lt, Gt, NEq, Eq,
  Dot, Cross, Neg, Exp, Not, Dim, Cot, Csc, Sec, Tan, Cos, Sin, Log, Ln, Prime, Comma, Boolean, 
  Real, Rational, Natural, Integer, IsIn, Point), 
  Expr(Sub, Sup, Over, Sqrt, Spc, Font, MO, Fenced, Spec, Ident, Row, Mtx, Case, Div, Str, 
  Int, Dbl), Spec(Quote, EmptyS, Ref, HARDNL, Sp, Sy, S, E, (:+:)),
  Spacing(Thin), Fonts(Bold, Emph), OverSymb(Hat), Label)
import Language.Drasil.Printing.Citation (CiteField(Year, Number, Volume, Title, Author, 
  Editor, Pages, Type, Month, Organization, Institution, Chapter, HowPublished, School, Note,
  Journal, BookTitle, Publisher, Series, Address, Edition), HP(URL, Verb), 
  Citation(Cite), BibRef)
import Language.Drasil.Printing.LayoutObj (Tags, Document(Document),
  LayoutObj(Graph, Bib, List, Header, Figure, Definition, Table, EqnBlock, Paragraph, 
  HDiv, ALUR))
import Language.Drasil.Printing.Helpers (comm, dot, paren, sufxer, sqbrac)
import Language.Drasil.Printing.PrintingInformation (HasPrintingOptions(..), HasOptions(..), Option(MathJax, Html))
import qualified Language.Drasil.TeX.Print as TP (p_expr)
data OpenClose = Open | Close

-- | Generate an HTML document from a Drasil 'Document'
genHTML :: (L.HasSymbolTable ctx, L.HasDefinitionTable ctx, HasPrintingOptions ctx, HasOptions ctx) =>
  ctx -> F.Filename -> L.Document -> Doc
genHTML sm fn doc = build sm fn (makeDocument sm doc)

-- | Build the HTML Document, called by genHTML
build :: HasOptions s => s -> String -> Document -> Doc
build s fn (Document t a c) =
  text ( "<!DOCTYPE html>") $$
  html ( head_tag ((linkCSS fn) $$ title (title_spec s t) $$
  text ("<meta charset=\"utf-8\">") $$
  text ("<script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/"++
          "2.7.0/MathJax.js?config=TeX-MML-AM_CHTML'></script>")) $$
  body (article_title (p_spec t s) $$ author (p_spec a s)
  $$ print s c
  ))
    
	
--getExprFn :: Option -> (Expr -> String)
--getExprFn MathJax = \x -> "\\(" ++ TP.p_expr x ++ "\\)"
--getExprFn Html =  p_expr
  
-- | Helper for rendering LayoutObjects into HTML
printLO :: HasOptions s => s -> LayoutObj -> Doc
printLO s (HDiv ts layoutObs l) = refwrap (p_spec l s) $
                                 div_tag ts (vcat (map (printLO s) layoutObs))
printLO s (Paragraph contents)  = paragraph $ p_spec contents s
printLO s (EqnBlock contents)     = p_spec contents s
printLO s (Table ts rows r b t)   = makeTable s ts rows (p_spec r s) b (p_spec t s)
printLO s (Definition dt ssPs l)  = makeDefn s dt ssPs (p_spec l s)
printLO s (Header n contents _)   = h (n + 1) $ p_spec contents s -- FIXME
printLO s (List t)                = makeList s t
printLO s (Figure r c f wp)       = makeFigure (p_spec r s) (p_spec c s) (text f) wp
printLO s (ALUR _ x l i)          = wrap "ul" ["hide-list-style"] $
  makeRefList (p_spec x s) (p_spec l s) (p_spec i s)
printLO s (Bib bib)               = makeBib s bib
printLO _ (Graph _ _ _ _ _)       = empty -- FIXME


-- | Called by build, uses 'printLO' to render the layout
-- objects in Doc format.
print :: HasOptions s => s -> [LayoutObj] -> Doc
print s l = foldr ($$) empty $ map (printLO s) l

-----------------------------------------------------------------
--------------------BEGIN SPEC PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders the title of the document. Different than body rendering
-- because newline can't be rendered in an HTML title.
title_spec :: HasOptions s => s -> Spec -> Doc
title_spec s (a :+: b)  = title_spec s a <> title_spec s b
title_spec _ HARDNL    = empty
title_spec sm s         = p_spec s sm

-- | Renders the Sentences in the HTML body (called by 'printLO')
p_spec :: HasOptions s => Spec -> s -> Doc
--p_spec (E e)           _  = em $ text $ getExprFn e
--p_spec (E e)             = em $ text $ p_expr e           
p_spec (E e)       s = case s ^. getOption of
  MathJax -> em $ text $ "\\(" ++ TP.p_expr e ++ "\\)"
  Html -> em $ text $ p_expr e
p_spec (a :+: b)        s = p_spec a s <> p_spec b s
p_spec (S s)            _ = text s 
p_spec (Sy s)           _ = text $ uSymb s
p_spec (Sp s)           _ = text $ unPH $ L.special s
p_spec HARDNL           _ = text "<br />"
p_spec (Ref L.Link r a _) s = reflinkURI r $ p_spec a s
p_spec (Ref _      r a _) s = reflink    r $ p_spec a s
p_spec EmptyS           _ = text "" -- Expected in the output
p_spec (Quote q)        s = text "&quot;" <> p_spec q s <> text "&quot;"
-- p_spec (Acc Grave c)     = text $ '&' : c : "grave;" --Only works on vowels.
-- p_spec (Acc Acute c)     = text $ '&' : c : "acute;" --Only works on vowels.

-- | Renders symbols for HTML document
symbol :: L.Symbol -> String
symbol (L.Atomic s)  = s
symbol (L.Special s) = unPH $ L.special s
symbol (L.Concat sl) = foldr (++) "" $ map symbol sl
--symbol (Greek g)   = unPH $ greek g
-- handle the special cases first, then general case
symbol (L.Corners [] [] [x] [] s) = (symbol s) ++ sup (symbol x)
symbol (L.Corners [] [] [] [x] s) = (symbol s) ++ sub (symbol x)
symbol (L.Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (L.Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (L.Corners _ _ _ _ _)      = error "rendering of L.Corners (general)"
symbol (L.Atop L.Vector s)        = "<b>" ++ symbol s ++ "</b>"
symbol (L.Atop L.Hat s)           = symbol s ++ "&#770;"
symbol (L.Atop L.Prime s)         = symbol s ++ "&prime;"
symbol L.Empty                    = ""

uSymb :: L.USymb -> String
uSymb (L.US ls) = formatu t b
  where
    (t,b) = partition ((> 0) . snd) ls
    formatu :: [(L.Symbol,Integer)] -> [(L.Symbol,Integer)] -> String
    formatu [] l = line l
    formatu l [] = concat $ intersperse "&sdot;" $ map pow l
    formatu nu de = line nu ++ "/" ++ (line $ map (second negate) de)
    line :: [(L.Symbol,Integer)] -> String
    line []  = ""
    line [x] = pow x
    line l   = '(' : (concat $ intersperse "&sdot;" $ map pow l) ++ ")"
    pow :: (L.Symbol,Integer) -> String
    pow (x,1) = symbol x
    pow (x,p) = symbol x ++ sup (show p)

-----------------------------------------------------------------
------------------BEGIN EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
-- | Renders expressions in the HTML (called by multiple functions)
p_expr :: Expr -> String
p_expr (Dbl d)        = showEFloat Nothing d ""
p_expr (Int i)        = show i
p_expr (Str s)        = s
p_expr (Div a b)      = fraction (p_expr a) (p_expr b)
p_expr (Case ps)      = cases ps p_expr
p_expr (Mtx a)        = "<table class=\"matrix\">\n" ++ p_matrix a ++ "</table>"
p_expr (Row l)        = concatMap p_expr l
p_expr (Ident s)      = s
p_expr (Spec s)       = unPH $ L.special s
--p_expr (Gr g)         = unPH $ greek g
p_expr (Sub e)        = sub $ p_expr e
p_expr (Sup e)        = sup $ p_expr e
p_expr (Over Hat s)   = p_expr s ++ "&#770;"
p_expr (MO o)         = p_ops o
p_expr (Fenced l r e) = fence Open l ++ p_expr e ++ fence Close r
p_expr (Font Bold e)  = bold $ p_expr e
p_expr (Font Emph e)  = "<em>" ++ p_expr e ++ "</em>" -- FIXME
p_expr (Spc Thin)     = "&#8239;"
p_expr (Sqrt e)       = "&radic;(" ++ p_expr e ++")"

p_ops :: Ops -> String
p_ops IsIn     = "&thinsp;&isin;&thinsp;"
p_ops Integer  = "&#8484;"
p_ops Rational = "&#8474;"
p_ops Real     = "&#8477;"
p_ops Natural  = "&#8469;"
p_ops Boolean  = "&#120121;"
p_ops Comma    = ","
p_ops Prime    = "&prime;"
p_ops Log      = "log"
p_ops Ln       = "ln"
p_ops Sin      = "sin"
p_ops Cos      = "cos"
p_ops Tan      = "tan"
p_ops Sec      = "sec"
p_ops Csc      = "csc"
p_ops Cot      = "cot"
p_ops Not      = "&not;"
p_ops Dim      = "dim"
p_ops Exp      = "e"
p_ops Neg      = "&minus;"
p_ops Cross    = "&#10799;"
p_ops Dot      = "&sdot;"
p_ops Eq       = " = " -- with spaces?
p_ops NEq      = "&ne;"
p_ops Lt       = "&thinsp;&lt;&thinsp;" --thin spaces make these more readable
p_ops Gt       = "&thinsp;&gt;&thinsp;"
p_ops LEq      = "&thinsp;&le;&thinsp;"
p_ops GEq      = "&thinsp;&ge;&thinsp;"
p_ops Impl     = " &rArr; "
p_ops Iff      = " &hArr; "
p_ops Subt     = "&minus;"
p_ops And      = " &and; "
p_ops Or       = " &or; "
p_ops Add      = "&plus;"
p_ops Mul      = "&#8239;"
p_ops Summ     = "&sum;"
p_ops Inte     = "&int;"
p_ops Prod     = "&prod;"
p_ops Point    = "."

fence :: OpenClose -> Fence -> String
fence Open  Paren = "("
fence Close Paren = ")"
fence Open  Curly = "{"
fence Close Curly = "}"
fence _     Abs   = "|"
fence _     Norm  = "||"

-- | For printing Matrix
p_matrix :: [[Expr]] -> String
p_matrix [] = ""
p_matrix [x] = "<tr>" ++ p_in x ++ "</tr>\n"
p_matrix (x:xs) = p_matrix [x] ++ p_matrix xs

p_in :: [Expr] -> String
p_in [] = ""
p_in [x] = "<td>" ++ p_expr x ++ "</td>"
p_in (x:xs) = p_in [x] ++ p_in xs

-----------------------------------------------------------------
------------------BEGIN TABLE PRINTING---------------------------
-----------------------------------------------------------------

-- | Renders HTML table, called by 'printLO'
makeTable :: HasOptions s => s -> Tags -> [[Spec]] -> Doc -> Bool -> Doc -> Doc
makeTable _ _ [] _ _ _       = error "No table to print (see PrintHTML)"
makeTable s ts (l:lls) r b t = refwrap r (wrap "table" ts (
    tr (makeHeaderCols s l) $$ makeRows s lls) $$ if b then caption t else empty)

-- | Helper for creating table rows
makeRows :: HasOptions s => s ->  [[Spec]] -> Doc
makeRows s = foldr ($$) empty . map (tr . makeColumns s)

makeColumns, makeHeaderCols :: HasOptions s => s -> [Spec] -> Doc
-- | Helper for creating table header row (each of the column header cells)
makeHeaderCols s = vcat . map (\a -> th $ p_spec a s)

-- | Helper for creating table columns
makeColumns s = vcat . map (\a -> td $ p_spec a s)

-----------------------------------------------------------------
------------------BEGIN DEFINITION PRINTING----------------------
-----------------------------------------------------------------

-- | Renders definition tables (Data, General, Theory, etc.)
makeDefn :: HasOptions s => s ->  L.DType -> [(String,[LayoutObj])] -> Doc -> Doc
makeDefn _ _ [] _  = error "L.Empty definition"
makeDefn s dt ps l = refwrap l $ wrap "table" [dtag dt] (makeDRows s ps)
  where dtag (L.General)  = "gdefn"
        dtag (L.Instance) = "idefn"
        dtag (L.TM)       = "tdefn"
        dtag (L.DD)       = "ddefn"

-- | Helper for making the definition table rows
makeDRows :: HasOptions s => s -> [(String,[LayoutObj])] -> Doc
makeDRows _ []         = error "No fields to create defn table"
makeDRows s ((f,d):[]) = tr (th (text f) $$ td (vcat $ map (printLO s) d))
makeDRows s ((f,d):ps) = tr (th (text f) $$ td (vcat $ map (printLO s) d)) $$ makeDRows s ps

-----------------------------------------------------------------
------------------BEGIN LIST PRINTING----------------------------
-----------------------------------------------------------------

-- | Renders lists
makeList :: HasOptions s => s -> ListType -> Doc -- FIXME: ref id's should be folded into the li
makeList s (Simple items) = div_tag ["list"] $
  vcat $ map (\(b,e,l) -> wrap "p" [] $ mlref s l $ p_spec b s <> text ": "
   <> p_item s e) items
makeList s (Desc items)   = div_tag ["list"] $
  vcat $ map (\(b,e,l) -> wrap "p" [] $ mlref s l $ wrap "b" [] $ p_spec b s
   <> text ": " <> p_item s e) items
makeList s (Ordered items) = wrap "ol" ["list"] (vcat $ map
  (wrap "li" [] . \(i,l) -> mlref s l $ p_item s i) items)
makeList s (Unordered items) = wrap "ul" ["list"] (vcat $ map
  (wrap "li" [] . \(i,l) -> mlref s l $ p_item s i) items)
makeList s (Definitions items) = wrap "ul" ["hide-list-style-no-indent"] $
  vcat $ map (\(b,e,l) -> wrap "li" [] $ mlref s l $ p_spec b s <> text " is the"
   <+> p_item s e) items

-- | Helper for setting up references
mlref :: HasOptions s => s -> Maybe Label -> Doc -> Doc
mlref s = maybe id $ \a -> refwrap $ p_spec a s

-- | Helper for rendering list items
p_item :: HasOptions s => s -> ItemType -> Doc
p_item sm (Flat s)     = p_spec s sm
p_item sm (Nested s l) = vcat [p_spec s sm, makeList sm l]

-----------------------------------------------------------------
------------------BEGIN FIGURE PRINTING--------------------------
-----------------------------------------------------------------
-- | Renders figures in HTML
makeFigure :: Doc -> Doc -> Doc -> L.MaxWidthPercent -> Doc
makeFigure r c f wp = refwrap r (image f c wp $$ caption c)

-- | Renders assumptions, requirements, likely changes
makeRefList :: Doc -> Doc -> Doc -> Doc
makeRefList a l i = wrap "li" [] (refwrap l (i <> text ": " <> a))

---------------------
--HTML bibliography--
---------------------
-- **THE MAIN FUNCTION**

makeBib :: HasOptions s => s -> BibRef -> Doc
makeBib s = wrap "ul" ["hide-list-style"] . vcat .
  map (\(x,(y,z)) -> makeRefList z y x) .
  zip [text $ sqbrac $ show x | x <- ([1..] :: [Int])] . map (renderCite s) 

--for when we add other things to reference like website, newspaper
renderCite :: HasOptions s => s -> Citation -> (Doc, Doc)
renderCite s (Cite e L.Book cfs) = (text e, renderF cfs (useStyleBk s) <> text " Print.")
renderCite s (Cite e L.Article cfs) = (text e, renderF cfs (useStyleArtcl s) <> text " Print.")
renderCite s (Cite e L.MThesis cfs) = (text e, renderF cfs (useStyleBk s) <> text " Print.")
renderCite s (Cite e L.PhDThesis cfs) = (text e, renderF cfs (useStyleBk s) <> text " Print.")
renderCite s (Cite e L.Misc cfs) = (text e, renderF cfs (useStyleBk s))
renderCite s (Cite e _ cfs) = (text e, renderF cfs (useStyleArtcl s)) --FIXME: Properly render these later.

renderF :: [CiteField] -> (StyleGuide -> (CiteField -> Doc)) -> Doc
renderF fields styl = hsep $ map (styl bibStyleH) (sortBy compCiteField fields)

compCiteField :: CiteField -> CiteField -> Ordering
compCiteField (Institution _) _ = LT
compCiteField _ (Institution _) = GT
compCiteField (Organization _) _ = LT
compCiteField _ (Organization _) = GT
compCiteField (Author     _) _ = LT
compCiteField _ (Author     _) = GT
compCiteField (Title      _) _ = LT
compCiteField _ (Title      _) = GT
compCiteField (Series     _) _ = LT
compCiteField _ (Series     _) = GT
compCiteField (BookTitle _) _  = LT
compCiteField _ (BookTitle _)  = GT
compCiteField (Editor     _) _ = LT
compCiteField _ (Editor     _) = GT
compCiteField (Journal    _) _ = LT
compCiteField _ (Journal    _) = GT
compCiteField (Volume     _) _ = LT
compCiteField _ (Volume     _) = GT
compCiteField (Number     _) _ = LT
compCiteField _ (Number     _) = GT
compCiteField (Edition    _) _ = LT
compCiteField _ (Edition    _) = GT
compCiteField (HowPublished (Verb _)) _ = LT
compCiteField _ (HowPublished (Verb _)) = GT
compCiteField (School     _) _ = LT
compCiteField _ (School     _) = GT
compCiteField (Address      _) _ = LT
compCiteField _ (Address      _) = GT
compCiteField (Publisher  _) _ = LT
compCiteField _ (Publisher  _) = GT
compCiteField (Month      _) _ = LT
compCiteField _ (Month      _) = GT
compCiteField (Year       _) _ = LT
compCiteField _ (Year       _) = GT
compCiteField (HowPublished (URL _)) _ = LT
compCiteField _ (HowPublished (URL _)) = GT
compCiteField (Chapter    _) _ = LT
compCiteField _ (Chapter    _) = GT
compCiteField (Pages      _) _ = LT
compCiteField _ (Pages      _) = GT
compCiteField (Note       _) _ = LT
compCiteField _ (Note       _) = GT
compCiteField (Type       _) _ = LT

-- Config helpers --
useStyleBk :: HasOptions s => s -> StyleGuide -> (CiteField -> Doc)
useStyleBk s MLA     = bookMLA s
useStyleBk s APA     = bookAPA s
useStyleBk s Chicago = bookChicago s

useStyleArtcl :: HasOptions s => s -> StyleGuide -> (CiteField -> Doc)
useStyleArtcl s MLA     = artclMLA s
useStyleArtcl s APA     = artclAPA s
useStyleArtcl s Chicago = artclChicago s

-- FIXME: move these show functions and use tags, combinators
bookMLA :: HasOptions s => s -> CiteField -> Doc
bookMLA sm (Address s)     = p_spec s sm <> text ":"
bookMLA _ (Edition    s)  = comm $ text $ show s ++ sufxer s ++ " ed."
bookMLA sm (Series     s)  = dot $ em $ p_spec s sm
bookMLA sm (Title      s)  = dot $ em $ p_spec s sm --If there is a series or collection, this should be in quotes, not italics
bookMLA _ (Volume     s)  = comm $ text $ "vol. " ++ show s
bookMLA sm (Publisher  s)  = comm $ p_spec s sm
bookMLA sm (Author     p)  = dot $ p_spec (rendPeople' p) sm
bookMLA _ (Year       y)  = dot $ text $ show y
--bookMLA (Date    d m y) = dot $ unwords [show d, show m, show y]
--bookMLA (URLdate d m y) = "Web. " ++ bookMLA (Date d m y) sm
bookMLA sm (BookTitle s)   = dot $ em $ p_spec s sm
bookMLA sm (Journal    s)  = comm $ em $ p_spec s sm
bookMLA _ (Pages      [n]) = dot $ text $ "p. " ++ show n
bookMLA _ (Pages  (a:b:[])) = dot $ text $ "pp. " ++ show a ++ "&ndash;" ++ show b
bookMLA _ (Pages _) = error "Page range specified is empty or has more than two items"
bookMLA sm (Note       s)    = p_spec s sm
bookMLA _ (Number      n)   = comm $ text $ ("no. " ++ show n)
bookMLA sm (School     s)    = comm $ p_spec s sm
--bookMLA (Thesis     t)  = comm $ show t
--bookMLA (URL        s)  = dot $ p_spec s
bookMLA sm (HowPublished (Verb s)) = comm $ p_spec s sm
bookMLA sm (HowPublished (URL s))  = dot $ p_spec s sm
bookMLA sm (Editor     p)  = comm $ text "Edited by " <> p_spec (foldlList (map (S . L.nameStr) p)) sm
bookMLA _ (Chapter _)       = text ""
bookMLA sm (Institution i)  = comm $ p_spec i sm
bookMLA sm (Organization i)  = comm $ p_spec i sm
bookMLA _ (Month m)         = comm $ text $ show m
bookMLA sm (Type t)          = comm $ p_spec t sm

bookAPA :: HasOptions s => s -> CiteField -> Doc --FIXME: year needs to come after author in L.APA
bookAPA s (Author   p) = p_spec (rendPeople L.rendPersLFM' p) s --L.APA uses initals rather than full name
bookAPA _ (Year     y) = dot $ text $ paren $ show y --L.APA puts "()" around the year
--bookAPA (Date _ _ y) = bookAPA (Year y) --L.APA doesn't care about the day or month
--bookAPA (URLdate d m y) = "Retrieved, " ++ (comm $ unwords [show d, show m, show y])
bookAPA _ (Pages     [n])  = dot $ text $ show n
bookAPA _ (Pages (a:b:[])) = dot $ text $ show a ++ "&ndash;" ++ show b
bookAPA _ (Pages _) = error "Page range specified is empty or has more than two items"
bookAPA s (Editor   p) = dot $ p_spec (foldlList $ map (S . L.nameStr) p) s <> text " (Ed.)"
bookAPA s i = bookMLA s i --Most items are rendered the same as L.MLA

bookChicago :: HasOptions s => s -> CiteField -> Doc
bookChicago s (Author   p) = p_spec (rendPeople L.rendPersLFM'' p) s --L.APA uses middle initals rather than full name
bookChicago s p@(Pages  _) = bookAPA s p
bookChicago s (Editor   p) = dot $ p_spec (foldlList $ map (S . L.nameStr) p) s <> (text $ toPlural p " ed")
bookChicago s i = bookMLA s i --Most items are rendered the same as L.MLA

-- for article renderings
artclMLA :: HasOptions s => s -> CiteField -> Doc
artclMLA sm (Title s) = doubleQuotes $ dot $ p_spec s sm
artclMLA s i         = bookMLA s i

artclAPA :: HasOptions s => s -> CiteField -> Doc
artclAPA sm (Title  s) = dot $ p_spec s sm
artclAPA _ (Volume n)  = em $ text $ show n
artclAPA _ (Number  n) = comm $ text $ paren $ show n
artclAPA s i           = bookAPA s i

artclChicago :: HasOptions s => s -> CiteField -> Doc
artclChicago s i@(Title    _)  = artclMLA s i 
artclChicago _ (Volume     n)  = comm $ text $ show n
artclChicago _ (Number      n)  = text $ "no. " ++ show n
artclChicago s i@(Year     _)   = bookAPA s i
--artclChicago i@(Date _ _ _) = bookAPA i
artclChicago s i  = bookChicago s i

-- PEOPLE RENDERING --
rendPeople :: (L.Person -> String) -> L.People -> Spec
rendPeople _ []  = S "N.a." -- "No authors given"
rendPeople f people = foldlList $ map (S . f) people --foldlList is in SentenceStructures.hs

rendPeople' :: L.People -> Spec
rendPeople' []  = S "N.a." -- "No authors given"
rendPeople' people = foldlList $ map (S . rendPers) (init people) ++  [S (rendPersL $ last people)]

foldlList :: [Spec] -> Spec
foldlList []    = EmptyS
foldlList [a,b] = a :+: S " and " :+: b
foldlList lst   = foldle1 (\a b -> a :+: S ", " :+: b) (\a b -> a :+: S ", and " :+: b) lst

foldle1 :: (a -> a -> a) -> (a -> a -> a) -> [a] -> a
foldle1 _ _ []       = error "foldle1 cannot be used with empty list"
foldle1 _ _ [x]      = x
foldle1 _ g [x,y]    = g x y
foldle1 f g (x:y:xs) = foldle1 f g ((f x y):xs)

-- LFM is Last, First Middle
rendPers :: L.Person -> String
rendPers = L.rendPersLFM

-- To render the last person's name
rendPersL :: L.Person -> String
rendPersL =
  (\n -> (if not (null n) && last n == '.' then init else id) n) . rendPers

--adds an 's' if there is more than one person in a list
toPlural :: L.People -> String -> String
toPlural (_:_) str = str ++ "s"
toPlural _     str = str
