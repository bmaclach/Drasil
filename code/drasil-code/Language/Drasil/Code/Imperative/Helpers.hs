module Language.Drasil.Code.Imperative.Helpers (
    blank,spc,oneTabbed,oneTab,vertical,verticalComma,verticalNewLine,
    angles,doubleQuoted,doubleQuotedText,capitalize,containsAll,
    makeLiteralNameValid,makeVarNameValid,makeClassNameValid,powerSet,
    hmap,himap,hicat,vicat,vibcat,vmap,vimap,vibmap, reduceLibs
) where

import Prelude hiding ((<>))
import Control.Monad (filterM)
import Data.Char (toUpper)
import Data.String.Utils (replace)
import Data.List (nub,intersperse)
import Text.PrettyPrint.HughesPJ (Doc, vcat, hcat, text, char, doubleQuotes, 
    (<>), ($+$), comma, punctuate, nest)

blank :: Doc
blank = text ""

spc :: Doc
spc = text " "

oneTabbed :: [Doc] -> Doc
oneTabbed = vcat . map oneTab

oneTab :: Doc -> Doc
oneTab = nest 4

vertical :: (a -> Doc) -> [a] -> Doc
vertical f = vcat . map f

verticalComma :: (a -> Doc) -> [a] -> Doc
verticalComma f = vcat . punctuate comma . map f

verticalNewLine :: (a -> Doc) -> [a] -> Doc
verticalNewLine f = vcat . punctuate (blank $+$ blank) . map f

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

doubleQuotedText :: String -> Doc
doubleQuotedText = doubleQuotes . text

doubleQuoted :: (a -> String) -> a -> Doc
doubleQuoted labeller = doubleQuotedText . labeller

capitalize :: String -> String
capitalize [] = error "capitalize called on an emptry String"
capitalize (c:cs) = (toUpper c):cs

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll = all . flip elem

makeLiteralNameValid :: String -> String
makeLiteralNameValid s =
    let chars = ["\\","\""]
    in foldl myLiteralNameReplace s chars

makeVarNameValid :: String -> String
makeVarNameValid s =
    let illegalChars = [
            "~","`","-","=","!","@","#","$","%","^","&","*","(",")","+",
            "[","]","\\",";","'",".","/","{","}","|",":","\"","<",">","?"," "]
    in foldl myVarNameReplace s illegalChars

makeClassNameValid :: String -> String
makeClassNameValid = makeVarNameValid

--http://community.livejournal.com/evan_tech/220036.html
powerSet :: Eq a => [a] -> [[a]]
powerSet = filterM (const [True, False]) . nub

hmap :: (a -> Doc) -> [a] -> Doc
hmap f = hcat . map f

himap :: Doc -> (a -> Doc) -> [a] -> Doc
himap c f = hcat . intersperse c . map f

hicat :: Doc -> [Doc] -> Doc
hicat c l = hcat $ intersperse c l

vicat :: Doc -> [Doc] -> Doc
vicat c = vcat . intersperse c

vibcat :: [Doc] -> Doc
vibcat = vicat blank

vmap :: (a -> Doc) -> [a] -> Doc
vmap f = vcat . map f

vimap :: Doc -> (a -> Doc) -> [a] -> Doc
vimap c f = vicat c . map f

vibmap :: (a -> Doc) -> [a] -> Doc
vibmap = vimap blank

reduceLibs :: [String] -> [String] -> [String]
reduceLibs libs modules = nub $ filter (`notElem` modules) libs 

--private
myLiteralNameReplace :: String -> String -> String
myLiteralNameReplace l old = replace old ("\\" ++ old) l

myVarNameReplace :: String -> String -> String
myVarNameReplace l old = replace old "_" l
----------
    