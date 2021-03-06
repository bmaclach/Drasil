-- | The logic to render Python code from an 'AbstractCode' is contained in this module
module GOOL.CodeGeneration.LanguageRenderer.PythonRenderer (
    -- * Python Code Configuration -- defines syntax of all Python code
    pythonConfig
) where

import GOOL.Code (Code(..))
import GOOL.CodeGeneration.AbstractCode hiding (comment,bool,int,float,char)
import GOOL.CodeGeneration.LanguageRenderer
import GOOL.Auxil.Printing (angles,blank,oneTab,oneTabbed,doubleQuotedText)
import GOOL.Auxil.DataTypes (Label,Options)
import GOOL.Auxil.Helper (vmap,vimap)

import Data.List (intersperse)
import Prelude hiding (print)
import Text.PrettyPrint.HughesPJ

pythonConfig :: Options -> Config -> Config
pythonConfig options c = 
    Config {
        renderCode = renderCode' c,
        
        argsList         = text "sys.argv",
        bitArray         = empty,
        commentStart     = text "#",
        endStatement     = empty,
        enumsEqualInts   = True,
        ext              = ".py",
        fileName         = \p _ -> p,
        include          = include',
        includeScope     = \_ -> empty,
        inherit          = empty,
        inputFunc        = text "raw_input()",    --change to "input()" for Python 3.0+
        iterForEachLabel = forLabel,
        iterInLabel      = text "in",
        list             = \_ -> empty,
        listObj          = empty,
        clsDec           = classDec,
        package          = \_ -> empty,
        printFunc        = text "sys.stdout.write",
        printLnFunc      = text "print",
        stateType        = pystateType c,
        
        blockStart = colon, blockEnd = empty,
        ifBodyStart = blockStart c, elseIf = text "elif",
        
        top    = pytop c,
        body   = pybody c,
        bottom = \_ -> empty,
        
        assignDoc = assignDocD' c, binOpDoc = binOpDoc', bodyDoc = bodyDocD c, blockDoc = blockDocD c, callFuncParamList = callFuncParamListD c,
        conditionalDoc = conditionalDocD' c, declarationDoc = declarationDoc' c, enumElementsDoc = enumElementsDoc' c, exceptionDoc = exceptionDoc' c, exprDoc = exprDoc' c, funcAppDoc = funcAppDocD c,
        funcDoc = funcDoc' c, iterationDoc = iterationDoc' c, litDoc = litDoc',
        clsDecDoc = clsDecDocD c, clsDecListDoc = clsDecListDocD c, classDoc = classDoc' c, objAccessDoc = objAccessDoc' c,
        objVarDoc = objVarDoc' c, paramDoc = paramDoc' c, paramListDoc = paramListDocD c, patternDoc = patternDocD c, printDoc = printDoc' c, retDoc = retDocD c, scopeDoc = \_ -> empty,
        stateDoc = stateDocD c, stateListDoc = stateListDocD c, statementDoc = statementDocD c, methodDoc = methodDoc' c,
        methodListDoc = methodListDocD c, methodTypeDoc = methodTypeDocD c, unOpDoc = unOpDocD', valueDoc = valueDoc' c
    }

-- convenience
imp, incl, initName :: Label
imp = "import*"
incl = "from"
initName = "__init__"

-- short names, packaged up above (and used below)
renderCode' :: Config -> [Label] -> AbstractCode -> Code
renderCode' c ms (AbsCode p) = Code [fileCode c p ms Source (ext c)]

include' :: Label -> Doc
include' n = text incl <+> text n <+> text imp

pystateType :: Config -> StateType -> DecDef -> Doc
pystateType _ (List _ _) _ = brackets (empty)
pystateType c s@(Base Integer) d = stateTypeD c s d
pystateType c s@(Base Float) d = stateTypeD c s d
pystateType c s@(Base String) d = text "str"
pystateType _ (Base _) _ = empty
pystateType c s d = stateTypeD c s d

pytop :: Config -> a -> b -> Doc
pytop c _ _ = vcat [
    text "import sys",
    text "import math"]

pybody :: Config -> FileType -> Label -> [Class] -> Doc
pybody c f p ms = vcat $ intersperse blank (map (classDoc c f p) (fixCtorNames initName ms))

-- code doc functions
binOpDoc' :: BinaryOp -> Doc
binOpDoc' Power = text "**"
binOpDoc' op = binOpDocD op

declarationDoc' :: Config -> Declaration -> Doc
declarationDoc' _ (VarDec _ _) = empty
declarationDoc' c (ListDec lt n t s) =  text n <+> equals <+> stateType c (List lt t) Dec
declarationDoc' c (ListDecValues _ n t vs) = text n <+> equals <+> brackets (callFuncParamList c vs)
declarationDoc' c (VarDecDef n t v) = text n <+> equals <+> valueDoc c v
declarationDoc' c (ConstDecDef n l) = declarationDoc c $ VarDecDef n (Base $ typeOfLit l) (Lit l)
declarationDoc' c d = declarationDocD c d

enumElementsDoc' :: Config -> [Label] -> Doc
enumElementsDoc' c es = vcat $
    zipWith (\e i -> text e <+> equals <+> int i) es nums
    where nums = [0..length es - 1]
    
exceptionDoc' :: Config -> Exception -> Doc
exceptionDoc' c (Throw s) = text "raise" <+> text "Exception" <> parens (litDoc c $ LitStr s)
exceptionDoc' c (TryCatch tryBody catchBody) = vcat [
    text "try" <+> colon,
    oneTab $ bodyDoc c tryBody,
    text "except" <+> text "Exception" <+> text "as" <+> text "exc" <+> colon,
    oneTab $ bodyDoc c catchBody]

exprDoc' :: Config -> Expression -> Doc
exprDoc' c (Exists (ObjAccess v (ListAccess i))) = exprDoc c $ BinaryExpr (v $. ListSize) Greater i
exprDoc' c e@(Exists (Arg _)) = exprDocD c e
exprDoc' c (Exists v) = exprDoc c $ BinaryExpr v NotEqual $ Var "None"
exprDoc' c e = exprDocD c e

funcDoc' :: Config -> Function -> Doc
funcDoc' c (Cast t) = stateType c t Def
funcDoc' c (Get n) = dot <> text n
funcDoc' c (Set n v) = dot <> text n <+> equals <+> valueDoc c v
funcDoc' c (IndexOf v) = dot <> funcAppDoc c "index" [v]
funcDoc' c ListSize = text "len"
funcDoc' c (ListAccess i) = brackets $ valueDoc c i
funcDoc' c (ListAdd i v) = dot <> funcAppDoc c "insert" [i, v]
funcDoc' c (ListPopulate size t) = brackets (valueDoc c dftVal) <+> char '*' <+> valueDoc c size
    where dftVal = case t of Base bt   -> defaultValue bt
                             otherwise -> error $ "ListPopulate does not yet support list type " ++ render (doubleQuotes $ stateType c t Def)
funcDoc' c f = funcDocD c f

iterationDoc' :: Config -> Iteration -> Doc
iterationDoc' c (For (DeclState (VarDecDef i (Base Integer) initv)) (Expr (BinaryExpr _ Less finalv)) (AssignState (PlusPlus _)) b) = 
    vcat [
        forLabel <+> text i <+> (iterInLabel c) <+> text "range" <> parens (valueDoc c finalv) <> colon,
        oneTab $ bodyDoc c b]
iterationDoc' c (For initv guard update b) = vcat [
    forLabel <+> statementDoc c Loop initv <> semi <+> valueDoc c guard <> semi <+> statementDoc c Loop update,
    oneTab $ bodyDoc c b]
iterationDoc' c (ForEach i listVar@(ListVar _ _) b) = vcat [
    (iterForEachLabel c) <+> valueDoc c (Var i) <+> (iterInLabel c) <+> valueDoc c listVar <> colon,
    oneTab $ bodyDoc c b]
iterationDoc' c i = iterationDocD c i

litDoc' :: Literal -> Doc
litDoc' (LitBool True) = text "True"
litDoc' (LitBool False) = text "False"
litDoc' l = litDocD l

classDoc' :: Config -> FileType -> Label -> Class -> Doc
classDoc' c f _ (MainClass _ _ fs) = methodListDoc c f "" fs
classDoc' c f _ m = vcat [
    clsDec c <+> text (className m) <> baseClass <> colon,
    oneTab $ modInnerDoc]
    where modInnerDoc = case m of (Class n _ _ _ fs) -> methodListDoc c f n fs
                                  (Enum _ _ es) -> enumElementsDoc c es
          baseClass = case m of (Class _ p _ _ _) -> case p of Nothing -> empty
                                                               Just pn -> parens (text pn)
                                otherwise -> empty

objAccessDoc' :: Config -> Value -> Function -> Doc
objAccessDoc' c v@(Self) f = valueDoc c v <> funcDoc c f
objAccessDoc' c v f@(ListSize) = funcDoc c f <> parens (valueDoc c v)
objAccessDoc' c v f@(ListPopulate _ _) = valueDoc c v <+> equals <+> funcDoc c f
objAccessDoc' c v f@(Floor) = funcAppDoc c "math.floor" [v]
objAccessDoc' c v f@(Ceiling) = funcAppDoc c "math.ceil" [v]
objAccessDoc' c v f = objAccessDocD c v f

objVarDoc' :: Config -> Value -> Value -> Doc
objVarDoc' c v1 v2 = valueDoc c v1 <> dot <> valueDoc c v2

paramDoc' :: Config -> Parameter -> Doc
paramDoc' _ (StateParam n _) = text n
paramDoc' c p = paramDocD c p

printDoc' :: Config -> Bool -> StateType -> Value -> Doc
printDoc' c False _ v = printFunc c <> parens (valueDoc c $ v $. Cast string)
printDoc' c True _ v = printLnFunc c <> parens (valueDoc c v)

methodDoc' :: Config -> FileType -> Label -> Method -> Doc
methodDoc' c _ _ (Method n _ (Construct _) ps b) = vcat [
    text "def" <+> text n <> parens (valueDoc c Self <> oneParam <> paramListDoc c ps) <> colon,
    oneTab $ bodyDoc c b]
        where oneParam | length ps > 0 = text ", "
                       | otherwise     = empty
methodDoc' c _ _ (Method n s t ps b) = vcat [
    text "def" <+> text n <> parens (valueDoc c Self <> oneParam <> paramListDoc c ps) <> colon,
    oneTab body]
        where oneParam | length ps > 0 = text ", "
                       | otherwise     = empty
              body | null b    = text "None"
                   | otherwise = bodyDoc c b
methodDoc' c _ _ (MainMethod b) = bodyDoc c b
methodDoc' _ _ _ _ = empty

valueDoc' :: Config -> Value -> Doc
valueDoc' _ (Self) = text "self"
valueDoc' c (StateObj t@(List _ _) _) = stateType c t Def
valueDoc' c (StateObj t vs) = stateType c t Def <> parens (callFuncParamList c vs)
valueDoc' c v@(Arg _) = valueDocD' c v
valueDoc' c v = valueDocD c v
