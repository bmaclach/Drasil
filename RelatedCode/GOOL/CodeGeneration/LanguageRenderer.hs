-- | The structure for a class of renderers is defined here.
module GOOL.CodeGeneration.LanguageRenderer (
    -- * Some Neccessary Data Types for Rendering
    StatementLocation(..), DecDef(..), FileType(..),

    -- * Generation Language Structuring
    Config(..),

    -- * Language Parametric Functions
    fileCode,
    
    -- * Common Syntax
    classDec, dot, doubleSlash, forLabel, new,
    
    -- * Default Functions available for use in renderers
    assignDocD,assignDocD',binOpDocD,bodyDocD,blockDocD,callFuncParamListD,conditionalDocD,conditionalDocD',conditionalDocD'',declarationDocD,declarationDocD',
    enumElementsDocD,exceptionDocD,exprDocD,exprDocD',exprDocD'',funcAppDocD,funcDocD,includeD,iterationDocD,litDocD,
    clsDecDocD,clsDecListDocD,classDocD,namespaceD,objAccessDocD,objVarDocD,
    paramDocD,paramListDocD,patternDocD,printDocD,retDocD,scopeDocD,stateDocD,stateListDocD,
    statementDocD,stateTypeD,methodDocD,methodDocD',methodListDocD,methodTypeDocD,unOpDocD,unOpDocD',valueDocD,valueDocD',
    
    -- * Helper Functions
    addDefaultCtor, comment, end, fixCtorNames, genNameFromType, jump, litsToValues, clsWithName, typeOfLit
) where

import GOOL.Code (Code(..))
import GOOL.CodeGeneration.AbstractCode hiding (comment,bool,int,float,char,string)
import GOOL.Auxil.DataTypes (Label)
import GOOL.Auxil.Printing (angles,blank,doubleQuotedText,oneTab,oneTabbed)
import GOOL.Auxil.Helper (himap,vibcat,vmap,vibmap)

import qualified Data.Map as Map (fromList,lookup)
import Data.List (find)
import Prelude hiding (break,print,return)
import Text.PrettyPrint.HughesPJ

data StatementLocation = Loop | NoLoop
data DecDef = Dec | Def
data FileType = Header | Source

-- | Configuration record (explicit dictionary) for a language
data Config = Config {
    renderCode :: [Label] -> AbstractCode -> Code,
    
    argsList :: Doc,
    bitArray :: Doc,
    commentStart :: Doc,
    endStatement :: Doc,
    enumsEqualInts :: Bool,     --whether Enum elements should explictly be set equal to their ordinal integers (in the default enumElementsDoc implementation)
    ext :: Label,
    fileName :: Label -> [Label] -> String,
    include :: Label -> Doc,
    includeScope :: Scope -> Doc,
    inherit :: Doc,
    inputFunc :: Doc,
    iterForEachLabel :: Doc,
    iterInLabel :: Doc,
    list :: Permanence -> Doc,
    listObj :: Doc,
    clsDec :: Doc,
    package :: Label -> Doc,
    printFunc :: Doc,
    printLnFunc :: Doc,
    stateType :: StateType -> DecDef -> Doc,
    
    blockStart :: Doc, blockEnd :: Doc,
    ifBodyStart :: Doc, elseIf :: Doc,
    
    top :: FileType -> Label -> Doc,
    body :: FileType -> Label -> [Class] -> Doc,
    bottom :: FileType -> Doc,
    
    assignDoc :: Assignment -> Doc,
    binOpDoc :: BinaryOp -> Doc,
    bodyDoc :: Body -> Doc,
    blockDoc :: Block -> Doc,
    callFuncParamList :: [Value] -> Doc,
    conditionalDoc :: Conditional -> Doc,
    declarationDoc :: Declaration -> Doc,
    enumElementsDoc :: [Label] -> Doc,
    exceptionDoc :: Exception -> Doc,
    exprDoc :: Expression -> Doc,
    funcAppDoc :: Label -> [Value] -> Doc,
    funcDoc :: Function -> Doc,
    iterationDoc :: Iteration -> Doc,
    litDoc :: Literal -> Doc,
    clsDecDoc :: Class -> Doc,
    clsDecListDoc :: [Class] -> Doc,
    classDoc :: FileType -> Label -> Class -> Doc,
    objAccessDoc :: Value -> Function -> Doc,
    objVarDoc :: Value -> Value -> Doc,
    paramDoc :: Parameter -> Doc,
    paramListDoc :: [Parameter] -> Doc,
    patternDoc :: Pattern -> Doc,
    printDoc :: Bool -> StateType -> Value -> Doc,
    retDoc :: Return -> Doc,
    scopeDoc :: Scope -> Doc,
    stateDoc :: StateVar -> Doc,
    stateListDoc :: [StateVar] -> Doc,
    statementDoc :: StatementLocation -> Statement -> Doc,
    methodDoc :: FileType -> Label -> Method -> Doc,
    methodListDoc :: FileType -> Label -> [Method] -> Doc,
    methodTypeDoc :: MethodType -> Doc,
    unOpDoc :: UnaryOp -> Doc,
    valueDoc :: Value -> Doc,
    getEnv :: String -> Doc -- careful, this can fail!
}

----------------------------------
-- CFamily Parametric Functions --
----------------------------------

fileCode :: Config -> Package -> [Label] -> FileType -> Label -> (FilePath, Doc)
fileCode c (Pack p ms _) ns f e = (fileName c p ns ++ e, fileDoc c f p $ map (clsWithName ms) ns)

fileDoc :: Config -> FileType -> Label -> [Class] -> Doc
fileDoc c f p ms = vibcat [
    top c f p,
    body c f p ms,
    bottom c f]

----------------------------------------
-- Syntax common to several renderers --
----------------------------------------

classDec,dot,doubleSlash,forLabel,new :: Doc
classDec = text "class"
dot = text "."
doubleSlash = text "//"
forLabel = text "for"
new = text "new"

---------------------------------
-- Var names required globally --
---------------------------------
observerListName :: Label
observerListName = "observerList"

-----------------------------------------------
-- 'Default' functions used in the renderers --
-----------------------------------------------

assignDocD :: Config -> Assignment -> Doc
assignDocD c (Assign v1 v2) = valueDoc c v1 <+> equals <+> valueDoc c v2
assignDocD c (PlusEquals v1 v2) = valueDoc c v1 <+> text "+=" <+> valueDoc c v2
assignDocD c (PlusPlus v) = valueDoc c v  <> text "++"

assignDocD' :: Config -> Assignment -> Doc
assignDocD' c (PlusEquals v1 v2) = valueDoc c v1 <+> equals <+> valueDoc c v1 <+> text "+" <+> valueDoc c v2
assignDocD' c (PlusPlus v) = valueDoc c v <+> equals <+> valueDoc c v <+> text "+" <+> int 1
assignDocD' c a = assignDocD c a

binOpDocD :: BinaryOp -> Doc
binOpDocD Equal = text "=="
binOpDocD NotEqual = text "!="
binOpDocD Greater = text ">"
binOpDocD GreaterEqual = text ">="
binOpDocD Less = text "<"
binOpDocD LessEqual = text "<="
binOpDocD Plus = text "+"
binOpDocD Minus = text "-"
binOpDocD Divide = text "/"
binOpDocD Multiply = text "*"
binOpDocD Modulo = text "%"
binOpDocD Power = text "pow"

bodyDocD :: Config -> Body -> Doc
bodyDocD c bs = vibmap (blockDoc c) blocks
    where blocks = filter (\b -> not $ isEmpty $ blockDoc c b) bs

blockDocD :: Config -> Block -> Doc
blockDocD c (Block ss) = vmap (statementDoc c NoLoop) statements
    where docOf s = statementDoc c NoLoop s
          notNullStatement s = (not $ isEmpty $ docOf s) && (render (docOf s) /= render (end c NoLoop))
          statements = filter notNullStatement ss
    
callFuncParamListD :: Config -> [Value] -> Doc
callFuncParamListD c vs = himap (text ", ") (valueDoc c) vs

conditionalDocD :: Config -> Conditional -> Doc
conditionalDocD _ (If [] _) = error "If with no body encountered"
conditionalDocD c (If (t:ts) elseBody) =
    let ifSect (v, b) = vcat [
            text "if" <+> parens (valueDoc c v) <+> ifBodyStart c,
            oneTab $ bodyDoc c b,
            blockEnd c]
        elseIfSect (v, b) = vcat [
            elseIf c <+> parens (valueDoc c v) <+> ifBodyStart c,
            oneTab $ bodyDoc c b,
            blockEnd c]
        elseSect = if null elseBody then empty else vcat [
            text "else" <+> ifBodyStart c,
            oneTab $ bodyDoc c elseBody,
            blockEnd c]
    in vcat [
        ifSect t,
        vmap elseIfSect ts,
        elseSect]
conditionalDocD _ (Switch _ [] _) = error "Switch with no cases encountered"
conditionalDocD c (Switch v cs defBody) =
    let breakState = statementDoc c NoLoop break
        caseDoc (l, result) = vcat [
            text "case" <+> litDoc c l <> colon,
            oneTabbed [
                bodyDoc c result,
                breakState]]
        defaultSection = vcat [
            text "default" <> colon,
            oneTabbed [
                bodyDoc c defBody,
                breakState]]
    in vcat [
        text "switch" <> parens (valueDoc c v) <+> lbrace,
        oneTabbed [
            vmap caseDoc cs,
            defaultSection],
        rbrace]

conditionalDocD' :: Config -> Conditional -> Doc
conditionalDocD' c cnd@(Switch _ [] _) = conditionalDocD c cnd
conditionalDocD' c (Switch v cs defBody) = conditionalDoc c $ If ifBranches defBody
    where ifBranches = map (\(l,b) -> (v ?== (Lit l), b)) cs
conditionalDocD' c cnd = conditionalDocD c cnd

conditionalDocD'' :: Config -> Conditional -> Doc
conditionalDocD'' c (Switch v cs@((LitStr _, _):_) defBody) = conditionalDoc c $ If cases defBody
    where cases = map (\(l, b) -> (v ?== Lit l, b)) cs
conditionalDocD'' c cond = conditionalDocD c cond

declarationDocD :: Config -> Declaration -> Doc
declarationDocD c (VarDec n t) = stateType c t Dec <+> text n
declarationDocD c (ListDec lt n t s) = stateType c (List lt t) Dec <+> text n <+> equals <+> new <+> stateType c (List lt t) Dec <> parens (int s)
declarationDocD c (ListDecValues lt n t vs) = stateType c (List lt t) Dec <+> text n <+> equals <+> new <+> stateType c (List lt t) Dec <+> braces (callFuncParamList c vs)
declarationDocD c (VarDecDef n t v) = stateType c t Dec <+> text n <+> equals <+> valueDoc c v
declarationDocD c (ObjDecDef n t v) = declarationDoc c $ VarDecDef n t v
declarationDocD c (ConstDecDef n l) = text "const" <+> stateType c (Base $ typeOfLit l) Dec <+> text n <+> equals <+> litDoc c l

declarationDocD' :: Config -> Declaration -> Doc
declarationDocD' c (VarDecDef n t Input) = vcat [
    declarationDoc c (VarDec n t) <> endStatement c,
    assignDoc c $ Assign (Var n) Input]
declarationDocD' c d = declarationDocD c d

enumElementsDocD :: Config -> [Label] -> Doc
enumElementsDocD c es = vcat $
    zipWith (\e i -> text e <+> equalsInt i <> interComma i) es nums
    where nums = [0..length es - 1]
          equalsInt i = if enumsEqualInts c then (equals <+> int i) else empty 
          interComma i = if i < length es - 1 then text "," else empty

exceptionDocD :: Config -> Exception -> Doc
exceptionDocD c (Throw s) = text "throw new" <+> text "System.ApplicationException" <> parens (litDoc c $ LitStr s)
exceptionDocD c (TryCatch tryBody catchBody) = vcat [
    text "try" <+> lbrace,
    oneTab $ bodyDoc c tryBody,
    rbrace <+> text "catch" <+> parens (text "System.Exception" <+> text "exc") <+> lbrace,
    oneTab $ bodyDoc c catchBody,
    rbrace]

exprDocD :: Config -> Expression -> Doc
exprDocD c (UnaryExpr r v) = unOpDoc c r <> parens (valueDoc c v)
exprDocD c (BinaryExpr v1 op v2) = val1 <+> binOpDoc c op <+> val2
    where val1 = case v1 of (Expr _)  -> parens (valueDoc c v1)
                            _ -> valueDoc c v1
          val2 = case v2 of (Expr _)  -> parens (valueDoc c v2)
                            _ -> valueDoc c v2
exprDocD c (Exists v) = valueDoc c v

exprDocD' :: Config -> Expression -> Doc
exprDocD' c (BinaryExpr v1 Power v2) = binOpDoc c Power <> parens (valueDoc c v1 <> comma <+> valueDoc c v2)
exprDocD' c e = exprDocD c e

exprDocD'' :: Config -> Expression -> Doc
exprDocD'' c (Exists (ObjAccess v (ListAccess i))) = exprDoc c $ BinaryExpr (Var $ render (valueDoc c v) ++ ".Length") Greater i
exprDocD'' c (Exists (Arg i)) = exprDoc c $ Exists $ ObjAccess (Var $ render $ argsList c) $ ListAccess $ litInt (fromIntegral i)
exprDocD'' c (Exists v) = exprDoc c $ BinaryExpr v NotEqual $ Var "null"
exprDocD'' c e = exprDocD' c e

funcAppDocD :: Config -> Label -> [Value] -> Doc
funcAppDocD c n vs = text n <> parens (callFuncParamList c vs)

funcDocD :: Config -> Function -> Doc
funcDocD c (Func n vs) = dot <> funcAppDoc c n vs
funcDocD c (Cast t) = parens (stateType c t Dec)
funcDocD c (Get n) = dot <> funcAppDoc c (getterName n) []
funcDocD c (Set n v) = dot <> funcAppDoc c (setterName n) [v]
funcDocD c (IndexOf v) = dot <> funcAppDoc c "IndexOf" [v]
funcDocD _ ListSize = dot <> text "Count"
funcDocD c (ListAccess v@(EnumVar _)) = funcDoc c $ ListAccess (v $. cast Integer)
funcDocD c (ListAccess v@(EnumElement _ _)) = funcDoc c $ ListAccess (v $. cast Integer)
funcDocD c (ListAccess v@(ObjAccess (ListVar _ (EnumType _)) (ListAccess _))) = funcDoc c $ ListAccess (v $. cast Integer)
funcDocD c (ListAccess i) = brackets $ valueDoc c i
funcDocD c (ListAdd i v) = dot <> funcAppDoc c "Insert" [i, v]
funcDocD c (ListSet i@(EnumVar _) v) = funcDoc c $ ListSet (i $. cast Integer) v
funcDocD c (ListSet i@(EnumElement _ _) v) = funcDoc c $ ListSet (i $. cast Integer) v
funcDocD c (ListSet i v) = brackets (valueDoc c i) <+> equals <+> valueDoc c v
funcDocD c (ListPopulate _ _) = empty
funcDocD c (IterBegin) = dot <> funcAppDoc c "begin" []
funcDocD c (IterEnd) = dot <> funcAppDoc c "end" []

includeD :: Label -> Label -> Doc
includeD incl n = text incl <+> text n

iterationDocD :: Config -> Iteration -> Doc
iterationDocD c (For initv guard update b) = vcat [
    forLabel <+> parens (statementDoc c Loop initv <> semi <+> valueDoc c guard <> semi <+> statementDoc c Loop update) <+> blockStart c,
    oneTab $ bodyDoc c b,
    blockEnd c]
iterationDocD c (ForEach var listVar@(ListVar _ t) b) = vcat [
    iterForEachLabel c <+> parens (stateType c t Dec <+> text var <+> iterInLabel c <+> valueDoc c listVar) <+> blockStart c,
    oneTab $ bodyDoc c b,
    blockEnd c]
iterationDocD c (ForEach _ val _) = error $ "Value in ForEach statement (" ++ render (valueDoc c val) ++ ") must be a ListVar"
iterationDocD c (While v b) = vcat [
    text "while" <+> parens (valueDoc c v) <+> blockStart c,
    oneTab $ bodyDoc c b,
    blockEnd c]

litDocD :: Literal -> Doc
litDocD (LitBool True) = text "true"
litDocD (LitBool False) = text "false"
litDocD (LitInt v) = integer v
litDocD (LitFloat v) = double v
litDocD (LitChar v) = quotes $ char v
litDocD (LitStr v) = doubleQuotedText v

clsDecDocD :: Config -> Class -> Doc
clsDecDocD c (Class n _ _ _ _) = (clsDec c) <+> text n <> endStatement c
clsDecDocD c (Enum _ _ _) = empty
clsDecDocD c m@(MainClass _ _ _) = clsDecDoc c $ convertToClass m

clsDecListDocD :: Config -> [Class] -> Doc
clsDecListDocD c = vmap $ clsDecDoc c
    
classDocD :: Config -> FileType -> Label -> Class -> Doc
classDocD c _ _ (Enum n s es) = vcat [
    scopeDoc c s <+> text "enum" <+> text n <+> lbrace,
    oneTab $ enumElementsDoc c es,
    rbrace]
classDocD c ft _ (Class n p s vs fs) = vcat [
    scopeDoc c s <+> clsDec c <+> text n <+> baseClass <+> lbrace,
    oneTabbed [
        stateListDoc c vs,
        blank,
        methodListDoc c ft n fs],
    rbrace]
    where baseClass = case p of Nothing -> empty
                                Just pn -> inherit c <+> text pn
classDocD c ft m mod@(MainClass _ _ _) = classDoc c ft m $ convertToClass mod

-- for 'packages' which are namespaces
namespaceD :: Label -> Doc
namespaceD n = text "namespace" <+> text n

objAccessDocD :: Config -> Value -> Function -> Doc
objAccessDocD c (Self) (Func n vs) = funcAppDoc c n vs
objAccessDocD _ _ (ListPopulate _ _) = empty
objAccessDocD c v f@(Cast _) = funcDoc c f <> parens (valueDoc c v)
objAccessDocD c v f@(Floor) = funcAppDoc c "Math.Floor" [v]
objAccessDocD c v f@(Ceiling) = funcAppDoc c "Math.Ceiling" [v]
objAccessDocD c v f = valueDoc c v <> funcDoc c f

objVarDocD :: Config -> Value -> Value -> Doc
objVarDocD c (Self) v = valueDoc c v
objVarDocD c v1 v2 = valueDoc c v1 <> dot <> valueDoc c v2

paramDocD :: Config -> Parameter -> Doc
paramDocD c (StateParam n t) = stateType c t Dec <+> text n
paramDocD _ (FuncParam _ _ _) = error "FuncParam not yet rendered"

paramListDocD :: Config -> [Parameter] -> Doc
paramListDocD c ps = himap (text ", ") (paramDoc c) ps

patternDocD :: Config -> Pattern -> Doc
patternDocD c (State (InitState n s)) = declarationDoc c $ VarDecDef n (Base String) (litString s)
patternDocD c (State (ChangeState n s)) = assignDoc c $ Assign (Var n) $ litString s
patternDocD c (State (CheckState n [] _)) = error $ "FSM '" ++ n ++ "': CheckState called with empty case list."
patternDocD c (State (CheckState n cs defBody)) = conditionalDoc c $ Switch (Var n) cases defBody
    where cases = map (\(s, b) -> (LitStr s, b)) cs
patternDocD c (Strategy (RunStrategy n (Strats strats r) v)) =
    case Map.lookup n (Map.fromList strats) of Nothing -> error $ "Strategy '" ++ n ++ "': RunStrategy called on non-existent strategy."
                                               Just b  -> vcat [
                                                            bodyDoc c b,
                                                            resultState]
    where resultState = case v of Nothing  -> empty
                                  Just var -> case r of Nothing  -> error $ "Strategy '" ++ n ++ "': Attempt to assign null return to a Value."
                                                        Just res -> assignDoc c $ Assign var res
patternDocD c (Observer (InitObserverList t os)) = declarationDoc c $ ListDecValues Dynamic observerListName t os
patternDocD c (Observer (AddObserver t o)) = valueDoc c $ obsList $. ListAdd last o
    where obsList = observerListName `listOf` t
          last = obsList $. ListSize
patternDocD c (Observer (NotifyObservers t fn ps)) = iterationDoc c $ For initv (Var index ?< (obsList $. ListSize)) ((&++)index) notify
    where obsList = observerListName `listOf` t
          index = "observerIndex"
          initv = varDecDef index (Base Integer) $ litInt 0
          notify = oneLiner $ ValState $ (obsList $. at index) $. Func fn ps

printDocD :: Config -> Bool -> StateType -> Value -> Doc
printDocD c newLn _ v@(ListVar _ t) = vcat [
    statementDoc c NoLoop $ printStr "[",
    iterationDoc c $ ForEach e v [ Block [
        print t element,
        printStr "," ] ],
    statementDoc c Loop $ printLastStr "]"]
    where e = genNameFromType t
          printLastStr = if newLn then printStrLn else printStr
          element = case t of List _ st -> e `listOf` st
                              _         -> Var e
printDocD c newLn _ v = printFn <> parens (valueDoc c v)
    where printFn = if newLn then printLnFunc c else printFunc c

retDocD :: Config -> Return -> Doc
retDocD c (Ret v) = text "return" <+> valueDoc c v

scopeDocD :: Scope -> Doc
scopeDocD Private = text "private"
scopeDocD Public = text "public"

permanence :: Permanence -> Doc
permanence Static = text "static"
permanence Dynamic = empty

stateDocD :: Config -> StateVar -> Doc
stateDocD c (StateVar n s p t _) = includeScope c s <+> permanence p <+> stateType c t Dec <+> text n <> endStatement c

stateListDocD :: Config -> [StateVar] -> Doc
stateListDocD c = vmap $ stateDoc c

statementDocD :: Config -> StatementLocation -> Statement -> Doc
statementDocD c loc (AssignState s) = assignDoc c s <> end c loc
statementDocD c loc (DeclState s) = declarationDoc c s <> end c loc
statementDocD c _ (CondState s) = conditionalDoc c s
statementDocD c _ (IterState s) = iterationDoc c s
statementDocD c loc (JumpState s) = jump s <> end c loc
statementDocD c loc (RetState s) = retDoc c s <> end c loc
statementDocD c loc (ValState s) = valueDoc c s <> end c loc
statementDocD c _ (CommentState s) = comment c s
statementDocD c loc (FreeState v) = text "delete" <+> valueDoc c v <> end c loc
statementDocD c loc (PrintState newLn t v) = printDoc c newLn t v <> end c loc
statementDocD c loc (ExceptState e) = exceptionDoc c e <> end c loc
statementDocD c loc (PatternState p) = patternDoc c p <> end c loc

stateTypeD :: Config -> StateType -> DecDef -> Doc
stateTypeD c (List lt t@(List _ _)) _ = list c lt <> angles (space <> stateType c t Dec <> space)
stateTypeD c (List lt t) _            = case t of Base Boolean -> bitArray c
                                                  _    -> list c lt <> angles (stateType c t Dec)
stateTypeD _ (Base Boolean) _    = text "Boolean"
stateTypeD _ (Base Integer) _    = text "int"
stateTypeD _ (Base Float) _      = text "float"
stateTypeD _ (Base Character) _  = text "char"
stateTypeD _ (Base String) _     = text "string"
stateTypeD _ (Type name) _       = text name
stateTypeD _ (Iterator _) _      = empty
stateTypeD _ (EnumType enum) _   = text enum

methodDocD :: Config -> FileType -> Label -> Method -> Doc
methodDocD c _ _ (Method n s t ps b) = vcat [
    scopeDoc c s <+> methodTypeDoc c t <+> text n <> parens (paramListDoc c ps) <+> lbrace,
    oneTab $ bodyDoc c b,
    rbrace]
methodDocD c _ _ (MainMethod b) = vcat [
    scopeDoc c Public <+> text "static" <+> methodTypeDoc c Void <+> text "Main" <> parens (text "string[] args") <+> lbrace,
    oneTab $ bodyDoc c b,
    rbrace]
methodDocD c f m t = methodDoc c f m $ convertToMethod t

methodDocD' :: Config -> FileType -> Label -> Method -> Doc
methodDocD' c _ _ (MainMethod b) = vcat [
    stateType c (Base Integer) Dec <+> text "main" <> parens (text "int argc, const char *argv[]") <+> lbrace,
    oneTab $ bodyDoc c $ b ++ retBody,
    rbrace]
    where retBody = oneLiner $ return $ litInt 0
methodDocD' c ft m f = methodDocD c ft m f

methodListDocD :: Config -> FileType -> Label -> [Method] -> Doc
methodListDocD c t m ms = vibmap (methodDoc c t m) funcs
    where funcs = filter (\f -> not $ isEmpty $ methodDoc c t m f) ms

methodTypeDocD :: Config -> MethodType -> Doc
methodTypeDocD c (MState t) = stateType c t Dec
methodTypeDocD _ Void = text "void"
methodTypeDocD _ _ = empty

unOpDocD :: UnaryOp -> Doc
unOpDocD Negate = text "-"
unOpDocD SquareRoot = text "sqrt"
unOpDocD Abs = text "fabs"
unOpDocD Not = text "!"

unOpDocD' :: UnaryOp -> Doc
unOpDocD' SquareRoot = text "math.sqrt"
unOpDocD' Abs = text "math.fabs"
unOpDocD' Not = text "not"
unOpDocD' op = unOpDocD op

valueDocD :: Config -> Value -> Doc
valueDocD c (Const n) = valueDoc c $ Var n
valueDocD c (Lit v) = litDoc c v
valueDocD _ (EnumElement en e) = text en <> dot <> text e
valueDocD c (FuncApp n vs) = funcAppDoc c n vs
valueDocD c (ObjAccess v f) = objAccessDoc c v f
valueDocD c (Expr v) = exprDoc c v
valueDocD _ Self = text "this"
valueDocD c (StateObj t@(List _ _) vs) = listObj c <+> stateType c t Def <> parens (callFuncParamList c vs)
valueDocD c (StateObj t vs) = new <+> stateType c t Def <> parens (callFuncParamList c vs)
valueDocD _ (Var v) = text v
valueDocD c (EnumVar v) = valueDoc c $ Var v
valueDocD c (ListVar v _) = valueDoc c $ Var v
valueDocD c (ObjVar v1 v2) = objVarDoc c v1 v2
valueDocD c (Arg i) = argsList c <> brackets (litDoc c $ LitInt $ fromIntegral i)
valueDocD c Input = inputFunc c
valueDocD c (Global s) = getEnv c s

valueDocD' :: Config -> Value -> Doc
valueDocD' c (Arg i) = argsList c <> brackets (litDoc c $ LitInt $ fromIntegral i + 1)
valueDocD' c v = valueDocD c v

----------------------
-- Helper Functions --
----------------------

addDefaultCtor :: Config -> Label -> Label -> [Method] -> [Method]
addDefaultCtor _ modName ctorName fs =
    case find ctor fs of Nothing   -> Method ctorName Public (Construct modName) [] [] : fs
                         _ -> fs
    where ctor (Method _ _ (Construct _) _ _) = True
          ctor _ = False

comment :: Config -> Comment -> Doc
comment c (Comment cmt) = (commentStart c) <+> text cmt
comment c (CommentDelimit cmt len) =
    let com = (commentStart c) <> text (" " ++ cmt ++ " ")
    in com <> text (dashes (render com) len)

dashes :: String -> Int -> String
dashes s l = take (l - length s) (repeat '-')

end :: Config -> StatementLocation -> Doc
end _ Loop = empty
end c NoLoop = endStatement c

-- change the name of constructors in the given list of Transformations
fixCtorName :: Label -> [Method] -> [Method]
fixCtorName newName = map (fixTran newName)
  where fixTran nn (Method _ s (Construct m) p b) = Method nn s (Construct m) p b
        fixTran _ f = f

-- change the constructor name on all modules
fixCtorNames :: Label -> [Class] -> [Class]
fixCtorNames newName = map (fixNames newName)
  where fixNames nn (Class n p s ss fs) = Class n p s ss $ fixCtorName nn fs
        fixNames nn (MainClass n vs fs) = MainClass n vs $ fixCtorName nn fs
        fixNames _ m = m

genNameFromType :: StateType -> Label   --used for debugging or for ensuring that variables declared within some evaluation of a recursive type (e.g. printing a list of lists of lists) have unique names
genNameFromType (List _ st) = "l" ++ genNameFromType st
genNameFromType (Base _) = "b"
genNameFromType (Type _) = "t"
genNameFromType (Iterator st) = "i" ++ genNameFromType st
genNameFromType (EnumType _) = "e"
        
jump :: Jump -> Doc
jump Break = text "break"
jump Continue = text "continue"

litsToValues :: [Literal] -> [Value]
litsToValues = map Lit

clsWithName :: [Class] -> Label -> Class
clsWithName (c:cs) n = if className c == n then c else clsWithName cs n
clsWithName [] n = error $ "Class '" ++ n ++ "' not found"

typeOfLit :: Literal -> BaseType
typeOfLit (LitBool _) = Boolean
typeOfLit (LitInt _) = Integer
typeOfLit (LitFloat _) = Float
typeOfLit (LitChar _) = Character
typeOfLit (LitStr _) = String
