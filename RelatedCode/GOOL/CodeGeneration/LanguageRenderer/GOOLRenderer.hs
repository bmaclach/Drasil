-- | The logic to render GOOL code from an 'AbstractCode' is contained in this module
module GOOL.CodeGeneration.LanguageRenderer.GOOLRenderer (
    -- * GOOL Code Configuration -- defines syntax of all GOOL code
    goolConfig
) where

import GOOL.Code (Code(..))
import GOOL.CodeGeneration.AbstractCode hiding (comment,bool,int,float,char)
import GOOL.CodeGeneration.LanguageRenderer
import GOOL.Auxil.Printing (blank,oneTab,oneTabbed,doubleQuotedText,verticalComma)
import GOOL.Auxil.DataTypes (Label,Options(hsModule))
import GOOL.Auxil.Helper (himap,vibcat,vibmap)

import Data.Char (toLower)
import Prelude hiding (print)
import Text.PrettyPrint.HughesPJ

goolConfig :: Options -> Config -> Config
goolConfig options c = 
    let hsMod = case (hsModule options) of Nothing -> error "GOOLRenderer: The GOOLHsModule option must be specified in the Configuration file when generating GOOL code."
                                           Just hsMod' -> hsMod'
    in Config {
        renderCode = renderCode' c,
        
        argsList         = text "Arg",
        bitArray         = empty,
        commentStart     = text "--",
        endStatement     = empty,
        enumsEqualInts   = False,
        ext              = ".hs",
        fileName         = \p _ -> p,
        include          = includeD "import",
        includeScope     = \_ -> empty,
        inherit          = text "extends",
        inputFunc        = text "Input",
        iterForEachLabel = text "ForEach",
        iterInLabel      = empty,
        list             = \lt -> case lt of Static  -> text "List Static"
                                             Dynamic -> text "List Dynamic",
        listObj          = empty,
        clsDec           = empty,
        package          = \p -> vcat [
            text $ makeAbsCode ++ " :: AbstractCode",
            text $ makeAbsCode ++ " = AbsCode $ Pack \"" ++ p ++ "\" " ++ classList],
        printFunc        = text "print",
        printLnFunc      = text "printLn",
        stateType        = goolstateType c,
        
        blockStart = text "[", blockEnd = text "]", 
        ifBodyStart = blockStart c, elseIf = empty,
        
        top    = gooltop c hsMod,
        body   = goolbody c,
        bottom = \_ -> empty,
        
        assignDoc = assignDoc' c, binOpDoc = binOpDoc', bodyDoc = bodyDoc' c, blockDoc = blockDoc' c, callFuncParamList = callFuncParamList' c,
        conditionalDoc = conditionalDoc' c, declarationDoc = declarationDoc' c, enumElementsDoc = enumElementsDoc' c, exceptionDoc = exceptionDoc' c, exprDoc = exprDoc' c, funcAppDoc = funcAppDoc' c,
        funcDoc = funcDoc' c, iterationDoc = iterationDoc' c, litDoc = litDoc',
        clsDecDoc = clsDecDoc' c, clsDecListDoc = clsDecListDoc' c, classDoc = classDoc' c, objAccessDoc = objAccessDoc' c,
        objVarDoc = objVarDoc' c, paramDoc = paramDoc' c, paramListDoc = paramListDoc' c, patternDoc = patternDoc' c, printDoc = printDoc' c, retDoc = retDocD c, scopeDoc = scopeDoc',
        stateDoc = stateDoc' c, stateListDoc = stateListDoc' c, statementDoc = statementDoc' c, methodDoc = methodDoc' c,
        methodListDoc = methodListDoc' c, methodTypeDoc = methodTypeDoc' c, unOpDoc = unOpDoc', valueDoc = valueDoc' c,
        getEnv = \_ -> error "getEnv for GOOL is not defined"
    }
    
-- for convenience
classList, classNameList, makeAbsCode :: String
classList = "classes"
classNameList = "clsNames"
makeAbsCode = "makeAbstractCode"

-- short names, packaged up above (and used below)
renderCode' :: Config -> [Label] -> AbstractCode -> Code
renderCode' c ms (AbsCode p) = Code [fileCode c p ms Source (ext c)]

goolstateType :: Config -> StateType -> DecDef -> Doc
goolstateType c (List lt t) _ = parens $ text "List" <+> listTypeDoc lt <+> stateType c t Dec
goolstateType c (Iterator t) _ = parens $ text "Iterator" <+> stateType c t Dec
goolstateType _ (Base Boolean) _ = text "bool"
goolstateType _ (Base Integer) _ = text "int"
goolstateType _ (Base Float) _ = text "float"
goolstateType _ (Base Character) _ = text "char"
goolstateType _ (Base String) _ = text "string"
goolstateType _ (Type name) _ = parens $ text "Type" <+> lbl name
goolstateType _ (EnumType enum) _ = parens $ text "EnumType" <+> lbl enum

gooltop :: Config -> Label -> a -> b -> Doc
gooltop c hsMod _ _ = vcat [
    text $ "module " ++ hsMod ++ " (" ++ classNameList ++ ", " ++ makeAbsCode ++ ") where",
    blank,
    include c "Prelude hiding (break,log,print,return)",
    include c "GOOL.CodeGeneration.AbstractCode",
    include c "GOOL.Auxil.DataTypes"]

goolbody :: Config -> a -> Label -> [Class] -> Doc
goolbody c _ p cs = vibcat [
    package c p,
    clsDecListDoc c cs,
    vibmap (classDoc c Source p) cs]

-- code doc functions
assignDoc' :: Config -> Assignment -> Doc
assignDoc' c (Assign (Var n) v2) = lbl n <+> text "&.=" <+> valueDoc c v2
assignDoc' c (Assign v1 (Var n)) = valueDoc c v1 <+> text "&=." <+> lbl n
assignDoc' c (Assign v1 v2) = valueDoc c v1 <+> text "&=" <+> valueDoc c v2
assignDoc' c (PlusEquals (Var n) v2) = lbl n <+> text "&+=" <+> valueDoc c v2
assignDoc' c (PlusEquals v1 v2) = text "AssignState $ PlusEquals" <+> valueDoc c v1 <+> valueDoc c v2
assignDoc' _ (PlusPlus (Var n)) = text "(&++)" <> lbl n
assignDoc' c (PlusPlus v) = text "AssignState $ PlusPlus" <+> valueDoc c v

binOpDoc' :: BinaryOp -> Doc
binOpDoc' Equal = text "?=="
binOpDoc' NotEqual = text "?!="
binOpDoc' Greater = text "?>"
binOpDoc' GreaterEqual = text "?>="
binOpDoc' Less = text "?<"
binOpDoc' LessEqual = text "?<="
binOpDoc' Plus = text "#+"
binOpDoc' Minus = text "#-"
binOpDoc' Divide = text "#/"
binOpDoc' Multiply = text "#*"
binOpDoc' Modulo = text "#%"
binOpDoc' Power = text "#^"

bodyDoc' :: Config -> Body -> Doc
bodyDoc' c [Block (s:[])] = parens $ text "oneLiner $" <+> statementDoc c NoLoop s
bodyDoc' c bs = hsVList (blockDoc c) bs

blockDoc' :: Config -> Block -> Doc
blockDoc' c (Block ss) =
    text "Block" <+> hsVList (statementDoc c NoLoop) ss
    
callFuncParamList' :: Config -> [Value] -> Doc
callFuncParamList' c vs = brackets $ himap comma (valueDoc c) vs

conditionalDoc' :: Config -> Conditional -> Doc
conditionalDoc' c i@(If [] _) = conditionalDocD c i
conditionalDoc' c (If cs b) = text "ifCond" <+> casesDoc c (valueDoc c) cs <+> elseBody c b
conditionalDoc' c s@(Switch _ [] _) = conditionalDocD c s
conditionalDoc' c (Switch v cs b) = text "switch" <+> parens (valueDoc c v) <+> casesDoc c (litDoc c) cs <+> elseBody c b

declarationDoc' :: Config -> Declaration -> Doc
declarationDoc' c (VarDec n t) = text "varDec" <+> lbl n <+> stateType c t Dec
declarationDoc' c (ListDec lt n t s) = text "listDec" <+> listTypeDoc lt <+> lbl n <+> stateType c t Dec <+> int s
declarationDoc' c (ListDecValues Static n t vs) = text "listDecValues" <+> lbl n <+> stateType c t Dec <+> hsList (valueDoc c) vs
declarationDoc' c (ListDecValues lt n t vs) = text "DeclState $ ListDecValues" <+> listTypeDoc lt <+> lbl n <+> stateType c t Dec <+> hsList (valueDoc c) vs
declarationDoc' c (VarDecDef n t v) = text "varDecDef" <+> lbl n <+> stateType c t Dec <+> valueDoc c v
declarationDoc' c (ObjDecDef n t v) = text "objDecDef" <+> lbl n <+> stateType c t Dec <+> valueDoc c v
declarationDoc' c (ConstDecDef n l) = text "constDecDef" <+> lbl n <+> litDoc c l

enumElementsDoc' :: Config -> [Label] -> Doc
enumElementsDoc' _ es = hsList lbl es

exceptionDoc' :: Config -> Exception -> Doc
exceptionDoc' _ (Throw s) = text "throw" <+> lbl s
exceptionDoc' c (TryCatch tb cb) = text "tryCatch" <+> bodyDoc c tb <+> bodyDoc c cb

exprDoc' :: Config -> Expression -> Doc
exprDoc' c (UnaryExpr r v) = parens (unOpDoc c r) <> valueDoc c v
exprDoc' c (BinaryExpr v1 op v2) = valueDoc c v1 <+> binOpDoc c op <+> valueDoc c v2
exprDoc' c (Exists v) = text "Expr $ Exists" <+> valueDoc c v

funcAppDoc' :: Config -> Label -> [Value] -> Doc
funcAppDoc' c n vs = lbl n <+> hsList (valueDoc c) vs

funcDoc' :: Config -> Function -> Doc
funcDoc' c (Func n vs) = text "Func" <+> funcAppDoc c n vs
funcDoc' c (Cast t) = text "Cast" <+> stateType c t Dec
funcDoc' _ (Get n) = text "Get" <+> lbl n
funcDoc' c (Set n v) = text "Set" <+> lbl n <+> valueDoc c v
funcDoc' c (IndexOf v) = text "IndexOf" <+> valueDoc c v
funcDoc' _ ListSize = text "ListSize"
funcDoc' _ (ListAccess (Var n)) = text "at" <+> lbl n
funcDoc' c (ListAccess v) = text "ListAccess" <+> valueDoc c v
funcDoc' c (ListAdd i v) = text "ListAdd" <+> valueDoc c i <+> valueDoc c v
funcDoc' c (ListSet i v) = text "ListSet" <+> valueDoc c i <+> valueDoc c v
funcDoc' c (ListPopulate v t) = text "ListPopulate" <+> valueDoc c v <+> stateType c t Dec
funcDoc' _ (IterBegin) = text "IterBegin"
funcDoc' _ (IterEnd) = text "IterEnd"
funcDoc' _ Floor = text "Floor"
funcDoc' _ Ceiling = text "Ceiling"

iterationDoc' :: Config -> Iteration -> Doc
iterationDoc' c (For initv cond upd b) = vcat [
    text "for" <+> parens (statementDoc c NoLoop initv) <+> valueDoc c cond <+> parens (statementDoc c NoLoop upd),
    bodyDoc c b]
iterationDoc' c (ForEach var listVar@(ListVar _ _) b) = vcat [
    text "forEach" <+> lbl var <+> valueDoc c listVar,
    bodyDoc c b]
iterationDoc' c (While v b) = vcat [
    text "while" <+> valueDoc c v,
    bodyDoc c b]
iterationDoc' c i = iterationDocD c i

listTypeDoc :: Permanence -> Doc
listTypeDoc Static = text "Static"
listTypeDoc Dynamic = text "Dynamic"

litDoc' :: Literal -> Doc
litDoc' = parens . litDoc''

litDoc'' :: Literal -> Doc
litDoc'' (LitBool v) = text "LitBool" <+> text (show v)
litDoc'' (LitInt v) = text "LitInt" <+> int v
litDoc'' (LitFloat v) = text "LitFloat" <+> float v
litDoc'' (LitChar v) = text "LitChar" <+> quotes (char v)
litDoc'' (LitStr v) = text "LitStr" <+> lbl v

clsDecDoc' :: Config -> Class -> Doc
clsDecDoc' _ cl = text $ hsClassName $ className cl

clsDecListDoc' :: Config -> [Class] -> Doc
clsDecListDoc' c cs = vcat [
    text $ classList ++ " :: [Class]",
    text classList <+> equals <+> hsList (text . hsClassName) clsNames,
    blank,
    text $ classNameList ++ " :: [String]",
    text classNameList <+> equals <+> hsList lbl clsNames,
    blank,
    himap comma (clsDecDoc c) cs <+> text ":: Class"]
    where clsNames = map className cs
    
classDoc' :: Config -> FileType -> Label -> Class -> Doc
classDoc' c _ _ (Enum n s es) = vcat [
    text (hsClassName n) <+> equals,
    oneTabbed [text "Enum" <+> lbl n <+> scopeDoc c s,
               enumElementsDoc c es]]
classDoc' c _ _ (Class n p s vs ms) = vcat [
    text (hsClassName n) <+> equals,
    oneTabbed [text scopeClass <+> lbl n <+> baseClass <+> stateListDoc c vs,
               methodListDoc c Source n ms]]
    where scopeClass = case s of Private -> "privClass"
                                 Public -> "pubClass"
          baseClass = case p of Nothing -> text "noParent"
                                Just pn -> parens $ inherit c <+> lbl pn
classDoc' c _ _ (MainClass n vs ms) = vcat [
    text (hsClassName n) <+> equals,
    oneTabbed [text "MainClass" <+> lbl n <+> stateListDoc c vs,
    methodListDoc c Source n ms]]

objAccessDoc' :: Config -> Value -> Function -> Doc
objAccessDoc' c v f = valueDoc c v <+> text "$." <+> funcDoc c f

objVarDoc' :: Config -> Value -> Value -> Doc
objVarDoc' c v1 v2 = valueDoc c v1 <> text "$->" <> valueDoc c v2

paramDoc' :: Config -> Parameter -> Doc
paramDoc' c (StateParam n t) = text "param" <+> lbl n <+> stateType c t Dec
paramDoc' c (FuncParam n t ps) = text "FuncParam" <+> lbl n <+> methodTypeDoc c t <+> paramListDoc c ps

paramListDoc' :: Config -> [Parameter] -> Doc
paramListDoc' c = hsList $ paramDoc c

patternDoc' :: Config -> Pattern -> Doc
patternDoc' _ (State (InitState n s)) = text "PatternState $ State $ InitState" <+> lbl n <+> lbl s
patternDoc' _ (State (ChangeState n s)) = text "PatternState $ State $ ChangeState" <+> lbl n <+> lbl s
patternDoc' c p@(State (CheckState _ [] _)) = patternDocD c p
patternDoc' c (State (CheckState n cs b)) =
    text "PatternState $ State $ CheckState" <+> lbl n <+> casesDoc c lbl cs <+> elseBody c b
patternDoc' c (Strategy (RunStrategy n (Strats s r) v)) =
    text "PatternState $ Strategy $ RunStrategy" <+> lbl n <+> parens (text "Strats" <+> casesDoc c lbl s <+> mVal r) <+> mVal v
    where mVal Nothing = text "Nothing"
          mVal (Just v') = parens $ text "Just" <+> valueDoc c v'
patternDoc' c (Observer (InitObserverList t os)) =
    text "PatternState $ Observer $ InitObserverList" <+> stateType c t Dec <+> hsList (valueDoc c) os
patternDoc' c (Observer (AddObserver t o)) =
    text "PatternState $ Observer $ AddObserver" <+> stateType c t Dec <+> valueDoc c o
patternDoc' c (Observer (NotifyObservers t fn ps)) = 
    text "PatternState $ Observer $ NotifyObservers" <+> stateType c t Dec <+> lbl fn <+> hsList (valueDoc c) ps
    
printDoc' :: Config -> Bool -> StateType -> Value -> Doc
printDoc' _ newLn (Base String) (Lit (LitStr s)) = text printFn <+> lbl s
    where printFn = if newLn then "printStrLn" else "printStr"
printDoc' c newLn t v = printFn <+> stateType c t Dec <+> valueDoc c v
    where printFn = if newLn then printLnFunc c else printFunc c
    
scopeDoc' :: Scope -> Doc
scopeDoc' Private = text "Private"
scopeDoc' Public = text "Public"

stateDoc' :: Config -> StateVar -> Doc
stateDoc' c (StateVar n s p t del) = text var <+> delp <+> stateType c t Dec <+> lbl n
    where var = case (s,p) of (Public, Dynamic) -> "pubMVar"
                              (Public, Static)  -> "pubGVar"
                              (Private, Dynamic) -> "privMVar"
                              (Private, Static) -> "privGVar"
          delp | del == alwaysDel = text "alwaysDel"
               | del == neverDel = text "neverDel"
               | otherwise = int del

stateListDoc' :: Config -> [StateVar] -> Doc
stateListDoc' c = hsVList $ stateDoc c

statementDoc' :: Config -> StatementLocation -> Statement -> Doc
statementDoc' c _ (FreeState v) = text "FreeState" <+> valueDoc c v
statementDoc' c loc s = statementDocD c loc s

methodDoc' :: Config -> FileType -> Label -> Method -> Doc
methodDoc' c _ _ (Method n s t ps b) = vcat [
    text meth <+> methodTypeDoc c t <+> lbl n <+> paramListDoc c ps,
    bodyDoc c b]
    where meth = case s of Public -> "pubMethod"
                           Private -> "privMethod"
methodDoc' c _ _ (MainMethod b) = vcat [
    text "MainMethod",
    bodyDoc c b]
methodDoc' c _ _ (GetMethod n t) = text "GetMethod" <+> lbl n <+> methodTypeDoc c t
methodDoc' c _ _ (SetMethod n p) = text "SetMethod" <+> lbl n <+> paramDoc c p

methodListDoc' :: Config -> FileType -> Label -> [Method] -> Doc
methodListDoc' c t m ms = hsVList (methodDoc c t m) ms

methodTypeDoc' :: Config -> MethodType -> Doc
methodTypeDoc' c (MState t) = parens $ text "typ" <+> stateType c t Dec
methodTypeDoc' _ Void = text "Void"
methodTypeDoc' _ (Construct n) = parens $ text "Construct" <+> lbl n

unOpDoc' :: UnaryOp -> Doc
unOpDoc' Negate = text "#~"
unOpDoc' SquareRoot = text "#/^"
unOpDoc' Abs = text "#|"
unOpDoc' Not = text "?!"

valueDoc' :: Config -> Value -> Doc
valueDoc' c = parens . valueDoc'' c

valueDoc'' :: Config -> Value -> Doc
valueDoc'' _ (Const n) = text "Const" <+> lbl n
valueDoc'' _ (Lit (LitBool v)) = text "litBool" <+> text (show v)
valueDoc'' _ (Lit (LitInt v)) = text "litInt" <+> int v
valueDoc'' _ (Lit (LitFloat v)) = text "litFloat" <+> float v
valueDoc'' _ (Lit (LitChar v)) = text "litChar" <+> quotes (char v)
valueDoc'' _ (Lit (LitStr v)) = text "litString" <+> lbl v
valueDoc'' _ (EnumElement en e) = lbl en <+> text "$:" <+> lbl e
valueDoc'' c (FuncApp n vs) = text "FuncApp" <+> funcAppDoc c n vs
valueDoc'' _ Self = text "Self"
valueDoc'' c (StateObj t vs) = text "StateObj" <+> stateType c t Dec <+> hsList (valueDoc c) vs
valueDoc'' _ (Var v) = text "Var" <+> lbl v
valueDoc'' _ (EnumVar v) = text "EnumVar" <+> lbl v
valueDoc'' c (ListVar v t) = lbl v <+> text "`listOf`" <+> stateType c t Dec
valueDoc'' _ (Arg i) = text "Arg" <+> int i
valueDoc'' c v = valueDocD c v

----------------------
-- Helper Functions --
----------------------
hsList :: (a -> Doc) -> [a] -> Doc
hsList f = brackets . hcat . punctuate comma . map f

hsVList :: (a -> Doc) -> [a] -> Doc
hsVList f es = vcat [
    text "[",
    blank,
    oneTab $ verticalComma f es,
    text "]" ]

hsClassName :: Label -> Label
hsClassName n = map toLower n ++ "Class"

lbl :: String -> Doc
lbl = doubleQuotedText

casesDoc :: Config -> (a -> Doc) -> [(a, Body)] -> Doc
casesDoc c f = hsVList (\(a,b) -> parens $ f a <> comma <+> bodyDoc c b)

elseBody :: Config -> Body -> Doc
elseBody c b = if null b then text "noElse" else bodyDoc c b
