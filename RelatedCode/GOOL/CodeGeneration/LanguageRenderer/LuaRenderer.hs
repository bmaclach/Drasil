-- | The logic to render Lua code from an 'AbstractCode' is contained in this module
module GOOL.CodeGeneration.LanguageRenderer.LuaRenderer (
    -- * Lua Code Configuration -- defines syntax of all Lua code
    luaConfig
) where

import GOOL.Code (Code(..))
import GOOL.CodeGeneration.AbstractCode hiding (comment,bool,int,float,char)
import GOOL.CodeGeneration.LanguageRenderer
import GOOL.Auxil.Printing (blank,oneTab,oneTabbed)
import GOOL.Auxil.DataTypes (Label,Options)
import GOOL.Auxil.Helper (vmap,vibmap)

import Data.List (intersperse)
import Prelude hiding (break,print,return)
import Text.PrettyPrint.HughesPJ

luaConfig :: Options -> Config -> Config
luaConfig options c = 
    Config {
        renderCode = renderCode' c,
        
        argsList         = text "arg",
        bitArray         = empty,
        commentStart     = text "--",
        endStatement     = empty,
        enumsEqualInts   = True,
        ext              = ".lua",
        fileName         = \p _ -> p,
        include          = include',
        includeScope     = \_ -> empty,
        inherit          = text "inheritsFrom",
        inputFunc        = text "io.stdin:read()",
        iterForEachLabel = forLabel,
        iterInLabel      = text "in",
        list             = \_ -> empty,
        listObj          = empty,
        clsDec           = empty,
        package          = \_ -> empty,
        printFunc        = text "io.write",
        printLnFunc      = text "print",
        stateType        = luastateType c,
        
        blockStart = text "do", blockEnd = text "end",
        ifBodyStart = text "then", elseIf = text "elseif",
        
        top    = luatop c,
        body   = luabody c,
        bottom = \_ -> empty,
        
        assignDoc = assignDocD' c, binOpDoc = binOpDoc', bodyDoc = bodyDocD c, blockDoc = blockDocD c, callFuncParamList = callFuncParamListD c,
        conditionalDoc = conditionalDoc' c, declarationDoc = declarationDoc' c, enumElementsDoc = enumElementsDocD c, exceptionDoc = exceptionDoc' c, exprDoc = exprDocD c, funcAppDoc = funcAppDocD c,
        funcDoc = funcDoc' c, iterationDoc = iterationDoc' c, litDoc = litDocD,
        clsDecDoc = clsDecDocD c, clsDecListDoc = clsDecListDocD c, classDoc = classDoc' c, objAccessDoc = objAccessDoc' c,
        objVarDoc = objVarDoc' c, paramDoc = paramDoc' c, paramListDoc = paramListDocD c, patternDoc = patternDocD c, printDoc = printDocD c, retDoc = retDocD c, scopeDoc = \_ -> empty,
        stateDoc = stateDocD c, stateListDoc = stateListDocD c, statementDoc = statementDocD c, methodDoc = methodDoc' c,
        methodListDoc = methodListDocD c, methodTypeDoc = \_ -> empty, unOpDoc = unOpDoc', valueDoc = valueDoc' c
    }

-- convenience
initName, indexOf :: Label
initName = "Create"
indexOf = "TableFind"

incl :: Doc
incl = text "require"

-- short names, packaged up above (and used below)
renderCode' :: Config -> [Label] -> AbstractCode -> Code
renderCode' c ms (AbsCode p) = Code [fileCode c p ms Source (ext c)]

include' :: Label -> Doc
include' n = incl <+> quotes (text n)

luastateType :: Config -> StateType -> DecDef -> Doc
luastateType _ (List _ t) _  = braces (empty)
luastateType _ (Base _) _    = empty
luastateType _ (Type name) _ = text name <> colon <> text initName
luastateType c s d           = stateTypeD c s d

luatop :: Config -> FileType -> a -> Doc
luatop c ft _ = vcat [
    methodDoc c ft "" tableFindFunc,      --Needed for IndexOf function calls. TODO: only include this if IndexOf is used in the code
    blank,
    text inheritanceFunc]                --Function used to simulate definition of classes and inheritance in Lua. Also defines default constuctor, and some common OO class features.
    where tableFindFunc = privMethod (MState (Base Integer)) indexOf [tableParam, valParam] tableFindBody     --TODO only include the TableFind definition if it's used in the body of the code?
          v = "v"
          val = "val"
          tableParam = param "t" (Type "Table")
          valParam = param val (Type "Value")
          tableFindBody = [Block [forEach v (ListVar "t" (Type "Value")) forBody], Block [return $ litInt (-1)]]
          forBody = oneLiner $ ifCond [(condExpr, oneLiner $ returnVar $ keyLabel v)] noElse
          condExpr = (Var v) ?== (Var val)
          --Lua inheritance code from http://lua-users.org/wiki/InheritanceTutorial
          --TODO only include the inheritance function definition if it's used in the body of the code?
          inheritanceFunc = "function " ++ render (inherit c) ++"(baseClass)\n\        
                            \    local new_class = {}\n\
                            \    new_class.__index = new_class\n\n\
                            \    function new_class:" ++ initName ++ "()\n\
                            \        local newinst = {}\n\
                            \        setmetatable(newinst, new_class)\n\
                            \        return newinst\n\
                            \    end\n\n\
                            \    if nil ~= baseClass then setmetatable(new_class, {__index = baseClass}) end\n\n\
                            \    function new_class:class()\n\
                            \        return new_class\n\
                            \    end\n\n\
                            \    function new_class:superClass()\n\
                            \        return baseClass\n\
                            \    end\n\n\
                            \    function new_class:isa( theClass )\n\
                            \        local b_isa = false\n\
                            \        local cur_class = new_class\n\
                            \        while ( nil ~= cur_class ) and ( false == b_isa ) do\n\
                            \            if cur_class == theClass then\n\
                            \                b_isa = true\n\
                            \            else\n\
                            \                cur_class = cur_class:superClass()\n\
                            \            end\n\
                            \        end\n\
                            \        return b_isa\n\
                            \    end\n\n\
                            \    return new_class\n\
                            \end"

luabody :: Config -> FileType -> Label -> [Class] -> Doc
luabody c f p ms = vibmap (classDoc c f p) $ fixCtorNames initName ms

-- code doc functions
binOpDoc' :: BinaryOp -> Doc
binOpDoc' NotEqual = text "~="
binOpDoc' Power = text "^"
binOpDoc' op = binOpDocD op

conditionalDoc' :: Config -> Conditional -> Doc
conditionalDoc' c (If (t:ts) elseBody) =
    let ifSect (v, b) = vcat [
            text "if" <+> parens (valueDoc c v) <+> ifBodyStart c,
            oneTab $ bodyDoc c b,
            endIf]
        elseIfSect (v, b) = vcat [
            elseIf c <+> parens (valueDoc c v) <+> ifBodyStart c,
            oneTab $ bodyDoc c b,
            endElseif v]
        elseSect = if null elseBody then empty else vcat [
            text "else",
            oneTab $ bodyDoc c elseBody,
            blockEnd c]
        endIf = if null ts && null elseBody then blockEnd c else empty
        endElseif v = if v == fst (last ts) && null elseBody then blockEnd c else empty
    in vcat [
        ifSect t,
        vmap elseIfSect ts,
        elseSect]
conditionalDoc' c cnd = conditionalDocD' c cnd

declarationDoc' :: Config -> Declaration -> Doc
declarationDoc' _ (VarDec _ _) = empty
declarationDoc' c (ListDec lt n t s) = text n <+> equals <+> stateType c (List lt t) Dec
declarationDoc' c (ListDecValues _ n t vs) = text n <+> equals <+> braces (callFuncParamList c vs)
declarationDoc' c (VarDecDef n t v) = text n <+> equals <+> valueDoc c v
declarationDoc' c (ConstDecDef n l) = declarationDoc c $ VarDecDef n (Base $ typeOfLit l) (Lit l)
declarationDoc' c d = declarationDocD c d

exceptionDoc' :: Config -> Exception -> Doc
exceptionDoc' c (Throw s) = text "error" <> parens (litDoc c $ LitStr s)
exceptionDoc' c (TryCatch tryBody catchBody) = vcat [
    text ("local " ++ status ++ ",") <+> text "exc" <+> equals <+> text "pcall(function()",
    oneTab $ bodyDoc c tryBody,
    blockEnd c <> text ")",
    conditionalDoc c $ If [( (?!)(Var status), catchBody )] noElse]
    where status = "excstatus"

funcDoc' :: Config -> Function -> Doc
funcDoc' _ (Cast _) = empty
funcDoc' c (Func n vs) = colon <> funcAppDoc c n vs
funcDoc' c (Get n) = colon <> funcAppDoc c (getterName n) []
funcDoc' c (Set n v) = colon <> funcAppDoc c (setterName n) [v]
funcDoc' _ (IndexOf var) = text indexOf
funcDoc' _ (ListSize) = text "#"
funcDoc' c (ListAccess i) = brackets $ valueDoc c $ listIndex i
funcDoc' _ (ListAdd _ _) = text "table" <> dot <> text "insert"
funcDoc' c (ListSet i v) = brackets (valueDoc c $ listIndex i) <+> equals <+> valueDoc c v
funcDoc' c f = funcDocD c f

iterationDoc' :: Config -> Iteration -> Doc
iterationDoc' c (For (DeclState (VarDecDef i (Base Integer) initv)) (Expr (BinaryExpr _ Less finalv)) (AssignState a) b) = 
    let step = case a of (PlusPlus _)     -> empty
                         (PlusEquals _ v) -> comma <> valueDoc c v
                         _                -> error "Lua: Unsupported form of step assignment in For statement. Use PlusPlus or PlusEquals."
    in vcat [
        forLabel <+> text i <+> equals <+> valueDoc c initv <> comma <> parens (valueDoc c (finalv #- litInt 1)) <> step <+> blockStart c,
        oneTab $ bodyDoc c b,
        blockEnd c]
iterationDoc' _ (For _ _ _ _) = error "Lua: Generic form of For statement not yet implemented. Use an integer index and a Less conditional, or replace with a ForEach."
iterationDoc' c (ForEach i listVar@(ListVar _ t) b) = vcat [
    iterForEachLabel c <+> text (keyLabel i) <> comma <> text i <+> iterInLabel c <+> funcAppDoc c "ipairs" [listVar] <+> blockStart c,
    oneTab $ bodyDoc c b,
    blockEnd c]
iterationDoc' c i = iterationDocD c i

classDoc' :: Config -> FileType -> Label -> Class -> Doc
classDoc' c _ _ (Enum n _ es) = vcat [
    text n <+> equals <+> lbrace,
    oneTab $ enumElementsDoc c es,
    rbrace]
classDoc' c f _ (Class n p _ _ fs) = vcat [ 
    text n <+> equals <+> inherit c <> parens (text baseClass),
    methodListDoc c f n fs]
    where baseClass = case p of Just pn -> pn
                                Nothing -> "nil"
classDoc' c f _ (MainClass _ _ fs) = methodListDoc c f "" fs

objAccessDoc' :: Config -> Value -> Function -> Doc
objAccessDoc' c v@(Self) f = valueDoc c v <> funcDoc c f
objAccessDoc' c v (Cast _) = valueDoc c v
objAccessDoc' c v f@(ListSize) = funcDoc c f <> parens (valueDoc c v)
objAccessDoc' c v f@(ListAdd i e) = funcDoc c f <> parens (callFuncParamList c [v, listIndex i, e])       --add 1 to account for Lua's 1-indexed lists
objAccessDoc' c v f@(IndexOf var) = funcDoc c f <> parens (callFuncParamList c [v, var])
objAccessDoc' c v f@(Floor) = funcAppDoc c "math.floor" [v]
objAccessDoc' c v f@(Ceiling) = funcAppDoc c "math.ceil" [v]
objAccessDoc' c v f = objAccessDocD c v f
          
objVarDoc' :: Config -> Value -> Value -> Doc
objVarDoc' c v1 v2 = valueDoc c v1 <> dot <> valueDoc c v2

paramDoc' :: Config -> Parameter -> Doc
paramDoc' _ (StateParam n _) = text n
paramDoc' c p = paramDocD c p

methodDoc' :: Config -> FileType -> Label -> Method -> Doc
methodDoc' c ft m f@(Method _ _ (Construct _) _ b) =
    let temp = Var "temp"
    in vcat [
        transDecLine c ft m f,
        oneTabbed [
            text "local temp = {}",
            text "setmetatable" <> parens (text "temp," <> text m),
            blank,
            bodyDoc c $ bodyReplace Self temp b,
            bodyBlank,
            retDoc c $ Ret temp],
        text "end"]
    where bodyBlank = if null b then empty else blank
methodDoc' c ft m f@(Method _ _ _ _ b) = vcat [
    transDecLine c ft m f,
    oneTab $ bodyDoc c b,
    text "end"]
methodDoc' c _ _ (MainMethod b) = bodyDoc c b
methodDoc' c ft m f = methodDocD c ft m f

unOpDoc' :: UnaryOp -> Doc
unOpDoc' Abs = text "math.abs"
unOpDoc' op = unOpDocD' op

valueDoc' :: Config -> Value -> Doc
valueDoc' _ (Self) = text "self"
valueDoc' c (StateObj t@(List _ _) _) = listObj c <> stateType c t Def
valueDoc' c (StateObj t vs) = stateType c t Def <> parens (callFuncParamList c vs)
valueDoc' c v@(Arg _) = valueDocD' c v
valueDoc' c v = valueDocD c v

----------------------
-- Helper Functions --
----------------------
keyLabel :: Label -> Label
keyLabel n = n ++ "_key"

listIndex :: Value -> Value     --AbstractCode lists are 0-indexed, but Lua's are 1-indexed. This function just increments the given Value by 1 to compensate.
listIndex i = i #+ litInt 1

transDecLine :: Config -> FileType -> Label -> Method -> Doc
transDecLine c _ m (Method n _ t ps _) = text "function" <+> text m <> modColon <> text n <> parens (paramListDoc c ps)
    where modColon = if null m then empty else colon
