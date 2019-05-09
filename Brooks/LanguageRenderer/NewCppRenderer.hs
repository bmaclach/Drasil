{-# LANGUAGE TypeFamilies #-}

module LanguageRenderer.NewCppRenderer (
    -- * C++ Code Configuration -- defines syntax of all C++ code
    CppSrcCode(..)
) where

import New (Label,
    PackageSym(..), RenderSym(..), KeywordSym(..), PermanenceSym(..),
    BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
    UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), NumericExpression(..), 
    BooleanExpression(..), ValueExpression(..), Selector(..), FunctionSym(..), 
    SelectorFunction(..), StatementSym(..), ControlStatementSym(..), ScopeSym(..),
    MethodTypeSym(..), ParameterSym(..), MethodSym(..), StateVarSym(..), 
    ClassSym(..), ModuleSym(..))
import NewLanguageRenderer (fileDoc', moduleDocD, classDocD, enumDocD,
    enumElementsDocD, multiStateDocD, blockDocD, bodyDocD, outDocD,
    printFileDocD, boolTypeDocD, intTypeDocD, charTypeDocD, stringTypeDocD,
    typeDocD, listTypeDocD, voidDocD, constructDocD, stateParamDocD, 
    paramListDocD, methodDocD, methodListDocD, stateVarDocD, stateVarListDocD,
    ifCondDocD, switchDocD, forDocD, forEachDocD, whileDocD, stratDocD, 
    assignDocD, plusEqualsDocD, plusPlusDocD, varDecDocD, varDecDefDocD, 
    listDecDocD, listDecDefDocD, objDecDefDocD, constDecDefDocD, statementDocD,
    returnDocD, commentDocD, notOpDocD, negateOpDocD, unOpDocD, equalOpDocD, 
    notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
    lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
    moduloOpDocD, andOpDocD, orOpDocD, binOpDocD, binOpDocD', litTrueD, 
    litFalseD, litCharD, litFloatD, litIntD, litStringD, defaultCharD, 
    defaultFloatD, defaultIntD, defaultStringD, varDocD, extVarDocD, selfDocD,
    argDocD, enumElemDocD, objVarDocD, inlineIfDocD, funcAppDocD, 
    extFuncAppDocD, stateObjDocD, listStateObjDocD, notNullDocD, 
    listIndexExistsDocD, funcDocD, castDocD, listSetDocD, listAccessDocD, 
    objAccessDocD, 
    castObjDocD, breakDocD, continueDocD, staticDocD, dynamicDocD, privateDocD, 
    publicDocD, dot, new, observerListName, doubleSlash, addCommentsDocD, 
    callFuncParamList, getterName, setterName, setMain, statementsToStateVars)
import Helpers (angles, oneTab, tripFst, tripSnd, tripThird)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Data.Map as Map (fromList,lookup)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, comma, empty,
  equals, semi, vcat, lbrace, rbrace, render, colon, render)

newtype CppSrcCode a = CPPSC {unCPPSC :: a}

instance Functor CppSrcCode where
    fmap f (CPPSC x) = CPPSC (f x)

instance Applicative CppSrcCode where
    pure = CPPSC
    (CPPSC f) <*> (CPPSC x) = CPPSC (f x)

instance Monad CppSrcCode where
    return = CPPSC
    CPPSC x >>= f = f x

liftA4 :: (a -> b -> c -> d -> e) -> CppSrcCode a -> CppSrcCode b -> CppSrcCode c -> CppSrcCode d -> CppSrcCode e
liftA4 f a1 a2 a3 a4 = CPPSC $ f (unCPPSC a1) (unCPPSC a2) (unCPPSC a3) (unCPPSC a4)

liftA5 :: (a -> b -> c -> d -> e -> f) -> CppSrcCode a -> CppSrcCode b -> CppSrcCode c -> CppSrcCode d -> CppSrcCode e -> CppSrcCode f
liftA5 f a1 a2 a3 a4 a5 = CPPSC $ f (unCPPSC a1) (unCPPSC a2) (unCPPSC a3) (unCPPSC a4) (unCPPSC a5)

liftA6 :: (a -> b -> c -> d -> e -> f -> g) -> CppSrcCode a -> CppSrcCode b -> CppSrcCode c -> CppSrcCode d -> CppSrcCode e -> CppSrcCode f -> CppSrcCode g
liftA6 f a1 a2 a3 a4 a5 a6 = CPPSC $ f (unCPPSC a1) (unCPPSC a2) (unCPPSC a3) (unCPPSC a4) (unCPPSC a5) (unCPPSC a6)

liftA7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> CppSrcCode a -> CppSrcCode b -> CppSrcCode c -> CppSrcCode d -> CppSrcCode e -> CppSrcCode f -> CppSrcCode g -> CppSrcCode h
liftA7 f a1 a2 a3 a4 a5 a6 a7 = CPPSC $ f (unCPPSC a1) (unCPPSC a2) (unCPPSC a3) (unCPPSC a4) (unCPPSC a5) (unCPPSC a6) (unCPPSC a7)

liftList :: ([a] -> b) -> [CppSrcCode a] -> CppSrcCode b
liftList f as = CPPSC $ f (map unCPPSC as)

lift1List :: (a -> [b] -> c) -> CppSrcCode a -> [CppSrcCode b] -> CppSrcCode c
lift1List f a as = CPPSC $ f (unCPPSC a) (map unCPPSC as)

unCPPSCPair :: (CppSrcCode a, CppSrcCode b) -> (a, b)
unCPPSCPair (a1, a2) = (unCPPSC a1, unCPPSC a2) 

lift4Pair :: (a -> b -> c -> d -> [(e, f)] -> g) -> CppSrcCode a -> CppSrcCode b -> CppSrcCode c -> CppSrcCode d -> [(CppSrcCode e, CppSrcCode f)] -> CppSrcCode g
lift4Pair f a1 a2 a3 a4 as = CPPSC $ f (unCPPSC a1) (unCPPSC a2) (unCPPSC a3) (unCPPSC a4) (map unCPPSCPair as)

lift3Pair :: (a -> b -> c -> [(d, e)] -> f) -> CppSrcCode a -> CppSrcCode b -> CppSrcCode c -> [(CppSrcCode d, CppSrcCode e)] -> CppSrcCode f
lift3Pair f a1 a2 a3 as = CPPSC $ f (unCPPSC a1) (unCPPSC a2) (unCPPSC a3) (map unCPPSCPair as)

liftPairFst :: (CppSrcCode a, b) -> CppSrcCode (a, b)
liftPairFst (c, n) = CPPSC $ (unCPPSC c, n)

liftTripFst :: (CppSrcCode a, b, c) -> CppSrcCode (a, b, c)
liftTripFst (c, n, b) = CPPSC $ (unCPPSC c, n, b)

instance PackageSym CppSrcCode where
    type Package CppSrcCode = ([(Doc, Label, Bool)], Label)
    packMods n ms = liftPairFst (sequence ms, n)

instance RenderSym CppSrcCode where
    type RenderFile CppSrcCode = (Doc, Label, Bool)
    fileDoc code = liftTripFst (liftA3 fileDoc' (top code) (fmap tripFst code) bottom, tripSnd $ unCPPSC code, tripThird $ unCPPSC code)
    top m = liftA3 cppstop m (include "") (list dynamic) endStatement
    bottom = return empty

instance KeywordSym CppSrcCode where
    type Keyword CppSrcCode = Doc
    endStatement = return semi
    endStatementLoop = return empty

    include _ = return $ text "#include"
    inherit = return colon

    list _ = return $ text "vector"
    argsList = return $ text "argv"
    listObj = return empty

    blockStart = return lbrace
    blockEnd = return rbrace

    ifBodyStart = blockStart
    elseIf = return $ text "else if"
    
    iterForEachLabel = return empty
    iterInLabel = return empty

    commentStart = return doubleSlash
    
    printFunc = return $ text "std::cout"
    printLnFunc = return $ text "std::cout"
    printFileFunc v = v -- is this right?
    printFileLnFunc v = v

instance PermanenceSym CppSrcCode where
    type Permanence CppSrcCode = Doc
    static = return staticDocD
    dynamic = return dynamicDocD

instance BodySym CppSrcCode where
    type Body CppSrcCode = Doc
    body = liftList bodyDocD
    bodyStatements = block
    oneLiner s = bodyStatements [s]

    addComments s = liftA2 (addCommentsDocD s) commentStart

instance BlockSym CppSrcCode where
    type Block CppSrcCode = Doc
    block sts = (lift1List blockDocD endStatement (map (fmap fst) (map state sts)))

instance StateTypeSym CppSrcCode where
    type StateType CppSrcCode = Doc
    bool = return $ cppBoolTypeDoc
    int = return $ intTypeDocD
    float = return $ cppFloatTypeDoc
    char = return $ charTypeDocD
    string = return $ stringTypeDocD
    infile = return $ cppInfileTypeDoc
    outfile = return $ cppOutfileTypeDoc
    listType p st = liftA2 listTypeDocD st (list p)
    intListType p = listType p int
    floatListType p = listType p float
    boolListType = listType p bool
    obj t = return $ typeDocD t
    enumType t = return $ typeDocD t
    iterator t = fmap cppIterTypeDoc (listType dynamic t)

-- ControlBlockSym Not translated yet
instance ControlBlockSym CppSrcCode where
    runStrategy l strats rv av = 
        case Map.lookup l (Map.fromList strats) of Nothing -> error $ "Strategy '" ++ l ++ "': RunStrategy called on non-existent strategy."
                                                   Just b  -> liftA2 stratDocD b (state resultState)
        where resultState = case av of Nothing    -> return (empty, False)
                                       Just vari  -> case rv of Nothing  -> error $ "Strategy '" ++ l ++ "': Attempt to assign null return to a Value."
                                                                Just res -> assign vari res

    listSlice t vnew vold b e s = 
        let l_temp = "temp"
            v_temp = var l_temp
            l_i = "i_temp"
            v_i = var l_i
        in
        block [
            (listDec l_temp 0 t),
            for (varDecDef l_i int (getB b)) (v_i ?< getE e) (getS s v_i)
                (oneLiner $ valState $ v_temp $. (listAppend (vold $. (listAccess v_i)))),
            (vnew &= v_temp)]
        where getB Nothing = litInt 0
              getB (Just n) = n
              getE Nothing = vold $. listSize
              getE (Just n) = n
              getS Nothing v = (&++) v
              getS (Just n) v = v &+= n

    stringSplit d vnew s = let l_ss = "ss"
                               v_ss = var l_ss
                               l_word = "word"
                               v_word = var l_word
                           in
        block [
            valState $ vnew $. (funcApp "clear" []),
            varDec l_ss (obj "std::stringstream"),
            valState $ objMethodCall v_ss "str" [s]
            varDec l_word string,
            while (funcApp "std::getline" [v_ss, v_word, litChar d]) (oneLiner $ valState $ vnew $. (listAppend v_word))
        ]

instance UnaryOpSym CppSrcCode where
    type UnaryOp CppSrcCode = Doc
    notOp = return $ notOpDocD
    negateOp = return $ negateOpDocD
    sqrtOp = return $ sqrtOpDocD
    absOp = return $ absOpDocD
    logOp = return $ text "log10"
    lnOp = return $ text "log"
    expOp = return $ expOpDocD
    sinOp = return $ sinOpDocD
    cosOp = return $ cosOpDocD
    tanOp = return $ tanOpDocD
    asinOp = return $ asinOpDocD
    acosOp = return $ acosOpDocD
    atanOp = return $ atanOpDocD
    floorOp = return $ text "floor"
    ceilOp = return $ text "ceil"

instance BinaryOpSym CppSrcCode where
    type BinaryOp CppSrcCode = Doc
    equalOp = return $ equalOpDocD
    notEqualOp = return $ notEqualOpDocD
    greaterOp = return $ greaterOpDocD
    greaterEqualOp = return $ greaterEqualOpDocD
    lessOp = return $ lessOpDocD
    lessEqualOp = return $ lessEqualOpDocD
    plusOp = return $ plusOpDocD
    minusOp = return $ minusOpDocD
    multOp = return $ multOpDocD
    divideOp = return $ divideOpDocD
    powerOp = return $ powerOpDocD
    moduloOp = return $ moduloOpDocD
    andOp = return $ andOpDocD
    orOp = return $ orOpDocD

instance ValueSym CppSrcCode where
    type Value CppSrcCode = Doc
    litTrue = return $ litTrueD
    litFalse = return $ litFalseD
    litChar c = return $ litCharD c
    litFloat v = return $ litFloatD v
    litInt v = return $ litIntD v
    litString s = return $ litStringD s

    defaultChar = return $ defaultCharD
    defaultFloat = return $ defaultFloatD
    defaultInt = return $ defaultIntD
    defaultString = return $ defaultStringD
    defaultBool = litFalse

    ($->) = objVar
    ($:) = enumElement

    const = var
    var n = return $ varDocD n
    extVar _ = var
    self = return $ selfDocD
    arg n = liftA2 argDocD (litInt (n+1)) argsList
    enumElement _ e = return $ text e
    enumVar = var
    objVar = liftA2 objVarDocD
    objVarSelf = var
    listVar n _ = var n
    n `listOf` t = listVar n t
    iterVar l = return $ text $ "(*" ++ l ++ ")"
    
    inputFunc = return $ text "std::cin"

    valName v = unCPPSC $ fmap render v

instance NumericExpression CppSrcCode where
    (#~) = liftA2 unOpDocD negateOp
    (#/^) = liftA2 unOpDocD sqrtOp
    (#|) = liftA2 unOpDocD absOp
    (#+) = liftA3 binOpDocD plusOp
    (#-) = liftA3 binOpDocD minusOp
    (#*) = liftA3 binOpDocD multOp
    (#/) = liftA3 binOpDocD divideOp
    (#%) = liftA3 binOpDocD moduloOp
    (#^) = liftA3 binOpDocD' powerOp

    log = liftA2 unOpDocD logOp
    ln = liftA2 unOpDocD lnOp
    exp = liftA2 unOpDocD expOp
    sin = liftA2 unOpDocD sinOp
    cos = liftA2 unOpDocD cosOp
    tan = liftA2 unOpDocD tanOp
    csc v = (litFloat 1.0) #/ (sin v)
    sec v = (litFloat 1.0) #/ (cos v)
    cot v = (litFloat 1.0) #/ (tan v)
    arcsin = liftA2 unOpDocD asinOp
    arccos = liftA2 unOpDocD acosOp
    arctan = liftA2 unOpDocD atanOp
    floor = liftA2 unOpDocD floorOp
    ceil = liftA2 unOpDocD ceilOp

instance BooleanExpression CppSrcCode where
    (?!) = liftA2 unOpDocD notOp
    (?&&) = liftA3 binOpDocD andOp
    (?||)= liftA3 binOpDocD orOp

    (?<) = liftA3 binOpDocD lessOp
    (?<=) = liftA3 binOpDocD lessEqualOp
    (?>) = liftA3 binOpDocD greaterOp
    (?>=) = liftA3 binOpDocD greaterEqualOp
    (?==) = liftA3 binOpDocD equalOp
    (?!=) = liftA3 binOpDocD notEqualOp
   
instance ValueExpression CppSrcCode where
    inlineIf = liftA3 inlineIfDocD
    funcApp n = liftList (funcAppDocD n)
    selfFuncApp = funcApp
    extFuncApp _ = funcApp
    stateObj t vs = liftA2 cppStateObjDoc t (liftList callFuncParamList vs)
    extStateObj _ = stateObj
    listStateObj = stateObj

    exists = notNull
    notNull v = v

instance Selector CppSrcCode where
    objAccess = liftA2 objAccessDocD
    ($.) = objAccess

    objMethodCall o f ps = objAccess o (func f ps)
    objMethodCallVoid o f = objMethodCall o f []

    selfAccess = objAccess self

    listPopulateAccess _ _ = return empty
    listSizeAccess v = objAccess v listSize

    listIndexExists v i = listSizeAccess v ?> i
    argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))
    
    indexOf v = objAccess l (fmap funcDocD (funcApp "find" [l $. iterBegin, l $. iterEnd, v])) #- v $. iterBegin

    stringEqual v1 v2 = v1 ?== v2

    castObj = liftA2 castObjDocD
    castStrToFloat v = funcApp "std::stod" [v]

instance FunctionSym CppSrcCode where
    type Function CppSrcCode = Doc
    func l vs = fmap funcDocD (funcApp l vs)
    cast targT _ = fmap castDocD targT
    castListToInt = cast (listType static int) int
    get n = fmap funcDocD (funcApp (getterName n) [])
    set n v = fmap funcDocD (funcApp (setterName n) [v])

    listSize = fmap funcDocD (var "size")
    listAdd _ v = fmap funcDocD (funcApp "push_back" [v])
    listPopulateInt _ = return empty
    listPopulateFloat _ = return empty
    listPopulateChar _ = return empty
    listPopulateBool _ = return empty
    listPopulateString _ = return empty
    listAppend v = fmap funcDocD (funcApp "push_back" [v])
    listExtendInt = listAppend defaultInt 
    listExtendFloat = listAppend defaultFloat 
    listExtendChar = listAppend defaultChar 
    listExtendBool = listAppend defaultBool
    listExtendString = listAppend defaultString
    listExtendList _ = fmap cppListExtendList

    iterBegin = fmap funcDocD (funcApp "begin" [])
    iterEnd = fmap funcDocD (funcApp "end" [])

instance SelectorFunction CppSrcCode where
    listAccess v = fmap funcDocD (funcApp "at" [v])
    listSet = liftA2 cppListSetDoc

    listAccessEnum t v = listAccess (castObj (cast int t) v)
    listSetEnum t i = listSet (castObj (cast int t) i)

    at l = listAccess (var l) -- parameter should be an Integer?

instance StatementSym CppSrcCode where
    type Statement CppSrcCode = (Doc, Bool)
    assign v1 v2 = liftPairFst (liftA2 assignDocD v1 v2, True)
    assignToListIndex lst index v = valState $ lst $. listSet index v
    (&=) = assign
    (&.=) l = assign (var l)
    (&=.) v l = assign v (var l)
    (&-=) v1 v2 = v1 &= (v1 #- v2)
    (&.-=) l v = l &.= (var l #- v)
    (&+=) v1 v2 = liftPairFst (liftA2 plusEqualsDocD v1 v2, True)
    (&.+=) l v = (var l) &+= v
    (&++) v = liftPairFst (fmap plusPlusDocD v, True)
    (&.++) l = (&++) (var l)
    (&~-) v = v &= (v #- (litInt 1))
    (&.~-) l = (&~-) (var l)

    varDec l t = liftPairFst (fmap (varDecDocD l) t, True)
    varDecDef l t v = liftPairFst (liftA2 (varDecDefDocD l) t v, True)
    listDec l n t = liftPairFst (liftA2 (cppListDecDoc l) (litInt n) t, True) -- this means that the type you declare must already be a list. Not sure how I feel about this. On the bright side, it also means you don't need to pass permanence
    listDecDef l t vs = liftPairFst (liftA2 (cppListDecDefDoc l) t (callFuncParamList vs), True)
    objDecDef l t v = liftPairFst (liftA2 (objDecDefDocD l) t v, True)
    objDecNew l t vs = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t vs), True)
    extObjDecNew l _ = objDecNew l
    objDecNewVoid l t = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t []), True)
    extObjDecNewVoid l _ = objDecNewVoid l
    constDecDef l t v = liftPairFst (liftA2 (constDecDefDocD l) t v, True)

    print _ v = liftPairFst (liftA2 (cppPrintDocD False) printFunc v, True)
    printLn _ v = liftPairFst (liftA2 (cppPrintDocD True) printLnFunc v, True)
    printStr s = liftPairFst (liftA2 (cppPrintDocD False) printFunc (litString s), True)
    printStrLn s = liftPairFst (liftA2 (cppPrintDocD True) printLnFunc (litString s), True)

    -- All below still needs to be translated
    printFile f _ v = liftPairFst (liftA2 (cppPrintDocD False) (printFileFunc f) v, True)
    printFileLn f _ v = liftPairFst (liftA2 (cppPrintDocD True) (printFileLnFunc f) v, True)
    printFileStr f s = liftPairFst (liftA2 (cppPrintDocD False) (printFileFunc f) (litString s), True)
    printFileStrLn f s = liftPairFst (liftA2 (cppPrintDocD True) (printFileLnFunc f) (litString s), True)

    -- Keep this block as is for now, I think this should work. Consult CppRenderer.hs if not
    printList t v = multi [(state (printStr "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])), (state (print t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printStr "]")]
    printLnList t v = multi [(state (printStr "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])), (state (print t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printStrLn "]")]
    printFileList f t v = multi [(state (printFileStr f "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])), (state (printFile f t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printFileStr f "]")]
    printFileLnList f t v = multi [(state (printFileStr f "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])), (state (printFile f t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printFileStrLn f "]")]

    getIntInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, True)
    getFloatInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, True)
    getBoolInput _ = liftPairFst (liftA3 cppInput v inputFunc endStatement, True)
    getStringInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, True)
    getCharInput _ = liftPairFst (liftA3 cppInput v inputFunc endStatement, True)
    discardInput = liftPairFst (fmap cppDiscardInput " " inputFunc, True)

    getIntFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, True)
    getFloatFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, True)
    getBoolFileInput _ _ = liftPairFst (liftA3 cppInput v f endStatement, True)
    getStringFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, True)
    getCharFileInput _ _ = liftPairFst (liftA3 cppInput v f endStatement, True)
    discardFileInput f = liftPairFst (fmap cppDiscardInput " " f, True)

    openFileR f n = liftPairFst (liftA3 cppOpenFile "std::fstream::in" f n, True)
    openFileW f n = liftPairFst (liftA3 cppOpenFile f n "std::fstream::out", True)
    openFileA f n = liftPairFst (liftA3 cppOpenFile f n "std::fstream::app", True)
    closeFile f = valState $ objMethodCall f "close" []

    getFileInputLine f v = valState $ funcApp "std::getLine" [f, v]
    discardFileLine f = liftPairFst (fmap cppDiscardInput "\\n" f, True)

    break = return (breakDocD, True)
    continue = return (continueDocD, True)

    returnState v = liftPairFst (fmap returnDocD v, True)
    returnVar l = liftPairFst (fmap returnDocD (var l), True)

    valState v = liftPairFst (v, True)

    comment cmt = liftPairFst (fmap (commentDocD cmt) commentStart, False)

    free v = liftPairFst (fmap freeDocD v, True)

    throw errMsg = liftPairFst (fmap cppThrowDoc (litString errMsg), True)

    initState fsmName initialState = varDecDef fsmName string (litString initialState)
    changeState fsmName toState = fsmName &.= (litString toState)

    initObserverList = listDecDef observerListName
    addObserver t o = valState $ obsList $. listAdd lastelem o
        where obsList = observerListName `listOf` t
              lastelem = obsList $. listSize

    state s = liftA2 statementDocD s endStatement
    loopState s = liftA2 statementDocD s endStatementLoop
    multi = lift1List multiStateDocD endStatement

instance ControlStatementSym CppSrcCode where
    ifCond bs b = liftPairFst (lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b bs, False)
    ifNoElse bs = ifCond bs $ body []
    switch v cs c = liftPairFst (lift3Pair switchDocD (state break) v c cs, False)
    switchAsIf v cs = ifCond cases
        where cases = map (\(l, b) -> (v ?== l, b)) cs

    ifExists v ifBody = ifCond [(notNull v, ifBody)]

    for sInit vGuard sUpdate b = liftPairFst (liftA6 forDocD blockStart blockEnd (loopState sInit) vGuard (loopState sUpdate) b, False)
    forRange i initv finalv stepv = for (varDecDef i int initv) ((var i) ?< finalv) (i &.+= stepv)
    forEach l t v = for (varDecDef l (iterator t) (v $. iterBegin)) (var l ?!= v $. iterEnd) (&.++ l)
    while v b = liftPairFst (liftA4 whileDocD blockStart blockEnd v b, False)

    tryCatch tb cb = liftPairFst (liftA2 cppTryCatch tb cb, False)

    checkState l = switch (var l) 

    notifyObservers fn t ps = for initv (var index ?< (obsList $. listSize)) ((&.++) index) notify
        where obsList = observerListName `listOf` t
              index = "observerIndex"
              initv = varDecDef index int $ litInt 0
              notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps

    getFileInputAll f v = let l_line = "nextLine"
                              v_line = var l_line
                          in
        multi [varDec l_line string,
            while ((?!) (funcApp "std::getline" [f, v_line]))
                (oneLiner $ valState $ v $. (listAppend $ v_line))]

instance ScopeSym CppSrcCode where
    type Scope CppSrcCode = Doc
    private = return privateDocD
    public = return publicDocD

    includeScope s = s

instance MethodTypeSym CppSrcCode where
    type MethodType CppSrcCode = Doc
    mState t = t
    void = return voidDocD
    construct n = return $ constructDocD n

instance ParameterSym CppSrcCode where
    type Parameter CppSrcCode = Doc
    stateParam n = fmap (stateParamDocD n)
    pointerParam n = fmap (cppPointerParamDoc n)

instance MethodSym CppSrcCode where
    -- Bool is True if the method is a main method, False otherwise
    type Method CppSrcCode = (Doc, Bool)
    method n _ _ t ps b = liftPairFst (liftA5 (cppMethod n) t (liftList paramListDocD ps) b blockStart blockEnd, False)
    getMethod n t = method (getterName n) public dynamic t [] getBody
        where getBody = oneLiner $ returnState (self $-> (var n))
    setMethod setLbl paramLbl t = method (setterName setLbl) public dynamic void [(stateParam paramLbl t)] setBody
        where setBody = oneLiner $ (self $-> (var setLbl)) &=. paramLbl
    mainMethod b = liftPairFst (liftA4 (cppMainMethod "main") int b blockStart blockEnd, True)
    privMethod n = method n private dynamic
    pubMethod n = method n public dynamic
    constructor n = method n public dynamic (construct n)

    function = method

instance StateVarSym CppSrcCode where
    type StateVar CppSrcCode = Doc
    stateVar _ l s p t = liftA4 (stateVarDocD l) (includeScope s) p t endStatement
    privMVar del l = stateVar del l private dynamic
    pubMVar del l = stateVar del l public dynamic
    pubGVar del l = stateVar del l public static

instance ClassSym CppSrcCode where
    -- Bool is True if the method is a main method, False otherwise
    type Class CppSrcCode = (Doc, Bool)
    buildClass n p s vs fs = liftPairFst (liftA4 (classDocD n p) inherit s (liftList stateVarListDocD vs) (liftList methodListDocD fs), any (snd . unCPPSC) fs)
    enum n es s = liftPairFst (liftA2 (enumDocD n) (return $ enumElementsDocD es False) s, False)
    mainClass n vs fs = fmap setMain $ buildClass n Nothing public vs fs
    privClass n p = buildClass n p private
    pubClass n p = buildClass n p public

instance ModuleSym CppSrcCode where
    -- Label is module name
    -- Bool is True if the method is a main method, False otherwise
    type Module CppSrcCode = (Doc, Label, Bool)
    buildModule n _ vs ms cs = 
        case null vs && null ms of True -> liftTripFst (liftList moduleDocD cs, n, any (snd . unCPPSC) cs) 
                                   _  -> liftTripFst (liftList moduleDocD ((pubClass n 
                                        Nothing (map (liftA4 statementsToStateVars
                                        public static endStatement) vs) ms):cs), n, or [any (snd . unCPPSC) ms, any (snd . unCPPSC) cs])
    -- Note: need to print libraries here instead of in cppstop

-- helpers
isDtor :: Label -> Bool
isDtor ('~':_) = True
isDtor _ = False

-- convenience
cppHeaderExt :: Label
cppHeaderExt = ".hpp"

cppstop :: (Doc, Label, Bool) -> Doc -> Doc -> Doc -> Doc
cppstop (m, n, b) inc lst end = vcat [
    if b then empty else inc <+> doubleQuotedText (n ++ cppHeaderExt),
    blank,
    inc <+> angles (text "algorithm"),
    inc <+> angles (text "iostream"),
    inc <+> angles (text "fstream"),
    inc <+> angles (text "iterator"),
    inc <+> angles (text "string"),
    inc <+> angles (text "math.h"),
    inc <+> angles (text "sstream"),
    inc <+> angles (text "limits"),
    inc <+> angles lst,
    blank,
    usingNameSpace "std" (Just "string") end,
    usingNameSpace "std" (Just $ render lst) end,
    usingNameSpace "std" (Just "ifstream") end,
    usingNameSpace "std" (Just "ofstream") end]

usingNameSpace :: Label -> Maybe Label -> Doc -> Doc
usingNameSpace n (Just m) end = text "using" <+> text n <> colon <> colon <> text m <> end
usingNameSpace n Nothing end = text "using namespace" <+> text n <> end

cppBoolTypeDoc :: Doc
cppBoolTypeDoc = text "bool"

cppFloatTypeDoc :: Doc
cppFloatTypeDoc = text "double"

cppInfileTypeDoc :: Doc
cppInfileTypeDoc = text "ifstream"

cppOutfileTypeDoc :: Doc
cppOutfileTypeDoc = text "ofstream"

cppIterTypeDoc :: Doc -> Doc
cppIterTypeDoc t = text "std::" <> t <> text "::iterator"

cppStateObjDoc :: Doc -> Doc -> Doc
cppStateObjDoc t ps = t <> parens ps

cppListSetDoc :: Doc -> Doc -> Doc
cppListSetDoc i v = dot <> at <> parens i <+> equals <+> v

cppListDecDoc :: Label -> Doc -> Doc -> Doc
cppListDecDoc l n t = t <+> text l <> parens n

cppListDecDefDoc :: Label -> Doc -> Doc -> Doc
cppListDecDefDoc l t vs = t <+> l <> braces vs

cppPrintDocD :: Bool -> Doc -> Doc -> Doc
cppPrintDocD newLn printFn v = printFn <+> text "<<" <+> v <+> end
    where end = if newLn then text "<<" <+> text "std::endl" else empty

cppThrowDoc :: Doc -> Doc
cppThrowDoc errMsg = text "throw" <> parens errMsg

cppTryCatch :: Doc -> Doc -> Doc
cppTryCatch tb cb= vcat [
    text "try" <+> lbrace,
    oneTab $ tb,
    rbrace <+> text "catch" <+> parens (text "...") <+> lbrace,
    oneTab $ cb,
    rbrace]

cppDiscardInput :: Label -> Doc -> Doc
cppDiscardInput sep inFn = inFn <> dot <> text "ignore" <+> parens 
    (text "std::numeric_limits<std::streamsize>::max()" <> comma <+> quotes sep)

cppInput :: Doc -> Doc -> Doc -> Doc
csInput v inFn end = vcat [
    inFn <+> text ">>" <+> v <> end,
    inFn <> dot <> text "ignore (std::numeric_limits<std::streamsize>::max(), '\\n')"]

cppOpenFile :: Label -> Doc -> Doc -> Doc
cppOpenFile mode f n = f <> dot <> text "open" <> parens (n <> comma <+> text mode)

cppListExtendList :: Doc -> Doc
cppListExtendList t = dot <> text "push_back" <> parens (t <> parens (integer 0))

cppPointerParamDoc :: Label -> Doc -> Doc
cppPointerParamDoc n t = t <+> text "&" <> text n

cppMethod :: Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
cppMethod n t b bStart bEnd = vcat [ttype <+> text n <> parens ps <+> bStart,
    oneTab b,
    bEnd]
    where ttype | isDtor n = empty
                | otherwise = t

cppMainMethod :: Label -> Doc -> Doc -> Doc
cppMainMethod n t b bStart bEnd = vcat [
    t <+> text "main" <> parens (text "int argc, const char *argv[]") <+> bStart,
    oneTab b,
    blank,
    text "return 0;",
    bEnd]