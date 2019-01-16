{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Java code from an 'AbstractCode' is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.NewJavaRenderer (
    -- * Java Code Configuration -- defines syntax of all Java code
    JavaCode(..)
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.New (Label,
    RenderSym(..), KeywordSym(..), PermanenceSym(..),
    BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
    StatementSym(..), UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), 
    NumericExpression(..), BooleanExpression(..), ValueExpression(..), Selector(..), 
    FunctionSym(..), SelectorFunction(..), ScopeSym(..), MethodTypeSym(..),
    ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import Language.Drasil.Code.Imperative.NewLanguageRenderer (fileDoc',
    moduleDocD, classDocD, enumDocD, enumElementsDocD,
    multiStateDocD, blockDocD, bodyDocD, outDocD, 
    printListDocD, printFileDocD, boolTypeDocD, intTypeDocD, charTypeDocD, typeDocD, listTypeDocD, 
    voidDocD, constructDocD, stateParamDocD,
    paramListDocD, methodListDocD, stateVarDocD, stateVarListDocD, ifCondDocD, switchDocD, forDocD, 
    forEachDocD, whileDocD, stratDocD, assignDocD, plusEqualsDocD, plusPlusDocD,
    varDecDocD, varDecDefDocD, listDecDocD, objDecDefDocD, statementDocD, returnDocD,
    commentDocD, notOpDocD, negateOpDocD, unOpDocD, equalOpDocD, 
    notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
    lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
    moduloOpDocD, andOpDocD, orOpDocD, binOpDocD, binOpDocD', litTrueD, litFalseD,
    litCharD, litFloatD, litIntD, litStringD, defaultCharD, defaultFloatD, defaultIntD, 
    defaultStringD, varDocD, extVarDocD, selfDocD, argDocD, enumElemDocD, objVarDocD, 
    inlineIfDocD, funcAppDocD, extFuncAppDocD, stateObjDocD, listStateObjDocD, 
    notNullDocD, funcDocD, castDocD, objAccessDocD, 
    castObjDocD, breakDocD, continueDocD, staticDocD, dynamicDocD, privateDocD, 
    publicDocD, dot, new, forLabel, observerListName, doubleSlash, 
    addCommentsDocD, callFuncParamList, getterName, setterName)
import Language.Drasil.Code.Imperative.Helpers (angles,oneTab)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Data.Map as Map (fromList,lookup)
import Control.Applicative (Applicative, liftA, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals,
    semi, vcat, lbrace, rbrace, render, colon, isEmpty)

newtype JavaCode a = JC {unJC :: a}

instance Functor JavaCode where
    fmap f (JC x) = JC (f x)

instance Applicative JavaCode where
    pure = JC
    (JC f) <*> (JC x) = JC (f x)

instance Monad JavaCode where
    return = JC
    JC x >>= f = f x

liftA4 :: (Doc -> Doc -> Doc -> Doc -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc
liftA4 f a1 a2 a3 a4 = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4)

liftA5 :: (Doc -> Doc -> Doc -> Doc -> Doc -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc
liftA5 f a1 a2 a3 a4 a5 = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4) (unJC a5)

liftA6 :: (Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc
liftA6 f a1 a2 a3 a4 a5 a6 = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4) (unJC a5) (unJC a6)

liftA7 :: (Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc
liftA7 f a1 a2 a3 a4 a5 a6 a7 = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4) (unJC a5) (unJC a6) (unJC a7)

liftList :: ([Doc] -> Doc) -> [JavaCode Doc] -> JavaCode Doc
liftList f as = JC $ f (map unJC as)

lift1List :: (Doc -> [Doc] -> Doc) -> JavaCode Doc -> [JavaCode Doc] -> JavaCode Doc
lift1List f a as = JC $ f (unJC a) (map unJC as)

unJCPair :: (JavaCode Doc, JavaCode Doc) -> (Doc, Doc)
unJCPair (a1, a2) = (unJC a1, unJC a2) 

lift4Pair :: (Doc -> Doc -> Doc -> Doc -> [(Doc, Doc)] -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> [(JavaCode Doc, JavaCode Doc)] -> JavaCode Doc
lift4Pair f a1 a2 a3 a4 as = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4) (map unJCPair as)

lift3Pair :: (Doc -> Doc -> Doc -> [(Doc, Doc)] -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> [(JavaCode Doc, JavaCode Doc)] -> JavaCode Doc
lift3Pair f a1 a2 a3 as = JC $ f (unJC a1) (unJC a2) (unJC a3) (map unJCPair as)

liftPairFst :: (JavaCode Doc, a) -> JavaCode (Doc, a)
liftPairFst (c, n) = JC $ (unJC c, n)

instance RenderSym JavaCode where
    type RenderFile JavaCode = (Doc, Label)
    fileDoc code = liftPairFst (liftA3 fileDoc' top (return $ fst $ unJC code) bottom, snd $ unJC code)
    top = liftA3 jtop endStatement (include "") (list static)
    bottom = return empty

instance KeywordSym JavaCode where
    type Keyword JavaCode = Doc
    endStatement = return semi
    endStatementLoop = return empty

    include _ = return $ text "import"
    inherit = return $ text "extends"

    list _ = return $ text "ArrayList"
    argsList = return $ text "args"
    listObj = return new

    blockStart = return lbrace
    blockEnd = return rbrace

    ifBodyStart = blockStart
    elseIf = return $ text "else if"
    
    iterForEachLabel = return forLabel
    iterInLabel = return colon

    commentStart = return doubleSlash
    
    printFunc = return $ text "System.out.print"
    printLnFunc = return $ text "System.out.println"
    printFileFunc f = liftA (printFileDocD "print") f
    printFileLnFunc f = liftA (printFileDocD "println") f

instance PermanenceSym JavaCode where
    type Permanence JavaCode = Doc
    static = return staticDocD
    dynamic = return dynamicDocD

instance BodySym JavaCode where
    type Body JavaCode = Doc
    body bs = liftList bodyDocD bs
    bodyStatements sts = block sts
    oneLiner s = bodyStatements [s]

    addComments s b = liftA2 (addCommentsDocD s) commentStart b

instance BlockSym JavaCode where
    type Block JavaCode = Doc
    block sts = lift1List blockDocD endStatement (map state sts)

instance StateTypeSym JavaCode where
    type StateType JavaCode = Doc
    bool = return $ boolTypeDocD
    int = return $ intTypeDocD
    float = return $ jFloatTypeDocD
    char = return $ charTypeDocD
    string = return $ jStringTypeDoc
    infile = return $ jInfileTypeDoc
    outfile = return $ jOutfileTypeDoc
    listType p st = liftA2 listTypeDocD st (list p)
    intListType p = liftA jIntListTypeDoc (list p)
    floatListType p = liftA jFloatListTypeDoc (list p)
    boolListType = return jBoolListTypeDocD
    obj t = return $ typeDocD t
    enumType t = return $ typeDocD t

instance ControlBlockSym JavaCode where
    ifCond bs b = lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b bs
    ifNoElse bs = ifCond bs $ body []
    switch v cs c = lift3Pair switchDocD (state break) v c cs
    switchAsIf v cs c = ifCond cases c
        where cases = map (\(l, b) -> (v ?== l, b)) cs

    ifExists v ifBody elseBody = ifCond [(notNull v, ifBody)] elseBody

    for sInit vGuard sUpdate b = liftA6 forDocD blockStart blockEnd (loopState sInit) vGuard (loopState sUpdate) b
    forRange i initv finalv stepv b = for (varDecDef i int initv) ((var i) ?<= finalv) (i &.+= stepv) b
    forEach l t v b = liftA7 (forEachDocD l) blockStart blockEnd iterForEachLabel iterInLabel t v b
    while v b = liftA4 whileDocD blockStart blockEnd v b

    tryCatch tb cb = liftA2 jTryCatch tb cb

    checkState l cs c = switch (var l) cs c
    runStrategy l strats rv av = 
        case Map.lookup l (Map.fromList strats) of Nothing -> error $ "Strategy '" ++ l ++ "': RunStrategy called on non-existent strategy."
                                                   Just b  -> liftA2 stratDocD b (state resultState)
        where resultState = case av of Nothing    -> return empty
                                       Just vari  -> case rv of Nothing  -> error $ "Strategy '" ++ l ++ "': Attempt to assign null return to a Value."
                                                                Just res -> assign vari res
    notifyObservers fn t ps = for initv (var index ?< (obsList $. listSize)) ((&.++) index) notify
        where obsList = observerListName `listOf` t
              index = "observerIndex"
              initv = varDecDef index int $ litInt 0
              notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps

    getFileInputAll f v = while (f $. (func "hasNextLine" []))
        (oneLiner $ valState $ v $. (listAppend $ f $. (func "nextLine" [])))
    listSlice t vnew vold b e s = 
        let l_temp = "temp"
            v_temp = var l_temp
            l_i = "i_temp"
            v_i = var l_i
        in
        (body [
            block [(listDec l_temp 0 t)],
            for (varDecDef l_i (int) (getB b)) (v_i ?< getE e) (getS s v_i)
                (oneLiner $ valState $ v_temp $. (listAppend (vold $. (listAccess v_i)))),
            block [(vnew &= v_temp)]])
        where getB Nothing = litInt 0
              getB (Just n) = n
              getE Nothing = vold $. listSize
              getE (Just n) = n
              getS Nothing v = (&++) v
              getS (Just n) v = v &+= n

instance UnaryOpSym JavaCode where
    type UnaryOp JavaCode = Doc
    notOp = return $ notOpDocD
    negateOp = return $ negateOpDocD
    sqrtOp = return $ text "Math.sqrt"
    absOp = return $ text "Math.abs"
    logOp = return $ text "Math.log10"
    lnOp = return $ text "Math.log"
    expOp = return $ text "Math.exp"
    sinOp = return $ text "Math.sin"
    cosOp = return $ text "Math.cos"
    tanOp = return $ text "Math.tan"
    floorOp = return $ text "Math.floor"
    ceilOp = return $ text "Math.ceil"

instance BinaryOpSym JavaCode where
    type BinaryOp JavaCode = Doc
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
    powerOp = return $ text "Math.pow"
    moduloOp = return $ moduloOpDocD
    andOp = return $ andOpDocD
    orOp = return $ orOpDocD

instance ValueSym JavaCode where
    type Value JavaCode = Doc
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

    ($->) v vr = objVar v vr
    ($:) en e = enumElement en e

    const n = var n
    var n = return $ varDocD n
    extVar l n = return $ extVarDocD l n
    self = return $ selfDocD
    arg n = liftA2 argDocD (litInt n) argsList
    enumElement en e = return $ enumElemDocD en e
    enumVar n = var n
    objVar n1 n2 = liftA2 objVarDocD n1 n2
    objVarSelf n = var n
    listVar n _ = var n
    n `listOf` t = listVar n t
    
    inputFunc = return (parens (text "new Scanner(System.in)"))

instance NumericExpression JavaCode where
    (#~) v = liftA2 unOpDocD negateOp v
    (#/^) v = liftA2 unOpDocD sqrtOp v
    (#|) v = liftA2 unOpDocD absOp v
    (#+)  v1 v2 = liftA3 binOpDocD v1 plusOp v2
    (#-)  v1 v2 = liftA3 binOpDocD v1 minusOp v2
    (#*)  v1 v2 = liftA3 binOpDocD v1 multOp v2
    (#/)  v1 v2 = liftA3 binOpDocD v1 divideOp v2
    (#%)  v1 v2 = liftA3 binOpDocD v1 moduloOp v2
    (#^)  v1 v2 = liftA3 binOpDocD' v1 powerOp v2

    log v = liftA2 unOpDocD logOp v
    ln v = liftA2 unOpDocD lnOp v
    exp v = liftA2 unOpDocD expOp v
    sin v = liftA2 unOpDocD sinOp v
    cos v = liftA2 unOpDocD cosOp v
    tan v = liftA2 unOpDocD tanOp v
    csc v = (litFloat 1.0) #/ (sin v)
    sec v = (litFloat 1.0) #/ (cos v)
    cot v = (litFloat 1.0) #/ (tan v)
    floor v = liftA2 unOpDocD floorOp v
    ceil v = liftA2 unOpDocD ceilOp v

instance BooleanExpression JavaCode where
    (?!) v = liftA2 unOpDocD notOp v
    (?&&) v1 v2 = liftA3 binOpDocD v1 andOp v2
    (?||) v1 v2 = liftA3 binOpDocD v1 orOp v2

    (?<)  v1 v2 = liftA3 binOpDocD v1 lessOp v2
    (?<=) v1 v2 = liftA3 binOpDocD v1 lessEqualOp v2
    (?>)  v1 v2 = liftA3 binOpDocD v1 greaterOp v2
    (?>=) v1 v2 = liftA3 binOpDocD v1 greaterEqualOp v2
    (?==) v1 v2 = liftA3 binOpDocD v1 equalOp v2
    (?!=) v1 v2 = liftA3 binOpDocD v1 notEqualOp v2
    
instance ValueExpression JavaCode where
    inlineIf c v1 v2 = liftA3 inlineIfDocD c v1 v2
    funcApp n vs = liftList (funcAppDocD n) vs
    selfFuncApp = funcApp
    extFuncApp l n vs = liftList (extFuncAppDocD l n) vs
    stateObj t vs = liftA2 stateObjDocD t (liftList callFuncParamList vs)
    extStateObj _ t vs = stateObj t vs
    listStateObj t vs = liftA3 listStateObjDocD listObj t (liftList callFuncParamList vs)

    exists = notNull
    notNull v = liftA3 notNullDocD v notEqualOp (var "null")

instance Selector JavaCode where
    objAccess v f = liftA2 objAccessDocD v f
    ($.) v f = objAccess v f

    objMethodCall o f ps = objAccess o (func f ps)
    objMethodCallVoid o f = objMethodCall o f []

    selfAccess f = objAccess self f

    listPopulateAccess _ _ = return empty
    listSizeAccess v = objAccess v listSize

    listIndexExists lst index = liftA3 jListIndexExists lst greaterOp index
    argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))

    stringEqual v1 str = objAccess v1 (func "equals" [str])

    castObj f v = liftA2 castObjDocD f v
    castStrToFloat v = funcApp "Double.parseDouble" [v]

instance FunctionSym JavaCode where
    type Function JavaCode = Doc
    func l vs = liftA funcDocD (funcApp l vs)
    cast targT _ = liftA castDocD targT
    castListToInt = liftA funcDocD (funcApp "ordinal" [])
    get n = liftA funcDocD (funcApp (getterName n) [])
    set n v = liftA funcDocD (funcApp (setterName n) [v])

    indexOf v = liftA funcDocD (funcApp "indexOf" [v])

    listSize = liftA funcDocD (funcApp "size" [])
    listAdd i v = liftA funcDocD (funcApp "add" [i, v])
    listPopulateInt _ = return empty
    listPopulateFloat _ = return empty
    listPopulateChar _ = return empty
    listPopulateBool _ = return empty
    listPopulateString _ = return empty
    listAppend v = liftA funcDocD (funcApp "add" [v])
    listExtendInt = liftA jListExtend defaultInt 
    listExtendFloat = liftA jListExtend defaultFloat 
    listExtendChar = liftA jListExtend defaultChar 
    listExtendBool = liftA jListExtend defaultBool
    listExtendString = liftA jListExtend defaultString
    listExtendList t = liftA jListExtendList t

    iterBegin = liftA funcDocD (funcApp "begin" [])
    iterEnd = liftA funcDocD (funcApp "end" [])

instance SelectorFunction JavaCode where
    listAccess i = liftA funcDocD (funcApp "get" [i])
    listSet i v = liftA funcDocD (funcApp "set" [i, v])

    listAccessEnum t v = listAccess (castObj (cast int t) v)
    listSetEnum t i v = listSet (castObj (cast int t) i) v

    at l = listAccess (var l)

instance StatementSym JavaCode where
    type Statement JavaCode = Doc
    assign v1 v2 = liftA2 assignDocD v1 v2
    assignToListIndex lst index v = lst $. listSet index v
    (&=) v1 v2 = assign v1 v2
    (&.=) l v = assign (var l) v
    (&=.) v l = assign v (var l)
    (&-=) v1 v2 = v1 &= (v1 #- v2)
    (&.-=) l v = l &.= (var l #- v)
    (&+=) v1 v2 = liftA2 plusEqualsDocD v1 v2
    (&.+=) l v = (var l) &+= v
    (&++) v = liftA plusPlusDocD v
    (&.++) l = (&++) (var l)
    (&~-) v = v &= (v #- (litInt 1))
    (&.~-) l = (&~-) (var l)

    varDec l t = liftA (varDecDocD l) t
    varDecDef l t v = liftA2 (varDecDefDocD l) t v
    listDec l n t = liftA2 (listDecDocD l) (litInt n) t -- this means that the type you declare must already be a list. Not sure how I feel about this. On the bright side, it also means you don't need to pass permanence
    listDecDef l t vs = liftA2 (jListDecDef l) t (liftList callFuncParamList vs)
    objDecDef l t v = liftA2 (objDecDefDocD l) t v
    objDecNew l t vs = liftA2 (objDecDefDocD l) t (stateObj t vs)
    extObjDecNew l _ t vs = objDecNew l t vs
    objDecNewVoid l t = liftA2 (objDecDefDocD l) t (stateObj t [])
    extObjDecNewVoid l _ t = objDecNewVoid l t
    constDecDef l t v = liftA2 (jConstDecDef l) t v

    print _ v = liftA2 outDocD printFunc v
    printLn _ v = liftA2 outDocD printLnFunc v
    printStr s = liftA2 outDocD printFunc (litString s)
    printStrLn s = liftA2 outDocD printLnFunc (litString s)

    printFile f _ v = liftA2 outDocD (printFileFunc f) v
    printFileLn f _ v = liftA2 outDocD (printFileLnFunc f) v
    printFileStr f s = liftA2 outDocD (printFileFunc f) (litString s)
    printFileStrLn f s = liftA2 outDocD (printFileLnFunc f) (litString s)

    printList t v = liftA4 printListDocD (state (printStr "[")) (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. (listSize)) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])) (state (print t (v $. (listAccess ((v $. (listSize)) #- (litInt 1)))))) (printStr "]")
    printLnList t v = liftA4 printListDocD (state (printStr "[")) (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. (listSize)) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])) (state (print t (v $. (listAccess ((v $. (listSize)) #- (litInt 1)))))) (printStrLn "]")
    printFileList f t v = liftA4 printListDocD (state (printFileStr f "[")) (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. (listSize)) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])) (state (printFile f t (v $. (listAccess ((v $. (listSize)) #- (litInt 1)))))) (printFileStr f "]")
    printFileLnList f t v = liftA4 printListDocD (state (printFileStr f "[")) (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. (listSize)) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])) (state (printFile f t (v $. (listAccess ((v $. (listSize)) #- (litInt 1)))))) (printFileStrLn f "]")

    getIntInput v = liftA3 jInput (return $ text "nextInt()") v inputFunc
    getFloatInput v = liftA3 jInput (return $ text "nextDouble()") v inputFunc
    getBoolInput v = liftA3 jInput (return $ text "nextBoolean()") v inputFunc
    getStringInput v = liftA3 jInput (return $ text "nextLine()") v inputFunc
    getCharInput _ = return empty
    discardInput = liftA jDiscardInput inputFunc
    getIntFileInput f v = liftA3 jInput (return $ text "nextInt()") v f
    getFloatFileInput f v = liftA3 jInput (return $ text "nextDouble()") v f
    getBoolFileInput f v = liftA3 jInput (return $ text "nextBoolean()") v f
    getStringFileInput f v = liftA3 jInput (return $ text "nextLine()") v f
    getCharFileInput _ _ = return empty
    discardFileInput f = liftA jDiscardInput f

    openFileR f n = liftA2 jOpenFileR f n
    openFileW f n = liftA2 jOpenFileW f n
    closeFile f = valState $ objMethodCall f "close" []

    getFileInputLine f v = v &= (f $. (func "nextLine" []))
    discardFileLine f = valState $ f $. (func "nextLine" [])
    stringSplit d vnew s = liftA3 jStringSplit vnew (listType dynamic string) 
        (funcApp "Arrays.asList" [s $. (func "split" [litString [d]])])

    break = return breakDocD  -- I could have a JumpSym class with functions for "return $ text "break" and then reference those functions here?
    continue = return continueDocD

    returnState v = liftA returnDocD v
    returnVar l = liftA returnDocD (var l)

    valState v = v

    comment cmt = liftA (commentDocD cmt) commentStart

    free _ = error "Cannot free variables in Java" -- could set variable to null? Might be misleading.

    throw errMsg = liftA jThrowDoc (litString errMsg)

    initState fsmName initialState = varDecDef fsmName string (litString initialState)
    changeState fsmName toState = fsmName &.= (litString toState)

    initObserverList t os = listDecDef observerListName t os
    addObserver t o = obsList $. listAdd lastelem o
        where obsList = observerListName `listOf` t
              lastelem = obsList $. listSize

    state s = liftA2 statementDocD s endStatement
    loopState s = liftA2 statementDocD s endStatementLoop
    multi s = lift1List multiStateDocD endStatement s

instance ScopeSym JavaCode where
    type Scope JavaCode = Doc
    private = return privateDocD
    public = return publicDocD

    includeScope s = s

instance MethodTypeSym JavaCode where
    type MethodType JavaCode = Doc
    mState t = t
    void = return voidDocD
    construct n = return $ constructDocD n

instance ParameterSym JavaCode where
    type Parameter JavaCode = Doc
    stateParam n t = liftA (stateParamDocD n) t

instance MethodSym JavaCode where
    type Method JavaCode = Doc
    method n s p t ps b = liftA5 (jMethod n) s p t (liftList (paramListDocD) ps) b
    getMethod n t = method (getterName n) public dynamic t [] getBody
        where getBody = oneLiner $ returnState (self $-> (var n))
    setMethod setLbl paramLbl t = method (setterName setLbl) public dynamic void [(stateParam paramLbl t)] setBody
        where setBody = oneLiner $ (self $-> (var setLbl)) &=. paramLbl
    mainMethod b = method "main" public static void [return $ text "String[] args"] b
    privMethod n t ps b = method n private dynamic t ps b
    pubMethod n t ps b = method n public dynamic t ps b
    constructor n ps b = method n public dynamic (construct n) ps b

    function = method

instance StateVarSym JavaCode where
    type StateVar JavaCode = Doc
    stateVar _ l s p t = liftA4 (stateVarDocD l) (includeScope s) p t endStatement
    privMVar del l t = stateVar del l private dynamic t
    pubMVar del l t = stateVar del l public dynamic t
    pubGVar del l t = stateVar del l public static t

instance ClassSym JavaCode where
    type Class JavaCode = Doc
    buildClass n p s vs fs = liftA4 (classDocD n p) inherit s (liftList stateVarListDocD vs) (liftList methodListDocD fs)
    enum n es s = liftA2 (enumDocD n) (return $ enumElementsDocD es False) s
    mainClass n vs fs = buildClass n Nothing public vs fs
    privClass n p vs fs = buildClass n p private vs fs
    pubClass n p vs fs = buildClass n p public vs fs

instance ModuleSym JavaCode where
    type Module JavaCode = (Doc, Label)
    buildModule n _ vs ms cs = 
        case vs ++ ms of [] -> liftPairFst (liftList moduleDocD cs, n) 
                         _  -> liftPairFst (liftList moduleDocD ((pubClass n 
                                   Nothing (map (liftA4 jStatementsToStateVars
                                   public static endStatement) vs) ms):cs), n)

jtop :: Doc -> Doc -> Doc -> Doc
jtop end inc lst = vcat [
    inc <+> text "java.util.Arrays" <> end,
    inc <+> text "java.util.BitSet" <> end,     --TODO: only include these if they are used in the code?
    inc <+> text "java.util.Scanner" <> end,
    inc <+> text "java.io.PrintWriter" <> end,
    inc <+> text "java.io.File" <> end,
    inc <+> text ("java.util." ++ render (lst)) <> end]

jFloatTypeDocD :: Doc
jFloatTypeDocD = text "double"

jStringTypeDoc :: Doc
jStringTypeDoc = text "String"

jInfileTypeDoc :: Doc
jInfileTypeDoc = text "Scanner"

jOutfileTypeDoc :: Doc
jOutfileTypeDoc = text "PrintWriter"

jIntListTypeDoc :: Doc -> Doc
jIntListTypeDoc lst = lst <> angles (text "Integer")

jFloatListTypeDoc :: Doc -> Doc
jFloatListTypeDoc lst = lst <> angles (text "Double")

jBoolListTypeDocD :: Doc
jBoolListTypeDocD = text "BitSet"

jListDecDef :: Label -> Doc -> Doc -> Doc
jListDecDef l st vs = st <+> text l <+> equals <+> new <+> st <+> parens (listElements)
    where listElements = if isEmpty vs then empty else text "Arrays.asList" <> parens vs

jConstDecDef :: Label -> Doc -> Doc -> Doc
jConstDecDef l st v = text "final" <+> st <+> text l <+> equals <+> v

jThrowDoc :: Doc -> Doc
jThrowDoc errMsg = text "throw new" <+> text "Exception" <> parens errMsg

jTryCatch :: Doc -> Doc -> Doc
jTryCatch tb cb = vcat [
    text "try" <+> lbrace,
    oneTab $ tb,
    rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> lbrace,
    oneTab $ cb,
    rbrace]

jDiscardInput :: Doc -> Doc
jDiscardInput inFn = inFn <> dot <> text "next()"

jInput :: Doc -> Doc -> Doc -> Doc
jInput it v inFn = v <+> equals <+> parens (inFn <> dot <> it) -- Changed from original GOOL, original GOOL was wrong.

jOpenFileR :: Doc -> Doc -> Doc
jOpenFileR f n = f <+> equals <+> new <+> text "Scanner" <> parens (new <+> text "File" <> parens n)

jOpenFileW :: Doc -> Doc -> Doc
jOpenFileW f n = f <+> equals <+> new <+> text "PrintWriter" <> parens n

jListExtend :: Doc -> Doc
jListExtend v = dot <> text "add" <> parens v

jListExtendList :: Doc -> Doc
jListExtendList t = dot <> text "add" <> parens (new <+> t <> parens (empty))

jStringSplit :: Doc -> Doc -> Doc -> Doc
jStringSplit vnew t s = vnew <+> equals <+> new <+> t
    <> parens s

jMethod :: Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
jMethod n s p t ps b = vcat [
    s <+> p <+> t <+> text n <> parens ps <+> text "throws Exception" <+> lbrace,
    oneTab $ b,
    rbrace]

jListIndexExists :: Doc -> Doc -> Doc -> Doc
jListIndexExists lst greater index = parens (lst <> text ".length" <+> greater <+> index)

jStatementsToStateVars :: Doc -> Doc -> Doc -> Doc -> Doc
jStatementsToStateVars s p end v = s <+> p <+> vs <> end