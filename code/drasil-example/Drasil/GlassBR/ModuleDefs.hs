-- Convention used below:
-- when 'name' and 'nameCT' both appear, 'name' is the Haskell function and
-- 'nameCT' is the "Code Template" that 'name' builds.

module Drasil.GlassBR.ModuleDefs (allMods, implVars, interpY, interpZ) where

import Language.Drasil
import Language.Drasil.ShortHands
import Language.Drasil.Code (($:=), Func, FuncStmt(..), Ind(..), Mod, asExpr, 
  fdec, ffor, funcData, funcDef, junk, junkLine, listEntry, multiLine, packmod, 
  repeated, singleLine, singleton)

import Drasil.GlassBR.Unitals (charWeight, glass_type, nomThick, pbTol, 
  plateLen, plateWidth, sdx, sdy, sdz, tNT)

allMods :: [Mod]
allMods = [readTableMod, inputMod, interpMod]

-- It's a bit odd that this has to be explicitly built here...
implVars :: [QuantityDict]
implVars = [v, x_z_1, y_z_1, x_z_2, y_z_2, mat, col,
  i, j, k, z, zVector, yMatrix, xMatrix, y, arr, filename,
  y_2, y_1, x_2, x_1, x]

--from TSD.txt:

readTableMod :: Mod
readTableMod = packmod "ReadTable" [readTable]

readTable :: Func
readTable = funcData "read_table"
  [ singleLine (repeated [junk, listEntry [WithPattern] zVector]) ',',
    multiLine (repeated [listEntry [WithLine, WithPattern] xMatrix,
                         listEntry [WithLine, WithPattern] yMatrix]) ','
  ]

-----

--from defaultInput.txt:

inputMod :: Mod
inputMod = packmod "InputFormat" [inputData]

inputData :: Func
inputData = funcData "get_input"
  [ junkLine,
    singleton plateLen, singleton plateWidth, singleton nomThick,
    junkLine,
    singleton glass_type,
    junkLine,
    singleton charWeight,
    junkLine,
    singleton tNT,
    junkLine,
    singleton sdx, singleton sdy, singleton sdz,
    junkLine,
    singleton pbTol
  ]

-----

one, two :: Symbol
one = Atomic "1"
two = Atomic "2"

-- No need to be too verbose
var :: String -> Symbol -> Space -> QuantityDict
var nam = implVar nam (nounPhraseSP nam)

y_2, y_1, x_2, x_1, x :: QuantityDict
y_1  = var "y1"          (sub lY one) Real
y_2  = var "y2"          (sub lY two) Real
x_1  = var "x1"          (sub lX one) Real
x_2  = var "x2"          (sub lX two) Real
x    = var "x"            lX          Real -- = params.wtnt from mainFun.py

v, x_z_1, y_z_1, x_z_2, y_z_2, mat, col,
  i, j, k, z, zVector, yMatrix, xMatrix, y, arr, filename :: QuantityDict
v       = var "v"         lV                          Real
i       = var "i"         lI                          Natural
j       = var "j"         lJ                          Natural
k       = var "k"         (sub lK two)                Natural
y       = var "y"         lY                          Real
z       = var "z"         lZ                          Real
zVector = var "zVector" (sub lZ (Atomic "vector")) (Vect Real)
yMatrix = var "yMatrix" (sub lY (Atomic "matrix")) (Vect $ Vect Real)
xMatrix = var "xMatrix" (sub lX (Atomic "matrix")) (Vect $ Vect Real)
arr     = var "arr"       (Atomic "arr")             (Vect Real)--FIXME: temporary variable for findCT?
x_z_1   = var "x_z_1"     (sub lX (sub lZ one))      (Vect Real)
y_z_1   = var "y_z_1"     (sub lY (sub lZ one))      (Vect Real)
x_z_2   = var "x_z_2"     (sub lX (sub lZ two))      (Vect Real)
y_z_2   = var "y_z_2"     (sub lY (sub lZ two))      (Vect Real)
mat     = var "mat"       (Atomic "mat")             (Vect $ Vect Real)
col     = var "col"       (Atomic "col")             (Vect Real)
filename= var "filename"  (Atomic "filename")         String

------------------------------------------------------------------------------------------
--
-- Some semantic functions

-- Given two points (x1,y1) and (x2,y2), return the slope of the line going through them
slope :: (Fractional a) => (a, a) -> (a, a) -> a
slope (x1,y1) (x2,y2) = (y2 - y1) / (x2 - x1)

-- Given two points (x1,y1) and (x2,y2), and an x ordinate, return
-- extrapoled y on the straight line in between
onLine :: (Fractional a) => (a, a) -> (a, a) -> a -> a
onLine p1@(x1,y1) p2 x_ = 
  let m = slope p1 p2 in
  m * (x_ - x1) + y1

------------------------------------------------------------------------------------------
-- Code Template helper functions

vLook :: (HasSymbol a, HasSymbol i, HasUID a, HasUID i) => a -> i -> Expr -> Expr
vLook a i_ p = idx (sy a) (sy i_ + p)

aLook :: (HasSymbol a, HasSymbol i, HasSymbol j, HasUID a, HasUID i, HasUID j) =>
  a -> i -> j -> Expr
aLook a i_ j_ = idx (idx (sy a) (sy i_)) (sy j_)

getCol :: (HasSymbol a, HasSymbol i, HasUID a, HasUID i) => a -> i -> Expr -> Expr
getCol a_ i_ p = apply (asExpr extractColumnCT) [sy a_, sy i_ + p]

call :: Func -> [QuantityDict] -> FuncStmt
call f l = FProcCall f $ map sy l

find :: (HasUID zv, HasUID z, HasSymbol zv, HasSymbol z) => zv -> z -> Expr
find zv z_ = apply (asExpr findCT) [sy zv, sy z_]

linInterp :: [Expr] -> Expr
linInterp = apply (asExpr linInterpCT)

interpOver :: (HasUID ptx, HasUID pty, HasUID ind, HasUID vv,
  HasSymbol ptx, HasSymbol pty, HasSymbol ind, HasSymbol vv) =>
  ptx -> pty -> ind -> vv -> [Expr]
interpOver ptx pty ind vv =
  [ vLook ptx ind 0, vLook pty ind 0
  , vLook ptx ind 1, vLook pty ind 1
  , sy vv ]
------------------------------------------------------------------------------------------
-- Code Templates

-- Note how this one uses a semantic function in its body
-- But it is also 'wrong' in the sense that it assumes x_1 <= x <= x_2
linInterpCT :: Func
linInterpCT = funcDef "lin_interp" [x_1, y_1, x_2, y_2, x] Real
  [ FRet $ onLine (sy x_1, sy y_1) (sy x_2, sy y_2) (sy x) ]

findCT :: Func
findCT = funcDef "find" [arr, v] Natural
  [
    ffor i (sy i $< (dim (sy arr) - 1))
      [ FCond ((vLook arr i 0 $<= (sy v)) $&& ((sy v) $<= vLook arr i 1))
        [ FRet $ sy i ] [] ],
    FThrow "Bound error"
  ]

extractColumnCT :: Func
extractColumnCT = funcDef "extractColumn" [mat, j] (Vect Real)
  [
    fdec col,
    --
    ffor i (sy i $< dim (sy mat))
      [ FAppend (sy col) (aLook mat i j) ],
    FRet (sy col)
  ]

interpY :: Func
interpY = funcDef "interpY" [filename, x, z] Real
  [
  -- hack
  fdec xMatrix,
  fdec yMatrix,
  fdec zVector,
  --
  call readTable [filename, zVector, xMatrix, yMatrix],
  -- endhack
    i     $:= find zVector z,
    x_z_1 $:= getCol xMatrix i 0,
    y_z_1 $:= getCol yMatrix i 0,
    x_z_2 $:= getCol xMatrix i 1,
    y_z_2 $:= getCol yMatrix i 1,
    FTry
      [ j $:= find x_z_1 x,
        k $:= find x_z_2 x ]
      [ FThrow "Interpolation of y failed" ],
    y_1 $:= linInterp (interpOver x_z_1 y_z_1 j x),
    y_2 $:= linInterp (interpOver x_z_2 y_z_2 k x),
    FRet $ linInterp [ vLook zVector i 0, sy y_1, vLook zVector i 1, sy y_2, sy z ]
  ]

interpZ :: Func
interpZ = funcDef "interpZ" [filename, x, y] Real
  [
    -- hack
  fdec xMatrix,
  fdec yMatrix,
  fdec zVector,
  --
  call readTable [filename, zVector, xMatrix, yMatrix],
  -- endhack
    ffor i (sy i $< (dim (sy zVector) - 1))
      [
        x_z_1 $:= getCol xMatrix i 0,
        y_z_1 $:= getCol yMatrix i 0,
        x_z_2 $:= getCol xMatrix i 1,
        y_z_2 $:= getCol yMatrix i 1,
        FTry
          [ j $:= find x_z_1 x,
            k $:= find x_z_2 x ]
          [ FContinue ],
        y_1 $:= linInterp (interpOver x_z_1 y_z_1 j x),
        y_2 $:= linInterp (interpOver x_z_2 y_z_2 k x),
        FCond ((sy y_1 $<= sy y) $&& (sy y $<= sy y_2))
          [ FRet $ linInterp [ sy y_1, vLook zVector i 0, sy y_2, vLook zVector i 1, sy y ]
          ] []
      ],
    FThrow "Interpolation of z failed"
  ]

interpMod :: Mod
interpMod = packmod "Interpolation" [linInterpCT, findCT, extractColumnCT, interpY, interpZ]
