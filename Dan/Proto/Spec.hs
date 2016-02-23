{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs #-}
module Spec where

import Format (FormatC)
import Unicode (Render)
import Symbol

--For writing chunks in a specification language that can be converted to TeX
infixr 5 :+:
data Spec where
  N     :: Symbol -> Spec
  Sy    :: USymb -> Spec
  S     :: String -> Spec           -- Strings, used for Descriptions in Chunks
  (:-:) :: Spec -> Spec -> Spec     -- Subscripting (Spec :-: Spec -> Spec_{Spec} in TeX)
  (:^:) :: Spec -> Spec -> Spec     -- Superscript (Spec :^: Spec -> Spec^{Spec} in TeX)
  (:+:) :: Spec -> Spec -> Spec     -- Concatenation of two Specs (e.g. delta :+: T -> deltaT)
  (:/:) :: Spec -> Spec -> Spec     -- Fractions (Spec :/: Spec -> frac{Spec}{Spec} in TeX)
  Empty :: Spec                     -- Blank
  U     :: (Render r) => r -> Spec  -- Unicode for special characters
  F     :: FormatC -> Spec -> Spec  -- Special formatting for certain symbols & special
                                    -- chars (e.g. hat, dot, etc.)

--Moving this here to avoid cyclic imports
data USymb = Unitless
           | UName Symbol
           | UProd [USymb]
           | UPow USymb Integer --can be negative, should not be 0
           | UDiv USymb USymb   --Get proper division (not neg pow)
                                --  necessary for things like J/(kg*C)
-- Language of unit equations, to define a unit relative
-- to another

--Maybe spec could become a functor/applicative/monad?
-- (if we generalize mapping somehow)
sMap :: (String->String) -> Spec -> Spec
sMap f (S a) = S (f a)
sMap f (a :-: b) = sMap f a :-: sMap f b
sMap f (a :^: b) = sMap f a :^: sMap f b
sMap f (a :+: b) = sMap f a :+: sMap f b
sMap f (a :/: b) = sMap f a :/: sMap f b
sMap _ a = a