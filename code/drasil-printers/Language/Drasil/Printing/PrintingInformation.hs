{-# LANGUAGE TemplateHaskell #-}

module Language.Drasil.Printing.PrintingInformation where

import Control.Lens (makeLenses, Lens')

import Language.Drasil (ChunkDB, HasSymbolTable(..)
  , HasTermTable(..), HasDefinitionTable(..)
  , HasUnitTable(..)
  )

data Notation = Scientific
              | Engineering
			  
data ExprFormat = MathJax
                | Html
				
class HasPrintingOptions c where
    getSetting :: Lens' c Notation
    getOption :: Lens' c ExprFormat
	
data PrintingConfiguration = PC { _notation :: Notation 
                                }
makeLenses ''PrintingConfiguration 

instance HasPrintingOptions  PrintingConfiguration where getSetting = notation



data PrintingInformation = PI
                         { _ckdb :: ChunkDB
                         , _configuration :: PrintingConfiguration
						 , _exprformat :: ExprFormat
                         }
makeLenses ''PrintingInformation

instance HasSymbolTable      PrintingInformation where symbolTable  = ckdb . symbolTable
instance HasTermTable        PrintingInformation where termTable    = ckdb . termTable
instance HasDefinitionTable  PrintingInformation where defTable     = ckdb . defTable
instance HasUnitTable        PrintingInformation where unitTable    = ckdb . unitTable
instance HasPrintingOptions  PrintingInformation where 
  getSetting  = configuration . getSetting
  getOption = exprformat
  
defaultConfiguration :: PrintingConfiguration
defaultConfiguration = PC Engineering
