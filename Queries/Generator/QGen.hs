{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveGeneric, DeriveFoldable,
             DeriveTraversable #-}

module Main where

import Grammar
import Tokens
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List

makeBaseFunctor ''Exp

-- (1) Negate AST -----

negateE :: Exp -> Exp
negateE = cata g
  where
    g (EQuantF q d bb)        = EQuant (negateQ q) (negateD d) (negateBB bb)
    g (EUnOpF uop e)          = EUnOp (negateUOP uop) e
    g (EBinOpF Implies el er) = EBinOp And (negateE el) er
    g (EBinOpF bop el er)     = EBinOp (negateBOP bop) el er
    g (ECompF cop el er)      = ENComp cop el er
    g (ENCompF cop el er)     = EComp cop el er
    g (EBlockF b)             = EBlock (negateB b)
    g (EParF e)               = EPar e
    g (EWordF s)              = EWord s

negateQ :: Quant -> Quant
negateQ All = QSome
negateQ QNo = QSome
negateQ QSome = QNo

negateD :: Decl -> Decl
negateD (DeclW ws e)      = DeclW ws (negateE e)
negateD (DeclWD ws e)     = DeclWD ws (negateE e)
negateD (DeclDW ws e)     = DeclDW ws (negateE e)
negateD (DeclDWD ws e)    = DeclDWD ws (negateE e)
negateD (DeclWR ws e d)   = DeclWR ws (negateE e) (negateD d)
negateD (DeclWDR ws e d)  = DeclWDR ws (negateE e) (negateD d)
negateD (DeclDWR ws e d)  = DeclDWR ws (negateE e) (negateD d)
negateD (DeclDWDR ws e d) = DeclDWDR ws (negateE e) (negateD d)

negateBB :: BlockOrBar -> BlockOrBar
negateBB (BlockB b) = BlockB (negateB b)
negateBB (BBar e)   = BBar (negateE e)

negateUOP :: UnOp -> UnOp
negateUOP Neg   = Id
negateUOP Id    = Neg
negateUOP Not   = OSome
negateUOP ONo   = OSome
negateUOP OSome = ONo

negateBOP :: BinOp -> BinOp
negateBOP Or           = And
negateBOP And          = Or
negateBOP Intersection = Union
--negateBOP Implies    = Implies // Ver linha 21
negateBOP Union        = Intersection
negateBOP Iff          = Iff
negateBOP Minus        = Minus
negateBOP Comp         = Comp

negateB :: Block -> Block
negateB EmptyBlock = EmptyBlock
negateB (Block e) = Block (negateE e)

-- (2) Show AST ------

showE :: Exp -> String
showE = cata g
  where
    g (EQuantF q d bb)    = showQ q ++ " " ++ showD d ++ " " ++ showBB bb
    g (EUnOpF uop e)      = showUOP uop ++ e
    g (EBinOpF bop el er) = el ++ showBOP bop  ++ er
    g (ECompF cop el er)  = el ++ showC cop ++ er
    g (ENCompF cop el er) = el ++ showNC cop ++ er
    g (EBlockF b)         = showB b
    g (EParF e)           = "(" ++ e ++ ")"
    g (EWordF s)          = s

showQ :: Quant -> String
showQ All = "all"
showQ QNo = "no"
showQ QSome = "some"

showD :: Decl -> String
showD (DeclW ws e)      = intercalate "," ws ++ ":" ++ showE e
showD (DeclWD ws e)     = intercalate "," ws ++ ": disj " ++ showE e
showD (DeclDW ws e)     = "disj " ++ intercalate "," ws ++ ":" ++ showE e
showD (DeclDWD ws e)    = "disj " ++ intercalate "," ws ++ ": disj " ++ showE e
showD (DeclWR ws e d)   = showD d ++ "," ++ intercalate "," ws ++ ":" ++ showE e
showD (DeclWDR ws e d)  = showD d ++ "," ++ intercalate "," ws ++ ": disj " ++ showE e
showD (DeclDWR ws e d)  = showD d ++ "," ++ "disj " ++ intercalate "," ws ++ ":" ++ showE e
showD (DeclDWDR ws e d) = showD d ++ "," ++ "disj " ++ intercalate "," ws ++ ": disj " ++ showE e

showBB :: BlockOrBar -> String
showBB (BlockB b) = showB b
showBB (BBar e)   = "| " ++ showE e

showUOP :: UnOp -> String
showUOP Id    = ""
showUOP Neg   = "!"
showUOP Not   = "not "
showUOP ONo   = "no "
showUOP OSome = "some "

showBOP :: BinOp -> String
showBOP Or           = " or "
showBOP And          = " and "
showBOP Intersection = " & "
showBOP Iff          = " iff "
showBOP Implies      = " implies "
showBOP Union        = " + "
showBOP Minus        = " - "
showBOP Comp         = "."

showC :: CompareOp -> String
showC Eq = " = "
showC In = " in "

showNC :: CompareOp -> String
showNC Eq = " != "
showNC In = " not in "

showB :: Block -> String
showB EmptyBlock = "{}"
showB (Block e) = "{" ++ showE e ++ "}"

main :: IO ()
main = do
    s <- getContents
    let ast = parseAlloy (scanTokens s)
        r   = showE ast
        nr  = showE . negateE $ ast
    putStrLn "Invariante lido:"
    print r
    putStrLn "Invariante negado:"
    print nr
    
