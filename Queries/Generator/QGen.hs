{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveGeneric, DeriveFoldable,
             DeriveTraversable #-}

module Main where

import Grammar
import Tokens
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List
import Test.QuickCheck

makeBaseFunctor ''Exp

-- (1) Generate SPARQL -----

generateE :: Exp -> Gen String
generateE = cata g
  where
    g (EQuantF q d bb) = do
      r2 <- generateBB bb
      r1 <- generateD d 
      return ("\n" ++ r1 ++ r2 ++ "\n")
    g (EUnOpF Id e)    = return ""
    -- g (EUnOpF Neg e)      = 
    -- g (EUnOpF Not e)      = 
    g (EUnOpF ONo e)   = do
      r <- e
      return ("\n\t\t" ++ "NOT EXISTS { " ++ "\t" ++ r ++ "\t\t} \n")
    g (EUnOpF OSome e) = do
      r <- e
      return ("\n\t\t" ++ "EXISTS { " ++ "\t" ++ r ++ "\t\t} \n")
    g (EBinOpF And el er)          = do
      r1 <- el
      r2 <- er
      return (r1 ++ "\n\t && " ++ r2)
    g (EBinOpF Or el er)           = do
      r1 <- el
      r2 <- er
      return ("(" ++ r1 ++ "\n\t || " ++ r2 ++ ")")
    -- g (EBinOpF Intersection el er) = 
    -- g (EBinOpF Iff el er)          = 
    -- g (EBinOpF Implies el er)      = 
    -- g (EBinOpF Union el er)        = 
    -- g (EBinOpF Minus el er)        = 
    g (EBinOpF Comp el er)         = do
      r1 <- el
      r2 <- er
      z  <- shuffle (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
      return ("\n\t\t " ++ ('?':r1) ++ " " ++ (':':r2) ++ " " ++ ('?':take 3 z) ++ " . " ++ "\n")
    g (ECompF Eq el er)  = do
      r1 <- el
      r2 <- er
      return (r1 ++ " = " ++ r2)
    -- g (ECompF In el er)  = 
    g (ENCompF Eq el er) = do
      r1 <- el
      r2 <- er
      return (r1 ++ " != " ++ r2)
    -- g (ENCompF In el er) = 
    g (EBlockF b)         = generateB b
    g (EParF e)           = e
    g (EWordF s)          = return s

generateD :: Decl -> Gen String
generateD (DeclW ws e)      = return $ genD (zip ws (repeat " rdf:type ")) (getType e)
generateD (DeclDW ws e)     = return $ genD (zip ws (repeat " rdf:type ")) (getType e) 
                              ++ "\n\tFILTER ( " 
                              ++ intercalate "\n\t && " (map genFilter (permutations ws)) 
                              ++ "\t)\n\n"
-- generateD (DeclWD ws e)     = 
-- generateD (DeclDWD ws e)    = 
generateD (DeclWR ws e d)   = do 
    gd <- generateD d 
    return (gd ++ genD (zip ws (repeat " rdf:type ")) (getType e))
generateD (DeclDWR ws e d)  = do
    gd <- generateD d 
    return (gd ++ genD (zip ws (repeat " rdf:type ")) (getType e) 
               ++ "\n\tFILTER ( " 
               ++ intercalate "\n\t && " (map genFilter (permutations ws)) 
               ++ "\t)\n\n")
-- generateD (DeclWDR ws e d)  =
-- generateD (DeclDWDR ws e d) = 

genD :: [(String, String)] -> String -> String
genD [] _ = []
genD ((w, r) : ts) t = "\t " ++ ('?':w) ++ r ++ (':':t) ++ " . \n" ++ genD ts t

genFilter :: [String] -> String
genFilter [h, h'] = ('?':h) ++ " != " ++ ('?':h')
genFilter (h:h':t) = ('?':h) ++ " != " ++ ('?':h') ++ "\n\t && " ++ genFilter (h':t)

getType :: Exp -> String
getType (EWord s) = s
getType _         = ""

generateBB :: BlockOrBar -> Gen String
generateBB (BlockB b) = generateB b
generateBB (BBar e)   = do
    ge <- generateE e
    return ("\n\t" ++ "FILTER ( " ++ ge ++ " \t)")

generateC :: CompareOp -> String
generateC Eq = " = "
generateC In = " in "

generateNC :: CompareOp -> String
generateNC Eq = " != "
generateNC In = " not in "

generateB :: Block -> Gen String
generateB EmptyBlock = return ""
generateB (Block e) = do
    ge <- generateE e
    return ("\n\t" ++ "FILTER ( " ++ ge ++ " \t)")

-- (2) Negate AST -----

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

-- (3) Show AST ------

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

-- (3) Main -----

prefixes :: String
prefixes = "PREFIX : <http://jcr.di.uminho.pt/m51-clav#> \n"
           ++ "PREFIX clav: <http://jcr.di.uminho.pt/m51-clav#> \n"
           ++ "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n"

main :: IO ()
main = do
    s <- getContents
    let ast = parseAlloy (scanTokens s)
        r   = showE ast
        nr  = negateE ast
        nrs = showE nr
    nrq <- generate (generateE nr)

    putStrLn "Invariante lido:"
    print r
    putStrLn "\nInvariante negado:"
    print nrs
    putStrLn "\nQuery gerada:"
    putStrLn prefixes
    putStrLn "SELECT * WHERE {\n"
    putStrLn nrq
    putStrLn "\n}"
    
