{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module StraightLine where


import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.Maybe
import Data.List


type Id = String


data Binop = Plus | Minus | Times | Div
  deriving (Show)


data Stm where
  CompoundStm :: Stm -> Stm -> Stm
  AssignStm :: Id -> Exp -> Stm
  PrintStm :: [Exp] -> Stm
  deriving (Show)


data Exp where
  IdExp :: Id -> Exp
  NumExp :: Int -> Exp
  OpExp :: Exp -> Binop -> Exp -> Exp
  EseqExp :: Stm -> Exp -> Exp
  deriving (Show)


maxargs :: Stm -> Int
maxargs (PrintStm es) = maximum $ length es : map maxargs_exp es
maxargs (CompoundStm s1 s2) = max (maxargs s1) (maxargs s2)
maxargs (AssignStm _ e) = maxargs_exp e


maxargs_exp :: Exp -> Int
maxargs_exp (OpExp e1 _ e2) = max (maxargs_exp e1) (maxargs_exp e2)
maxargs_exp (EseqExp stm e) = max (maxargs stm) (maxargs_exp e)
maxargs_exp _ = 0


prog :: Stm
prog = CompoundStm (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3))) $
       CompoundStm (AssignStm "b" (EseqExp (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                                           (OpExp (NumExp 10) Times (IdExp "a")))) $
       PrintStm [IdExp "b"]


interp :: Stm -> IO ()
interp prog = evalStateT (interpStm prog) M.empty


type Table = M.Map Id Int
type Interpreter a = StateT Table IO a


interpStm :: Stm -> Interpreter ()

interpStm (CompoundStm s1 s2) = do interpStm s1
                                   interpStm s2

interpStm (AssignStm var e) = do v <- interpExp e
                                 modify $ M.insert var v

interpStm (PrintStm es) = do vs <- mapM interpExp es
                             liftIO $ putStrLn $ intercalate " " (map show vs)


interpExp :: Exp -> Interpreter Int

interpExp (IdExp i)
  = gets (fromMaybe 0 . M.lookup i)

interpExp (NumExp n)
  = return n

interpExp (OpExp e1 op e2)
  = do v1 <- interpExp e1
       v2 <- interpExp e2

       return $ (binopDenote op) v1 v2

interpExp (EseqExp stm e)
  = do interpStm stm
       interpExp e


binopDenote :: Binop -> (Int -> Int -> Int)
binopDenote Plus = (+)
binopDenote Minus = (-)
binopDenote Times = (*)
binopDenote Div = div
