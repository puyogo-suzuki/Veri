{-# LANGUAGE ScopedTypeVariables #-}
module Veri.Alpha where

import Control.Monad.State
import Veri.AST
import Data.HashMap.Strict as HM
import Debug.Trace

alphaEquivalent :: Epsilon -> Epsilon -> Bool
alphaEquivalent es1 es2 =
    trace ("\n" ++ show es1' ++ "\n" ++ show es2') equiv es1' es2'
    where
        es1' = evalState (conv es1) (HM.empty, 0)
        es2' = evalState (conv es2) (HM.empty, 0)

register :: VariableIdent -> State ((HM.HashMap VariableIdent VariableIdent), Int) VariableIdent
register vi = do
    (dic, i) <- get
    put (HM.insert vi (show i) dic, i + 1)
    return $ show i

look :: VariableIdent -> State ((HM.HashMap VariableIdent VariableIdent), Int) VariableIdent
look vi = do
    dic <- gets fst
    case HM.lookup vi dic of
        Just ret -> return ret
        Nothing -> register vi

conv :: Epsilon -> State ((HM.HashMap VariableIdent VariableIdent), Int) Epsilon
conv (Variable v) = Variable <$>look v
conv Asterisk = return Asterisk
conv Sort = return Sort
conv (App e1 e2) = App <$> conv e1 <*> conv e2
conv (Lambda v e1 e2) = do
    e1' <- conv e1
    v' <- register v
    e2' <- conv e2
    return $ Lambda v' e1' e2'
conv (Pi v e1 e2) = do
    e1' <- conv e1
    v' <- register v
    e2' <- conv e2
    return $ Pi v' e1' e2'
conv (Const s es) = Const s <$> mapM conv es

equiv :: Epsilon -> Epsilon -> Bool
equiv (Variable v1) (Variable v2) = v1 == v2
equiv Asterisk Asterisk = True
equiv Sort Sort = True
equiv (App e1 e2) (App e3 e4) = (equiv e1 e3) && (equiv e2 e4)
equiv (Lambda v1 e1 e2) (Lambda v2 e3 e4) = (v1 == v2) && (equiv e1 e3) && (equiv e2 e4)
equiv (Pi v1 e1 e2) (Pi v2 e3 e4) = (v1 == v2) && (equiv e1 e3) && (equiv e2 e4)
equiv (Const s1 es1) (Const s2 es2) = (s1 == s2) && (Prelude.foldl (&&) True (zipWith equiv es1 es2))
equiv _ _ = False