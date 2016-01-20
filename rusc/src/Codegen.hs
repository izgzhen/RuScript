{-# LANGUAGE LambdaCase #-}

module Codegen where

import Control.Monad.State
import qualified Data.Vector as V
import qualified Data.Map as M
import AST
import ByteCode
import Control.Arrow

-- The enclosed loop's entry position and exit label
type EnclosedLoop   = (Pos, Label)

-- Codegen State Monad
type Codegen        = State CodegenState

data CodegenState = CodegenState {
    _bytecode       :: V.Vector ByteCode,
    _enclosed       :: Maybe EnclosedLoop,
    _labelPos       :: M.Map Label Pos,
    _labelCounter   :: Label
}

-- flatten a block into the linear address space, return
-- the next new address after the allocated space

class ToByteCode a where
    flatten :: Pos -> a -> Codegen Pos

instance ToByteCode Block where
    flatten entry = \case
        Branch expr b1 b2 -> do
            a1 <- flatten entry expr
            a2 <- flatten (a1 + 1) b1
            a3 <- flatten (a2 + 1) b2
            write a1 $ JUMPT $ Left (a2 + 1)
            write a2 $ JUMP  $ Left a3
            return a3
        Loop expr b -> do
            a1 <- flatten entry expr
            exit <- mkLabel
            a2 <- inLoop entry exit $
                    flatten (a1 + 1) b
            write a1 $ JUMPF $ Left (a2 + 1)
            write a2 $ JUMP  $ Left entry
            substantiate exit (a2 + 1)
            return (a2 + 1)
        Linear stmts -> foldM flatten entry stmts

instance ToByteCode Expr where
    flatten p (EVar x) = pushVar p x
    flatten p (EGet x attr) = do
        p' <- pushVar p x
        emit $ PUSHA attr
        return $ p' + 1
    flatten p (EInvoke x f exprs) = do
        let exprs' = reverse exprs
        p' <- foldM flatten p exprs'
        p'' <- pushVar p' x
        emit $ INVOKE f
        return $ p'' + 1
    flatten p (ENew c exprs) = do
        let exprs' = reverse exprs
        p' <- foldM flatten p exprs'
        new p' c
    flatten p (ELit lit) = do
        case lit of
            LStr s  -> emit $ PUSHSTR s
            LInt i  -> emit $ PUSHINT i
            LBool b -> if b then emit (PUSHBOOL 1) else emit (PUSHBOOL 0)
        return $ p + 1

instance ToByteCode Statement where
    flatten p (SVar (x, _) (Just expr)) =
        flatten p expr >>= \p' -> popVar p' x
    flatten p (SAssign x expr) =
        flatten p expr >>= \p' -> popVar p' x
    flatten p (SBBlock b) = flatten p b
    flatten p SReturn = emit RET >> return (p + 1)
    flatten p SBreak = withLoop $ \_ exitLabel -> do
        emit $ JUMP (Right exitLabel)
        return $ p + 1

instance ToByteCode Declaration where
    flatten p (FnDecl (FnSig name bindings) stmts) = do
        emit SFUNC
        p' <- foldM flatten (p + 1) stmts
        let n = countLocalVars bindings stmts
        emit $ EBODY n
        return $ p' + 1
    flatten p (ClassDecl x mFather attrs methods) = do
        let concretes = filter (isConcrete . snd) methods
        father_idx <- case mFather of
            Just x  -> indexOfClass x
            Nothing -> return (-1)
        emit $ CLASS (length attrs) (length concretes) father_idx
        mapM_ (\(_, (s, _)) -> emit $ PUSHSTR s) attrs
        let p' = p + 1 + length attrs
        foldM (\p (_, (Concrete (FnSig name bindings) stmts)) -> do
                    emit SFUNC
                    p' <- foldM flatten (p + 1) stmts
                    emit $ EBODY (countLocalVars bindings stmts)
                    emit $ PUSHSTR name
                    return $ p' + 2) p' concretes

-- Helpers

write :: Pos -> ByteCode -> Codegen ()
write = undefined

mkLabel :: Codegen Label
mkLabel = undefined

substantiate :: Label -> Pos -> Codegen ()
substantiate = undefined

inLoop :: Pos -> Label -> Codegen a -> Codegen a
inLoop = undefined


pushVar :: Pos -> Name -> Codegen Pos
pushVar = undefined

emit :: ByteCode -> Codegen ()
emit = undefined

new :: Pos -> Name -> Codegen Pos
new = undefined

popVar :: Pos -> Name -> Codegen Pos
popVar = undefined

withLoop :: (Pos -> Label -> Codegen a) -> Codegen a
withLoop = undefined

countLocalVars :: [Binding] -> [Statement] -> Int
countLocalVars = undefined


isConcrete :: Method -> Bool
isConcrete = undefined

indexOfClass :: Name -> Codegen Int
indexOfClass = undefined


