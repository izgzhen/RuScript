{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Codegen where

import Control.Monad.State
import qualified Data.Vector as V
import qualified Data.Map as M
import AST
import ByteCode
import Control.Arrow
import Data.Either
import Control.Lens

-- The enclosed loop's entry position and exit label
type EnclosedLoop   = (Pos, Label)

-- Codegen State Monad
type Codegen        = State CodegenState

data CodegenState = CodegenState {
    _bytecode       :: V.Vector ByteCode,
    _enclosed       :: Maybe EnclosedLoop,
    _labelPos       :: M.Map Label Pos,
    _labelCounter   :: Label,
    _symbolTable    :: M.Map Name Int
}

makeLenses ''CodegenState

-- flatten a block into the linear address space, return
-- the next new address after the allocated space

class ToByteCode a where
    -- flatten :: Pos -> a -> Codegen Pos
    flatten :: a -> Codegen ()

instance ToByteCode Block where
    -- flatten entry = \case
    --     Branch expr b1 b2 -> do
    --         a1 <- flatten entry expr
    --         a2 <- flatten (a1 + 1) b1
    --         a3 <- flatten (a2 + 1) b2
    --         write a1 $ JUMPT $ Left (a2 + 1)
    --         write a2 $ JUMP  $ Left a3
    --         return a3
    --     Loop expr b -> do
    --         a1 <- flatten entry expr
    --         exit <- mkLabel
    --         a2 <- inLoop entry exit $
    --                 flatten (a1 + 1) b
    --         write a1 $ JUMPF $ Left (a2 + 1)
    --         write a2 $ JUMP  $ Left entry
    --         substantiate exit (a2 + 1)
    --         return (a2 + 1)
    --     Linear stmts -> foldM flatten entry stmts

    flatten (Branch expr b1 b2) = do
        flatten expr
        l1 <- jumpt -- JUMPT <l1>
        flatten b1
        l2 <- jump  -- JUMP  <l2>
        mark l1     -- substantiate l1 to current position
        flatten b2
        mark l2

    flatten (Loop expr b) = do
        entry <- getPos

        flatten expr
        exit <- jumpf
        inLoop entry exit $ flatten b
        emit $ JUMP $ Left entry
        mark exit
        
    flatten (Linear stmts) = mapM_ flatten stmts

instance ToByteCode Expr where
    -- flatten p (EVar x) = pushVar p x
    -- flatten p (EGet x attr) = do
    --     p' <- pushVar p x
    --     emit $ PUSHA attr
    --     return $ p' + 1
    -- flatten p (EInvoke x f exprs) = do
    --     let exprs' = reverse exprs
    --     p' <- foldM flatten p exprs'
    --     p'' <- pushVar p' x
    --     emit $ INVOKE f
    --     return $ p'' + 1
    -- flatten p (ENew c exprs) = do
    --     let exprs' = reverse exprs
    --     p' <- foldM flatten p exprs'
    --     new p' c
    -- flatten p (ELit lit) = do
    --     case lit of
    --         LStr s  -> emit $ PUSHSTR s
    --         LInt i  -> emit $ PUSHINT i
    --         LBool b -> if b then emit (PUSHBOOL 1) else emit (PUSHBOOL 0)
    --     return $ p + 1

    flatten (EVar x) = pushVar x

    flatten (EGet x attr) = do
        pushVar x
        emit $ PUSHA attr

    flatten (EInvoke x f exprs) = do
        let exprs' = reverse exprs
        mapM_ flatten exprs'
        pushVar x
        emit $ INVOKE f

    flatten (ENew c exprs) = do
        mapM_ flatten $ reverse exprs
        new c

    flatten (ELit lit) = do
        case lit of
            LStr s  -> emit $ PUSHSTR s
            LInt i  -> emit $ PUSHINT i
            LBool b -> if b then emit (PUSHBOOL 1) else emit (PUSHBOOL 0)

instance ToByteCode Statement where
    -- flatten p (SVar (x, _) (Just expr)) =
    --     flatten p expr >>= \p' -> popVar p' x
    -- flatten p (SAssign x expr) =
    --     flatten p expr >>= \p' -> popVar p' x
    -- flatten p (SBBlock b) = flatten p b
    -- flatten p SReturn = emit RET >> return (p + 1)
    -- flatten p SBreak = withLoop $ \_ exitLabel -> do
    --     emit $ JUMP (Right exitLabel)
    --     return $ p + 1

    flatten (SVar (x, _) (Just expr)) = do
        flatten expr
        popVar x

    flatten (SAssign x expr) = do
        flatten expr
        popVar x

    flatten (SBBlock b) = flatten b
    flatten SReturn     = emit RET
    flatten SBreak      = withLoop $ \_ exitLabel -> emit $ JUMP (Right exitLabel)

instance ToByteCode Declaration where
    -- flatten p (FnDecl (FnSig name bindings) stmts) = do
    --     emit SFUNC
    --     flattenFunc (p + 1) name bindings stmts

    flatten (FnDecl (FnSig name bindings) stmts) = do
        emit SFUNC
        flattenFunc name bindings stmts

    -- flatten p (ClassDecl x mFather attrs methods) = do
    --     let concretes = filter (isConcrete . snd) methods
    --     father_idx <- case mFather of
    --         Just x  -> indexOfClass x
    --         Nothing -> return (-1)
    --     emit $ CLASS (length attrs) (length concretes) father_idx
    --     mapM_ (\(_, (s, _)) -> emit $ PUSHSTR s) attrs
    --     let p' = p + 1 + length attrs
    --     foldM (\p (_, (Concrete (FnSig name bindings) stmts)) -> do
    --                 p' <- flattenFunc p name bindings stmts
    --                 emit $ PUSHSTR name
    --                 return $ p' + 1)
    --           p' concretes

    flatten (ClassDecl x mFather attrs methods) = do
        let concretes = filter (isConcrete . snd) methods
        father_idx <- case mFather of
            Just x  -> indexOfClass x
            Nothing -> return (-1)
        emit $ CLASS (length attrs) (length concretes) father_idx
        forM_ attrs $ \(_, (s, _)) -> emit $ PUSHSTR s
        forM_ concretes $ \(_, (Concrete (FnSig name bindings) stmts)) -> do
                    flattenFunc name bindings stmts
                    emit $ PUSHSTR name

-- flattenFunc :: Pos -> String -> [Binding] -> [Statement] -> Codegen Pos
-- flattenFunc p name bindings stmts = do
--         (p', nLocals) <- inScope p bindings $ \p -> do
--             p' <- foldM flatten (p + 1) stmts
--             nLocals <- M.size <$> use symbolTable
--             return (p', nLocals)
--         emit $ EBODY nLocals
--         return $ p' + 1

flattenFunc :: String -> [Binding] -> [Statement] -> Codegen ()
flattenFunc name bindings stmts = do
        nLocals <- inScope bindings $ do
            mapM_ flatten stmts
            nLocals <- M.size <$> use symbolTable
            return nLocals
        emit $ EBODY nLocals

-- inScope :: Pos -> [Binding] -> (Pos -> Codegen a) -> Codegen a
-- inScope p bindings gen = do
--     oldTable <- use symbolTable
--     symbolTable .= M.fromList (zip (map fst bindings) [0..])
--     forM_ [0..(length bindings)] $ emit . POP
--     ret <- gen $ p + length bindings
--     symbolTable .= oldTable
--     return ret

inScope :: [Binding] -> Codegen a -> Codegen a
inScope bindings gen = do
    oldTable <- use symbolTable
    symbolTable .= M.fromList (zip (map fst bindings) [0..])
    forM_ [0..(length bindings)] $ emit . POP
    ret <- gen
    symbolTable .= oldTable
    return ret

instance ToByteCode Program where
    -- flatten p (Program mixed) = do
    --     let declarations = rights mixed
    --     let topStmts = lefts mixed
    --     p'  <- foldM flatten p declarations
    --     foldM flatten p' topStmts

    flatten (Program mixed) = do
        let declarations = rights mixed
        let topStmts = lefts mixed
        mapM_ flatten declarations
        mapM_ flatten topStmts

-- Helpers

write :: Pos -> ByteCode -> Codegen ()
write i b = bytecode %= update' i b

mkLabel :: Codegen Label
mkLabel = do
    (ret, l') <- genLabel <$> use labelCounter
    labelCounter .= l'
    return ret

substantiate :: Label -> Pos -> Codegen ()
substantiate lbl pos = labelPos %= M.insert lbl pos

inLoop :: Pos -> Label -> Codegen a -> Codegen a
inLoop pos lbl gen = do
    e <- use enclosed
    enclosed .= Just (pos, lbl)
    a <- gen
    enclosed .= e
    return a

jumpt :: Codegen Label
jumpt = undefined

jump :: Codegen Label
jump = undefined

jumpf :: Codegen Label
jumpf = undefined

mark :: Label -> Codegen ()
mark = undefined

getPos :: Codegen Pos
getPos = undefined

-- pushVar :: Pos -> Name -> Codegen Pos
-- pushVar pos name = do
--     t <- use symbolTable
--     case M.lookup name t of
--         Just idx -> emit $ PUSH idx
--         Nothing  -> do
--             let idx = M.size t
--             symbolTable .= M.insert name idx t
--             emit $ PUSH idx
--     return $ pos + 1

pushVar :: Name -> Codegen ()
pushVar name = do
    t <- use symbolTable
    case M.lookup name t of
        Just idx -> emit $ PUSH idx
        Nothing  -> do
            let idx = M.size t
            symbolTable .= M.insert name idx t
            emit $ PUSH idx

emit :: ByteCode -> Codegen ()
emit = undefined

new :: Name -> Codegen ()
new = undefined

popVar :: Name -> Codegen ()
popVar = undefined

withLoop :: (Pos -> Label -> Codegen a) -> Codegen a
withLoop = undefined

countLocalVars :: [Binding] -> [Statement] -> Int
countLocalVars = undefined


isConcrete :: Method -> Bool
isConcrete = undefined

indexOfClass :: Name -> Codegen Int
indexOfClass = undefined


update' :: Int -> a -> V.Vector a -> V.Vector a
update' i a v = V.update v $ V.singleton (i, a)
