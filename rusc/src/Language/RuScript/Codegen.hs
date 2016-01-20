{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Language.RuScript.Codegen where

import Control.Monad.State
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Arrow
import Data.Either
import Control.Lens
import Data.Maybe (fromJust)

import Language.RuScript.AST
import Language.RuScript.ByteCode

-- The enclosed loop's entry position and exit label
type EnclosedLoop   = (Pos, Label)

-- Codegen State Monad
type Codegen        = State CodegenState

data CodegenState = CodegenState {
    _globalTable    :: M.Map Name Int,
    _bytecode       :: V.Vector ByteCode,
    _enclosed       :: Maybe EnclosedLoop,
    _labelPos       :: M.Map Label Pos,
    _labelCounter   :: Label,
    _symbolTable    :: M.Map Name Int
}

initState :: CodegenState
initState = CodegenState M.empty V.empty Nothing M.empty initLabel M.empty

makeLenses ''CodegenState

-- flatten a block into the linear address space, return
-- the next new address after the allocated space

printCode :: V.Vector ByteCode -> IO ()
printCode v = mapM_ (\(i, c) -> putStrLn $ show i ++ " : " ++ show c) $ zip [0..] (V.toList v)

runCodeGen :: ToByteCode a => a -> V.Vector ByteCode
runCodeGen a = let s = execState (flatten a) initState
               in  delabel (_bytecode s) (_labelPos s)

delabel :: V.Vector ByteCode -> M.Map Label Pos -> V.Vector ByteCode
delabel v m = V.imap f v
    where
        f i (JUMP  (Right lbl)) = JUMP  $ Left (fromJust (M.lookup lbl m) - i - 1)
        f i (JUMPF (Right lbl)) = JUMPF $ Left (fromJust (M.lookup lbl m) - i - 1)
        f i (JUMPT (Right lbl)) = JUMPT $ Left (fromJust (M.lookup lbl m) - i - 1)
        f _ other = other


class ToByteCode a where
    flatten :: a -> Codegen ()

instance ToByteCode Block where
    flatten (Branch expr b1 b2) = do
        flatten expr
        l1 <- jumpf -- JUMPF <l1>
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
        p <- getPos
        emit $ JUMP $ Left (entry - p - 1)
        mark exit


instance ToByteCode Expr where
    flatten (EVar x) = pushVar x

    flatten (EGet x attr) = do
        pushVar x
        emit $ PUSHA attr

    flatten (EInvoke x f exprs) = do
        let exprs' = reverse exprs
        flatten exprs'
        pushVar x
        emit $ INVOKE f

    flatten (ENew c exprs) = do
        flatten $ reverse exprs
        new c

    flatten (ELit lit) = do
        case lit of
            LStr s  -> emit $ PUSHSTR s
            LInt i  -> emit $ PUSHINT i
            LBool b -> if b then emit (PUSHBOOL 1) else emit (PUSHBOOL 0)

instance ToByteCode Statement where
    flatten (SVar (x, _) (Just expr)) = do
        addVar x
        flatten expr
        popVar x

    flatten (SAssign lhs expr) = do
        case lhs of
            LVar x -> flatten expr >> popVar x
            LAttr x attr -> do
                popVar x
                flatten expr
                emit $ POPA attr

    flatten (SBlock b) = flatten b
    flatten (SReturn expr) = do
        flatten expr
        emit RET
    flatten SBreak      = withLoop $ \_ exitLabel -> emit $ JUMP (Right exitLabel)

instance ToByteCode Declaration where
    flatten (FnDecl (FnSig name bindings) stmts) = do
        emit SFUNC
        flattenFunc name bindings stmts

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

instance ToByteCode a => ToByteCode [a] where
    flatten = mapM_ flatten

flattenFunc :: String -> [Binding] -> [Statement] -> Codegen ()
flattenFunc name bindings stmts = do
        nLocals <- inScope bindings $ do
            flatten stmts
            nLocals <- M.size <$> use symbolTable
            return nLocals
        emit $ EBODY nLocals

inScope :: [Binding] -> Codegen a -> Codegen a
inScope bindings gen = do
    oldTable <- use symbolTable
    symbolTable .= M.fromList (zip (map fst bindings) [0..])
    forM_ [0..(length bindings)] $ emit . POP
    ret <- gen
    symbolTable .= oldTable
    return ret

instance ToByteCode Program where
    flatten (Program mixed) = do
        let declarations = rights mixed
        let topStmts = lefts mixed
        flatten declarations
        flatten topStmts

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
jumpt = jumpGen JUMPT

jump :: Codegen Label
jump = jumpGen JUMP

jumpf :: Codegen Label
jumpf = jumpGen JUMPF

jumpGen :: (Address -> ByteCode) -> Codegen Label
jumpGen constr = do
    l <- mkLabel
    emit $ constr $ Right l
    return l

mark :: Label -> Codegen ()
mark l = getPos >>= substantiate l

getPos :: Codegen Pos
getPos = V.length <$> use bytecode

withVar :: (Int -> ByteCode) -> Name -> Codegen ()
withVar constr name = do
    t <- use symbolTable
    case M.lookup name t of
        Just idx -> emit $ constr idx
        Nothing  -> error $ "use name before declaration: " ++ show name

addVar :: Name -> Codegen ()
addVar name = symbolTable %= (\t -> M.insert name (M.size t) t)

popVar :: Name -> Codegen ()
popVar = withVar POP

pushVar :: Name -> Codegen ()
pushVar = withVar PUSH

emit :: ByteCode -> Codegen ()
emit code = bytecode %= flip V.snoc code

new :: Name -> Codegen ()
new x = indexOfClass x >>= emit . NEW

withLoop :: (Pos -> Label -> Codegen a) -> Codegen a
withLoop callback = do
    mEnclosed <- use enclosed
    case mEnclosed of
        Just (pos, lbl) -> callback pos lbl
        Nothing -> error "not in enclosed loop"

isConcrete :: Method -> Bool
isConcrete (Concrete _ _) = True
isConcrete _ = False

indexOfClass :: Name -> Codegen Int
indexOfClass name = do
    t <- use globalTable
    case M.lookup name t of
        Nothing  -> error $ "can't find class " ++ show name
        Just idx -> return idx


update' :: Int -> a -> V.Vector a -> V.Vector a
update' i a v = V.update v $ V.singleton (i, a)
