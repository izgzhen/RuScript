module Language.RuScript.NameSpace (
  NameSpace
, initNameSpace
, insertName
, lookupName
, insertAnony
, collectNames
) where

import qualified Data.Map as M

data NameSpace = NameSpace {
  _map    :: M.Map String Int,
  _nextId :: Int
} deriving (Show, Eq)

initNameSpace = NameSpace M.empty 0

insertName name (NameSpace m i) = (NameSpace (M.insert name i m) (i + 1), i)

lookupName name (NameSpace m i) = M.lookup name m

insertAnony (NameSpace m i) = (NameSpace m (i + 1), i)

collectNames :: [String] -> NameSpace
collectNames = foldr (\n s -> fst $ insertName n s) initNameSpace