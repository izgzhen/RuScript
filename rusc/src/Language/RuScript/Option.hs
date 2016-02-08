-- Command line options

module Language.RuScript.Option where

data Option = Option {
  _debugOpt    :: Bool
, _optimizeOpt :: Bool
, _includeOpt  :: String
} deriving (Show, Eq)

defaultOpt :: Option
defaultOpt = Option {
  _debugOpt    = False
, _optimizeOpt = False
, _includeOpt  = ""
}

parseOpt :: [String] -> Option
parseOpt as = parseOpt' as defaultOpt
    where
        parseOpt' ("-debug":args) opt = parseOpt' args $ opt { _debugOpt = True }
        parseOpt' ("-o":args)     opt = parseOpt' args $ opt { _optimizeOpt = True }
        parseOpt' ("-i":i:args)   opt = parseOpt' args $ opt { _includeOpt = i }
        parseOpt' (x:args)        opt = error $ "undefined option: " ++ show x
        parseOpt' _               opt = opt
