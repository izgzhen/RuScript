-- Command line options

module Language.RuScript.Option where

data Option = Option {
    _debugOpt :: Bool
} deriving (Show, Eq)

defaultOpt :: Option
defaultOpt = Option {
    _debugOpt = False
}

parseOpt :: [String] -> Option
parseOpt ("-debug":_) = defaultOpt { _debugOpt = True }
parseOpt _ = defaultOpt
