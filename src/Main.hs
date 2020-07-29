import GraphMod.Utils
import qualified GraphMod.Trie as Trie
import GraphMod.ModuleDiagram

import Data.Maybe(fromMaybe)
import qualified Data.Map    as Map
import System.Environment(getArgs)
import System.IO(hPutStrLn,stderr)
import System.Console.GetOpt

import Paths_graphmod (version)
import Data.Version (showVersion)
import Data.List (isPrefixOf)
import Data.List.Split

main :: IO ()
main = do xs <- getArgs
          let (fs, ms, errs) = getOpt Permute options xs
          case errs of
            [] | show_version opts ->
                  putStrLn ("graphmod " ++ showVersion version)

               | otherwise -> do
                     hsfiles <- traverse parseFile ms
                     putStrLn $ "Found " ++ show (length hsfiles) ++ " files"
                     -- WARNING: careful if you enable this one, it is so big
                     -- that it takes age to render and is mostly unusable
                     -- make_diagrams defaultOptions hsfiles
                     make_diagrams checkLayerHsToCore hsfiles
                     make_diagrams checkLayerCoreToStg hsfiles
                     make_diagrams checkLayerCoreToByteCode hsfiles
                     make_diagrams checkLayerCoreToInterface hsfiles
                     make_diagrams checkLayerStgToCmm hsfiles
                     make_diagrams checkLayerCmmToAsm hsfiles
                     make_diagrams checkLayerCmmToLlvm hsfiles
                     make_diagrams checkLayerParser hsfiles
                     make_diagrams sourceImports hsfiles
                     make_diagrams coreSourceImports hsfiles
                     make_diagrams coreToCoreSourceImports hsfiles
                     putStrLn $ "Done"
              where opts = foldr ($) default_opts fs

            _ -> hPutStrLn stderr $
                  usageInfo "usage: graphmod MODULES/PATHS" options


---------------------------------------
-- GHC specific
---------------------------------------

mk :: String -> ModName
mk s = splitOn "." s

ghcCommons :: [ModuleName]
ghcCommons = fmap mk
   [ "GHC.Data"
   , "GHC.Utils"
   , "GHC.Builtin"
   , "GHC.Config"
   , "GHC.Platform"
   , "GHC.Prelude"
   , "GHC.Types"
   ]
   
checkLayering :: ModuleName -> [ModuleName] -> ModuleName -> Import -> Bool
checkLayering layer allowed name imp =
   (layer `isPrefixOf` name)
   && checkLayeringFilter (importName imp) (layer : allowed ++ ghcCommons)

checkLayeringFilter :: ModuleName -> [ModuleName] -> Bool
checkLayeringFilter name allowed = not (any (`isPrefixOf` name) allowed)

coreToCoreSourceImports :: DepOptions
coreToCoreSourceImports = defaultOptions
   { outputFile     = "ghc_coreToCore_source_imports"
   , showNormalDeps = False
   , filterImport = checkLayering ["GHC","CoreToCore"] []
   }

coreSourceImports :: DepOptions
coreSourceImports = defaultOptions
   { outputFile     = "ghc_core_source_imports"
   , showNormalDeps = False
   , filterImport = checkLayering ["GHC","Core"] []
   }

sourceImports :: DepOptions
sourceImports = defaultOptions
   { outputFile     = "ghc_source_imports"
   , showNormalDeps = False
   }

checkLayerHsToCore :: DepOptions
checkLayerHsToCore = defaultOptions
   { outputFile = "ghc_layer_HsToCore"
   , filterImport = checkLayering ["GHC","HsToCore"] [ ["GHC","Hs"]
                                                     , ["GHC","Core"]
                                                     ]
   }

checkLayerParser :: DepOptions
checkLayerParser = defaultOptions
   { outputFile = "ghc_layer_parser"
   , filterImport = checkLayering ["GHC","Parser"] [ ["GHC","Hs"]
                                                   ]
   }

checkLayerCoreToStg :: DepOptions
checkLayerCoreToStg = defaultOptions
   { outputFile = "ghc_layer_coretostg"
   , filterImport = checkLayering ["GHC","CoreToStg"] [ ["GHC","Core"]
                                                      , ["GHC","Stg"]
                                                      ]
   }

checkLayerCoreToByteCode :: DepOptions
checkLayerCoreToByteCode = defaultOptions
   { outputFile = "ghc_layer_coretobytecode"
   , filterImport = checkLayering ["GHC","CoreToByteCode"] [ ["GHC","Core"]
                                                           , ["GHC","ByteCode"]
                                                           ]
   }

checkLayerCoreToInterface :: DepOptions
checkLayerCoreToInterface = defaultOptions
   { outputFile = "ghc_layer_coretointerface"
   , filterImport = checkLayering ["GHC","CoreToInterface"] [ ["GHC","Core"]
                                                            , ["GHC","Interface"]
                                                            ]
   }

checkLayerStgToCmm :: DepOptions
checkLayerStgToCmm = defaultOptions
   { outputFile = "ghc_layer_stgtocmm"
   , filterImport = checkLayering ["GHC","StgToCmm"] [ ["GHC","Stg"]
                                                     , ["GHC","Cmm"]
                                                     ]
   }

checkLayerCmmToAsm :: DepOptions
checkLayerCmmToAsm = defaultOptions
   { outputFile = "ghc_layer_cmmtoasm"
   , filterImport = checkLayering ["GHC","CmmToAsm"] [ ["GHC","Cmm"] ]
   }

checkLayerCmmToLlvm :: DepOptions
checkLayerCmmToLlvm = defaultOptions
   { outputFile = "ghc_layer_cmmtollvm"
   , filterImport = checkLayering ["GHC","CmmToLlvm"] [ ["GHC","Cmm"]
                                                      , ["GHC","Llvm"]
                                                      ]
   }





data Input  = File FilePath | Module ModName
              deriving Show


-- Command line options
--------------------------------------------------------------------------------
data Opts = Opts
  { inc_dirs      :: [FilePath]
  , quiet         :: Bool
  , with_missing  :: Bool
  , use_clusters  :: Bool
  , mod_in_cluster:: Bool
  , ignore_mods   :: IgnoreSet
  , collapse_quals :: Trie.Trie String Bool
    -- ^ The "Bool" tells us if we should collapse modules as well.
    -- For example, "True" says that A.B.C would collapse not only A.B.C.*
    -- but also the module A.B.C, if it exists.
  , show_version  :: Bool
  , color_scheme  :: Int
  , prune_edges   :: Bool
  , graph_size    :: String

  , use_cabal     :: Bool -- ^ should we try to use a cabal file, if any
  }

type IgnoreSet  = Trie.Trie String IgnoreSpec
data IgnoreSpec = IgnoreAll | IgnoreSome [String]  deriving Show

type OptT = Opts -> Opts

default_opts :: Opts
default_opts = Opts
  { inc_dirs        = []
  , quiet           = False
  , with_missing    = False
  , use_clusters    = True
  , mod_in_cluster  = True
  , ignore_mods     = Trie.empty
  , collapse_quals  = Trie.empty
  , show_version    = False
  , color_scheme    = 0
  , prune_edges     = False
  , graph_size      = "6,4"
  , use_cabal       = True
  }

options :: [OptDescr OptT]
options =
  [ Option ['q'] ["quiet"] (NoArg set_quiet)
    "Do not show warnings"

  , Option ['i'] []        (ReqArg add_inc "DIR")
    "Add a search directory"

  , Option ['a'] ["all"]   (NoArg set_all)
    "Add nodes for missing modules"

  , Option []    ["no-cluster"] (NoArg set_no_cluster)
    "Do not cluster directories"

  , Option []    ["no-module-in-cluster"] (NoArg set_no_mod_in_cluster)
    "Do not place modules matching a cluster's name inside it."

--   , Option ['r'] ["remove-module"] (ReqArg add_ignore_mod "NAME")
--     "Do not display module NAME"

  , Option ['R'] ["remove-qual"]   (ReqArg add_ignore_qual "NAME")
    "Do not display modules NAME.*"

  , Option ['c'] ["collapse"]   (ReqArg (add_collapse_qual False) "NAME")
    "Display modules NAME.* as one node"

  , Option ['C'] ["collapse-module"] (ReqArg (add_collapse_qual True) "NAME")
    "Display modules NAME and NAME.* as one node"

  , Option ['p'] ["prune-edges"] (NoArg set_prune)
    "Remove imports if the module is imported by another imported module"

  , Option ['d'] ["graph-dim"] (ReqArg set_size "SIZE,SIZE")
    "Set dimensions of the graph.  See the `size` attribute of graphvize."

  , Option ['s'] ["colors"] (ReqArg add_color_scheme "NUM")
    "Choose a color scheme number (0-5)"

  , Option [] ["no-cabal"] (NoArg (set_cabal False))
    "Do not use Cabal for paths and modules."

  , Option ['v'] ["version"]   (NoArg set_show_version)
    "Show the current version."
  ]

set_quiet        :: OptT
set_quiet o       = o { quiet = True }

set_show_version :: OptT
set_show_version o = o { show_version = True }

set_all          :: OptT
set_all o         = o { with_missing = True }

set_no_cluster   :: OptT
set_no_cluster o  = o { use_clusters = False }

set_no_mod_in_cluster :: OptT
set_no_mod_in_cluster o = o { mod_in_cluster = False }

add_inc          :: FilePath -> OptT
add_inc d o       = o { inc_dirs = d : inc_dirs o }

-- add_ignore_mod   :: String -> OptT
-- add_ignore_mod s o = o { ignore_mods = ins (splitModName s) }
--   where
--   ins (q,m) = Trie.insert q (upd m) (ignore_mods o)
-- 
--   upd _ (Just IgnoreAll)        = IgnoreAll
--   upd m (Just (IgnoreSome ms))  = IgnoreSome (m:ms)
--   upd m Nothing                 = IgnoreSome [m]

add_ignore_qual :: String -> OptT
add_ignore_qual s o = o { ignore_mods = Trie.insert (splitQualifier s)
                                          (const IgnoreAll) (ignore_mods o) }

add_color_scheme :: String -> OptT
add_color_scheme n o = o { color_scheme = case reads n of
                                            [(x,"")] -> x
                                            _ -> color_scheme default_opts }

add_collapse_qual :: Bool -> String -> OptT
add_collapse_qual m s o = o { collapse_quals = upd (splitQualifier s)
                                                      (collapse_quals o) }

  where
  upd [] (Trie.Sub xs (Just _)) = Trie.Sub xs (Just m)
  upd _ t@(Trie.Sub _ (Just _)) = t
  upd [] _                      = Trie.Sub Map.empty (Just m)
  upd (q:qs) (Trie.Sub as _)    = Trie.Sub (Map.alter add q as) Nothing
    where add j = Just $ upd qs $ fromMaybe Trie.empty j

set_prune :: OptT
set_prune o = o { prune_edges = True }

set_size :: String -> OptT
set_size s o = o { graph_size = s }

set_cabal :: Bool -> OptT
set_cabal on o = o { use_cabal = on }

