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
import Data.Map (Map)
import Data.List.Split

main :: IO ()
main = do xs <- getArgs
          let (fs, ms, errs) = getOpt Permute options xs
          case errs of
            [] | show_version opts ->
                  putStrLn ("graphmod " ++ showVersion version)

               | otherwise -> do
                     hsfiles <- traverse parseFile ms
                     --make_diagrams defaultOptions hsfiles
                     make_diagrams checkLayerHaskellToCore hsfiles
                     make_diagrams checkLayerCoreToStg hsfiles
                     make_diagrams checkLayerCoreToByteCode hsfiles
                     make_diagrams checkLayerCoreToInterface hsfiles
                     make_diagrams checkLayerStgToCmm hsfiles
                     make_diagrams checkLayerCmmToAsm hsfiles
                     make_diagrams checkLayerCmmToLlvm hsfiles
                     make_diagrams sourceImports hsfiles
                     make_diagrams coreSourceImports hsfiles
                     make_diagrams coreToCoreSourceImports hsfiles
              where opts = foldr ($) default_opts fs

            _ -> hPutStrLn stderr $
                  usageInfo "usage: graphmod MODULES/PATHS" options


---------------------------------------
-- GHC specific
---------------------------------------

mk :: String -> ModName
mk s = splitOn "." s

renaming :: Map ModName ModName
renaming = Map.fromList $ fmap (\(x,y) -> (mk x, mk y))
   [ ("RnExpr", "GHC.Renamer.Expr")
   , ("RnBinds", "GHC.Rename.Binds")
   , ("RnEnv", "GHC.Rename.Env")
   , ("RnFixity", "GHC.Rename.Fixity")
   , ("RnHsDoc", "GHC.Rename.Doc")
   , ("RnNames", "GHC.Rename.Names")
   , ("RnPat", "GHC.Rename.Pat")
   , ("RnSource", "GHC.Rename.Source")
   , ("RnSplice", "GHC.Rename.Splice")
   , ("RnTypes", "GHC.Rename.Types")
   , ("RnUnbound", "GHC.Rename.Unbound")
   , ("RnUtils", "GHC.Rename.Utils")

   , ("CoreArity", "GHC.Core.Arity")
   , ("CoreFVs", "GHC.Core.FVs")
   , ("CoreLint", "GHC.Core.Lint")
   , ("CoreMap", "GHC.Core.Map")
   , ("CoreOpt", "GHC.Core.SimpleOpt")
   , ("CoreSeq", "GHC.Core.Seq")
   , ("CoreStats", "GHC.Core.Stats")
   , ("CoreSubst", "GHC.Core.Subst")
   , ("CoreSyn", "GHC.Core.Syntax")
   , ("CoreTidy", "GHC.Core.Tidy")
   , ("CoreUnfold", "GHC.Core.Unfold")
   , ("CoreUtils", "GHC.Core.Utils")
   , ("MkCore", "GHC.Core.Make")
   , ("PprCore", "GHC.Core.Pretty")
   , ("OccurAnal", "GHC.Core.OccurAnal")
   , ("Rules", "GHC.Core.Rules")

   , ("Class", "GHC.Core.Class")
   , ("CoAxiom", "GHC.Core.CoAxiom")
   , ("Coercion", "GHC.Core.Coercion")
   , ("FamInstEnv", "GHC.Core.FamInstEnv")
   , ("InstEnv", "GHC.Core.InstEnv")
   , ("Kind", "GHC.Core.Kind")
   , ("OptCoercion", "GHC.Core.OptCoercion")
   , ("TyCoFVs", "GHC.Core.TyCoFVs")
   , ("TyCon", "GHC.Core.TyCon")
   , ("TyCoPpr", "GHC.Core.TyCoPpr")
   , ("TyCoRep", "GHC.Core.TyCoRep")
   , ("TyCoSubst", "GHC.Core.TyCoSubst")
   , ("TyCoTidy", "GHC.Core.TyCoTidy")
   , ("Type", "GHC.Core.Type")
   , ("Unify", "GHC.Core.Unify")
   , ("ConLike", "GHC.Core.ConLike")
   , ("DataCon", "GHC.Core.DataCon")


   , ("CallArity", "GHC.CoreToCore.CallArity")
   , ("CoreMonad", "GHC.CoreToCore.Monad")
   , ("CSE", "GHC.CoreToCore.CSE")
   , ("Exitify", "GHC.CoreToCore.Exitify")
   , ("FloatIn", "GHC.CoreToCore.FloatIn")
   , ("FloatOut", "GHC.CoreToCore.FloatOut")
   , ("LiberateCase", "GHC.CoreToCore.LiberateCase")
   , ("SAT", "GHC.CoreToCore.StaticArgs")
   , ("SetLevels", "GHC.CoreToCore.SetLevels")
   , ("SimplCore", "GHC.CoreToCore.Simplifier")
   , ("SimplEnv", "GHC.CoreToCore.Simplify.Env")
   , ("Simplify", "GHC.CoreToCore.Simplify")
   , ("SimplMonad", "GHC.CoreToCore.Simplify.Monad")
   , ("SimplUtils", "GHC.CoreToCore.Simplify.Utils")
   , ("SpecConstr", "GHC.CoreToCore.SpecConstr")
   , ("Specialise", "GHC.CoreToCore.Specialise")
   , ("DmdAnal", "GHC.CoreToCore.DmdAnal")
   , ("WorkWrap", "GHC.CoreToCore.WorkWrap")
   , ("WwLib", "GHC.CoreToCore.WorkWrapLib")
   , ("PrelRules", "GHC.CoreToCore.ConstantFold")

   , ("BlockId", "GHC.Cmm.BlockId")
   , ("CLabel", "GHC.Cmm.CLabel")
   , ("CmmBuildInfoTables", "GHC.Cmm.BuildInfoTables")
   , ("CmmCallConv", "GHC.Cmm.CallConv")
   , ("CmmCommonBlockElim", "GHC.Cmm.CommonBlockElim")
   , ("CmmContFlowOpt", "GHC.Cmm.ContFlowOpt")
   , ("CmmExpr", "GHC.Cmm.Expr")
   , ("Cmm", "GHC.Cmm")
   , ("CmmImplementSwitchPlans", "GHC.Cmm.Switch.Implement")
   , ("CmmInfo", "GHC.Cmm.Info")
   , ("CmmLayoutStack", "GHC.Cmm.LayoutStack")
   , ("CmmLint", "GHC.Cmm.Lint")
   , ("CmmLive", "GHC.Cmm.Liveness")
   , ("CmmMachOp", "GHC.Cmm.MachOp")
   , ("CmmMonad", "GHC.Cmm.Monad")
   , ("CmmNode", "GHC.Cmm.Node")
   , ("CmmOpt", "GHC.Cmm.Opt")
   , ("CmmPipeline", "GHC.Cmm.Pipeline")
   , ("CmmProcPoint", "GHC.Cmm.ProcPoint")
   , ("CmmSink", "GHC.Cmm.Sink")
   , ("CmmSwitch", "GHC.Cmm.Switch")
   , ("CmmType", "GHC.Cmm.Type")
   , ("CmmUtils", "GHC.Cmm.Utils")
   , ("Debug", "GHC.Cmm.DebugBlock")
   , ("Hoopl.Block", "GHC.Cmm.Dataflow.Block")
   , ("Hoopl.Collections", "GHC.Cmm.Dataflow.Collections")
   , ("Hoopl.Dataflow", "GHC.Cmm.Dataflow")
   , ("Hoopl.Graph", "GHC.Cmm.Dataflow.Graph")
   , ("Hoopl.Label", "GHC.Cmm.Dataflow.Label")
   , ("MkGraph", "GHC.Cmm.Graph")
   , ("PprCmmDecl", "GHC.Cmm.Pretty.Decl")
   , ("PprCmmExpr", "GHC.Cmm.Pretty.Expr")
   , ("PprCmm", "GHC.Cmm.Pretty")
   , ("PprC", "GHC.CmmToC")

   , ("Bitmap", "GHC.Data.Bitmap")
   , ("SMRep", "GHC.RTS.Storage")
   , ("CorePrep", "GHC.CoreToStg.Prep")

   , ("Avail", "GHC.BasicTypes.Avail")
   , ("BasicTypes", "GHC.BasicTypes")
   , ("Demand", "GHC.BasicTypes.Demand")
   , ("FieldLabel", "GHC.BasicTypes.FieldLabel")
   , ("Id", "GHC.BasicTypes.Id")
   , ("IdInfo", "GHC.BasicTypes.IdInfo")
   , ("Lexeme", "GHC.BasicTypes.Lexeme")
   , ("Literal", "GHC.BasicTypes.Literal")
   , ("MkId", "GHC.BasicTypes.MkId")
   , ("Module", "GHC.BasicTypes.Module")
   , ("NameCache", "GHC.BasicTypes.NameCache")
   , ("NameEnv", "GHC.BasicTypes.NameEnv")
   , ("Name", "GHC.BasicTypes.Name")
   , ("NameSet", "GHC.BasicTypes.NameSet")
   , ("OccName", "GHC.BasicTypes.OccName")
   , ("PatSyn", "GHC.BasicTypes.PatSyn")
   , ("RdrName", "GHC.BasicTypes.RdrName")
   , ("SrcLoc", "GHC.BasicTypes.SrcLoc")
   , ("UniqSupply", "GHC.BasicTypes.UniqSupply")
   , ("Unique", "GHC.BasicTypes.Unique")
   , ("VarEnv", "GHC.BasicTypes.VarEnv")
   , ("Var", "GHC.BasicTypes.Var")
   , ("VarSet", "GHC.BasicTypes.VarSet")

   , ("Coverage", "GHC.HsToCore.Coverage")
   , ("Desugar", "GHC.HsToCore")
   , ("DsArrows", "GHC.HsToCore.Arrows")
   , ("DsBinds", "GHC.HsToCore.Binds")
   , ("DsCCall", "GHC.HsToCore.CCall")
   , ("DsExpr", "GHC.HsToCore.Expr")
   , ("DsForeign", "GHC.HsToCore.Foreign")
   , ("DsGRHSs", "GHC.HsToCore.GRHSs")
   , ("DsListComp", "GHC.HsToCore.ListComp")
   , ("DsMeta", "GHC.HsToCore.Meta")
   , ("DsMonad", "GHC.HsToCore.Monad")
   , ("DsUsage", "GHC.HsToCore.Usage")
   , ("DsUtils", "GHC.HsToCore.Utils")
   , ("ExtractDocs", "GHC.HsToCore.ExtractDocs")
   , ("MatchCon", "GHC.HsToCore.MatchCon")
   , ("Match", "GHC.HsToCore.Match")
   , ("MatchLit", "GHC.HsToCore.MatchLit")

   , ("CoreToStg", "GHC.CoreToStg")

   , ("RepType", "GHC.Stg.RepType")
   , ("SimplStg", "GHC.Stg.Simplify")
   , ("StgCse", "GHC.Stg.CSE")
   , ("StgLiftLams/Analysis", "GHC.Stg.LambdaLift.Analysis")
   , ("StgLiftLams", "GHC.Stg.LambdaLift")
   , ("StgLiftLams/LiftM", "GHC.Stg.LambdaLift.Monad")
   , ("StgLiftLams/Transformation", "GHC.Stg.LambdaLift.Trans")
   , ("StgStats", "GHC.Stg.Stats")
   , ("UnariseStg", "GHC.Stg.Unrarise")
   , ("StgFVs", "GHC.Stg.FVs")
   , ("StgLint", "GHC.Stg.Lint")
   , ("StgSubst", "GHC.Stg.Subst")
   , ("StgSyn", "GHC.Stg.Syntax")

   , ("ClsInst", "GHC.TypeCheck.ClsInst")
   , ("FamInst", "GHC.TypeCheck.FamInst")
   , ("FunDeps", "GHC.TypeCheck.FunDeps")
   , ("Inst", "GHC.TypeCheck.Inst")
   , ("TcAnnotations", "GHC.TypeCheck.Annotations")
   , ("TcArrows", "GHC.TypeCheck.Arrows")
   , ("TcBackpack", "GHC.TypeCheck.ModuleSig")
   , ("TcBinds", "GHC.TypeCheck.Binds")
   , ("TcCanonical", "GHC.TypeCheck.Canonical")
   , ("TcClassDcl", "GHC.TypeCheck.ClassDcl")
   , ("TcDefaults", "GHC.TypeCheck.Defaults")
   , ("TcDeriv", "GHC.TypeCheck.Deriv")
   , ("TcDerivInfer", "GHC.TypeCheck.DerivInfer")
   , ("TcDerivUtils", "GHC.TypeCheck.DerivUtils")
   , ("TcEnv", "GHC.TypeCheck.Env")
   , ("TcErrors", "GHC.TypeCheck.Errrors")
   , ("TcEvidence", "GHC.TypeCheck.Evidence")
   , ("TcEvTerm", "GHC.TypeCheck.EvTerm")
   , ("TcExpr", "GHC.TypeCheck.Expr")
   , ("TcFlatten", "GHC.TypeCheck.Flatten")
   , ("TcForeign", "GHC.TypeCheck.Foreign")
   , ("TcGenDeriv", "GHC.TypeCheck.GenDeriv")
   , ("TcGenFunctor", "GHC.TypeCheck.GenFunctor")
   , ("TcGenGenerics", "GHC.TypeCheck.GenGenerics")
   , ("TcHoleErrors", "GHC.TypeCheck.HoleErrors")
   , ("TcHoleFitTypes", "GHC.TypeCheck.HoleFitTypes")
   , ("TcHsSyn", "GHC.TypeCheck.HsSyn")
   , ("TcHsType", "GHC.TypeCheck.HsType")
   , ("TcInstDcls", "GHC.TypeCheck.InstDcls")
   , ("TcInteract", "GHC.TypeCheck.Interact")
   , ("TcMatches", "GHC.TypeCheck.Matches")
   , ("TcMType", "GHC.TypeCheck.MType")
   , ("TcPat", "GHC.TypeCheck.Pat")
   , ("TcPatSyn", "GHC.TypeCheck.PatSyn")
   , ("TcPluginM", "GHC.TypeCheck.PluginM")
   , ("TcRnDriver", "GHC.TypeCheck.RnDriver")
   , ("TcRnExports", "GHC.TypeCheck.RnExports")
   , ("TcRnMonad", "GHC.TypeCheck.RnMonad")
   , ("TcRnTypes", "GHC.TypeCheck.RnTypes")
   , ("TcRules", "GHC.TypeCheck.Rules")
   , ("TcSigs", "GHC.TypeCheck.Sigs")
   , ("TcSimplify", "GHC.TypeCheck.Simplify")
   , ("TcSMonad", "GHC.TypeCheck.SMonad")
   , ("TcSplice", "GHC.TypeCheck.Splice")
   , ("TcTyClsDecls", "GHC.TypeCheck.TyClsDecls")
   , ("TcTyDecls", "GHC.TypeCheck.TyDecls")
   , ("TcTypeable", "GHC.TypeCheck.Typeable")
   , ("TcType", "GHC.TypeCheck.Type")
   , ("TcTypeNats", "GHC.TypeCheck.TypeNats")
   , ("TcUnify", "GHC.TypeCheck.Unify")
   , ("TcValidity", "GHC.TypeCheck.Validity")

   , ("DynFlags", "GHC.Driver.SessionState")
   , ("DriverPhases", "GHC.Driver.Phases")
   , ("DriverPipeline", "GHC.Driver.Pipeline")

   , ("TysPrim", "GHC.Builtin.PrimTypes")
   , ("PrimOp",  "GHC.Builtin.PrimOps")
   , ("PrelNames", "GHC.Builtin.Names")
   , ("THNames", "GHC.Builtin.Names.TH")
   , ("TysWiredIn", "GHC.Builtin.Types")
   , ("KnownUniques", "GHC.Builtin.Uniques")
   , ("PrelInfo", "GHC.Builtin.Query")

{-
? <= backpack/BkpSyn.hs
? <= backpack/DriverBkp.hs
? <= backpack/NameShape.hs
? <= backpack/RnModIface.hs
? <= ghci/ByteCodeAsm.hs
? <= ghci/ByteCodeGen.hs
? <= ghci/ByteCodeInstr.hs
? <= ghci/ByteCodeItbls.hs
? <= ghci/ByteCodeLink.hs
? <= ghci/ByteCodeTypes.hs
? <= ghci/Debugger.hs
? <= ghci/GHCi.hs
? <= ghci/Linker.hs
? <= ghci/LinkerTypes.hs
? <= ghci/RtClosureInspect.hs
? <= hieFile/HieAst.hs
? <= hieFile/HieBin.hs
? <= hieFile/HieDebug.hs
? <= hieFile/HieTypes.hs
? <= hieFile/HieUtils.hs
? <= iface/BinFingerprint.hs
? <= iface/BinIface.hs
? <= iface/BuildTyCl.hs
? <= iface/FlagChecker.hs
? <= iface/IfaceEnv.hs
? <= iface/IfaceSyn.hs
? <= iface/IfaceType.hs
? <= iface/LoadIface.hs
? <= iface/MkIface.hs
? <= iface/TcIface.hs
? <= iface/ToIface.hs
? <= llvmGen/Llvm/AbsSyn.hs
? <= llvmGen/LlvmCodeGen/Base.hs
? <= llvmGen/LlvmCodeGen/CodeGen.hs
? <= llvmGen/LlvmCodeGen/Data.hs
? <= llvmGen/LlvmCodeGen.hs
? <= llvmGen/LlvmCodeGen/Ppr.hs
? <= llvmGen/LlvmCodeGen/Regs.hs
? <= llvmGen/Llvm.hs
? <= llvmGen/LlvmMangler.hs
? <= llvmGen/Llvm/MetaData.hs
? <= llvmGen/Llvm/PpLlvm.hs
? <= llvmGen/Llvm/Types.hs
? <= main/Annotations.hs
? <= main/Ar.hs
? <= main/CliOption.hs
? <= main/CmdLineParser.hs
? <= main/CodeOutput.hs
? <= main/Constants.hs
? <= main/DriverMkDepend.hs
? <= main/DynamicLoading.hs
? <= main/Elf.hs
? <= main/ErrUtils.hs
? <= main/FileCleanup.hs
? <= main/FileSettings.hs
? <= main/Finder.hs
? <= main/GHC.hs
? <= main/GhcMake.hs
? <= main/GhcMonad.hs
? <= main/GhcNameVersion.hs
? <= main/GhcPlugins.hs
? <= main/HeaderInfo.hs
? <= main/Hooks.hs
? <= main/HscMain.hs
? <= main/HscStats.hs
? <= main/HscTypes.hs
? <= main/InteractiveEval.hs
? <= main/InteractiveEvalTypes.hs
? <= main/PackageConfig.hs
? <= main/Packages.hs
? <= main/PipelineMonad.hs
? <= main/PlatformConstants.hs
? <= main/Plugins.hs
? <= main/PprTyThing.hs
? <= main/Settings.hs
? <= main/StaticPtrTable.hs
? <= main/SysTools/BaseDir.hs
? <= main/SysTools/ExtraObj.hs
? <= main/SysTools.hs
? <= main/SysTools/Info.hs
? <= main/SysTools/Process.hs
? <= main/SysTools/Settings.hs
? <= main/SysTools/Tasks.hs
? <= main/SysTools/Terminal.hs
? <= main/TidyPgm.hs
? <= main/ToolSettings.hs
? <= nativeGen/AsmCodeGen.hs
? <= nativeGen/BlockLayout.hs
? <= nativeGen/CFG.hs
? <= nativeGen/CPrim.hs
? <= nativeGen/Dwarf/Constants.hs
? <= nativeGen/Dwarf.hs
? <= nativeGen/Dwarf/Types.hs
? <= nativeGen/Format.hs
? <= nativeGen/Instruction.hs
? <= nativeGen/NCGMonad.hs
? <= nativeGen/PIC.hs
? <= nativeGen/PPC/CodeGen.hs
? <= nativeGen/PPC/Cond.hs
? <= nativeGen/PPC/Instr.hs
? <= nativeGen/PPC/Ppr.hs
? <= nativeGen/PPC/RegInfo.hs
? <= nativeGen/PPC/Regs.hs
? <= nativeGen/PprBase.hs
? <= nativeGen/RegAlloc/Graph/ArchBase.hs
? <= nativeGen/RegAlloc/Graph/ArchX86.hs
? <= nativeGen/RegAlloc/Graph/Coalesce.hs
? <= nativeGen/RegAlloc/Graph/Main.hs
? <= nativeGen/RegAlloc/Graph/SpillClean.hs
? <= nativeGen/RegAlloc/Graph/SpillCost.hs
? <= nativeGen/RegAlloc/Graph/Spill.hs
? <= nativeGen/RegAlloc/Graph/Stats.hs
? <= nativeGen/RegAlloc/Graph/TrivColorable.hs
? <= nativeGen/RegAlloc/Linear/Base.hs
? <= nativeGen/RegAlloc/Linear/FreeRegs.hs
? <= nativeGen/RegAlloc/Linear/JoinToTargets.hs
? <= nativeGen/RegAlloc/Linear/Main.hs
? <= nativeGen/RegAlloc/Linear/PPC/FreeRegs.hs
? <= nativeGen/RegAlloc/Linear/SPARC/FreeRegs.hs
? <= nativeGen/RegAlloc/Linear/StackMap.hs
? <= nativeGen/RegAlloc/Linear/State.hs
? <= nativeGen/RegAlloc/Linear/Stats.hs
? <= nativeGen/RegAlloc/Linear/X86_64/FreeRegs.hs
? <= nativeGen/RegAlloc/Linear/X86/FreeRegs.hs
? <= nativeGen/RegAlloc/Liveness.hs
? <= nativeGen/RegClass.hs
? <= nativeGen/Reg.hs
? <= nativeGen/SPARC/AddrMode.hs
? <= nativeGen/SPARC/Base.hs
? <= nativeGen/SPARC/CodeGen/Amode.hs
? <= nativeGen/SPARC/CodeGen/Base.hs
? <= nativeGen/SPARC/CodeGen/CondCode.hs
? <= nativeGen/SPARC/CodeGen/Expand.hs
? <= nativeGen/SPARC/CodeGen/Gen32.hs
? <= nativeGen/SPARC/CodeGen/Gen64.hs
? <= nativeGen/SPARC/CodeGen.hs
? <= nativeGen/SPARC/CodeGen/Sanity.hs
? <= nativeGen/SPARC/Cond.hs
? <= nativeGen/SPARC/Imm.hs
? <= nativeGen/SPARC/Instr.hs
? <= nativeGen/SPARC/Ppr.hs
? <= nativeGen/SPARC/Regs.hs
? <= nativeGen/SPARC/ShortcutJump.hs
? <= nativeGen/SPARC/Stack.hs
? <= nativeGen/TargetReg.hs
? <= nativeGen/X86/CodeGen.hs
? <= nativeGen/X86/Cond.hs
? <= nativeGen/X86/Instr.hs
? <= nativeGen/X86/Ppr.hs
? <= nativeGen/X86/RegInfo.hs
? <= nativeGen/X86/Regs.hs
? <= parser/ApiAnnotation.hs
? <= parser/Ctype.hs
? <= parser/HaddockUtils.hs
? <= parser/RdrHsSyn.hs
? <= prelude/ForeignCall.hs
? <= profiling/CostCentre.hs
? <= profiling/CostCentreState.hs
? <= profiling/ProfInit.hs
? <= utils/AsmUtils.hs
? <= utils/Bag.hs
? <= utils/Binary.hs
? <= utils/BooleanFormula.hs
? <= utils/BufWrite.hs
? <= utils/Digraph.hs
? <= utils/Encoding.hs
? <= utils/EnumSet.hs
? <= utils/Exception.hs
? <= utils/FastFunctions.hs
? <= utils/FastMutInt.hs
? <= utils/FastStringEnv.hs
? <= utils/FastString.hs
? <= utils/Fingerprint.hs
? <= utils/FiniteMap.hs
? <= utils/FV.hs
? <= utils/GhcPrelude.hs
? <= utils/GraphBase.hs
? <= utils/GraphColor.hs
? <= utils/GraphOps.hs
? <= utils/GraphPpr.hs
? <= utils/IOEnv.hs
? <= utils/Json.hs
? <= utils/ListSetOps.hs
? <= utils/Maybes.hs
? <= utils/MonadUtils.hs
? <= utils/OrdList.hs
? <= utils/Outputable.hs
? <= utils/Pair.hs
? <= utils/Panic.hs
? <= utils/PlainPanic.hs
? <= utils/PprColour.hs
? <= utils/Pretty.hs
? <= utils/State.hs
? <= utils/Stream.hs
? <= utils/StringBuffer.hs
? <= utils/TrieMap.hs
? <= utils/UniqDFM.hs
? <= utils/UniqDSet.hs
? <= utils/UniqFM.hs
? <= utils/UniqMap.hs
? <= utils/UniqSet.hs
? <= utils/UnVarGraph.hs
? <= utils/Util.hs
-}
   ]

ghcCommons :: [ModuleName]
ghcCommons =
   [ ["GHC","Entity"]
   , ["GHC","Data"]
   , ["GHC","Utils"]
   , ["GHC","Builtin"]
   , ["GHC","Config"]
   , ["GHC","RTS"]
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
   , renameModules  = renaming
   }

checkLayerHaskellToCore :: DepOptions
checkLayerHaskellToCore = defaultOptions
   { outputFile = "ghc_layer_haskelltocore"
   , filterImport = checkLayering ["GHC","HsToCore"] [ ["GHC","Hs"]
                                                     , ["GHC","Core"]
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

