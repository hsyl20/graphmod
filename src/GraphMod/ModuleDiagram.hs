{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternGuards #-}

module GraphMod.ModuleDiagram where

import Diagrams.Prelude
--import Diagrams.Backend.SVG
import Diagrams.Backend.Rasterific
import GraphMod.Utils

import Data.Tree
import Data.List (sortOn,isPrefixOf,nub, intersperse, sort)
import Data.List.Extra (nubOn)
import Data.Maybe (fromMaybe)
import Control.Arrow (second)

type ModuleName = [String]

data DepOptions = DepOptions
   { filterModule          :: ModuleName -> [Import] -> Bool
   , filterImport          :: ModuleName -> Import -> Bool
   , filterExternImport    :: ModuleName -> Bool
   , showExternModules     :: Bool                           -- ^ Show dependencies to other modules
   , showSiblingsDeps      :: Bool
   , showParentToChildDeps :: Bool
   , showChildToParentDeps :: Bool
   , arrowShaftAngle       :: Float
   , externArrowShaftAngle :: Float
   }

defaultOptions :: DepOptions
defaultOptions = DepOptions
   { filterModule          = \_ _ -> True
   , filterImport          = \_ _ -> True
   , filterExternImport    = not . (["GHC"] `isPrefixOf`)
   , showExternModules     = True
   , showSiblingsDeps      = True
   , showParentToChildDeps = False
   , showChildToParentDeps = False
   , arrowShaftAngle       = 1/24
   , externArrowShaftAngle = 1/60
   }



make_diagrams :: DepOptions -> [(ModName,[Import])] -> IO ()
make_diagrams DepOptions{..} rawModuleImports = do
      --renderSVG' "deps.svg" (SVGOptions (mkWidth 3000) Nothing "" [] True) diag
      putStrLn (drawForest (fmap (fmap (\(_,t,is) -> joinModName t ++": "++concat (intersperse "," (fmap show (fromMaybe [] is))))) qtrees))
      renderRasterific "deps.png" (mkWidth 10000) diag
   where
      -- filterImport modName importName = not (  "GHC.Entity."      `isPrefixOf` importName
      --                                       || "GHC.Utils"        `isPrefixOf` importName
      --                                       || "GHC.Data."        `isPrefixOf` importName
      --                                       || "GHC.Config.Flags" `isPrefixOf` importName
      --                                       )
      --filterImport modName importName = "GHC.IR.Haskell.TypeChecker" `isPrefixOf` importName
      filterImport = \_ _ -> True


      -- First we have the list of the considered modules and their imports in
      -- `rawModuleImports`. We allow module filtering with `filterModules`.
      filteredModules = filter (uncurry filterModule) rawModuleImports

      -- Filter imports
      filteredImports = fmap (\x -> second (filter (filterImport (fst x))) x) filteredModules

      -- finally we have all the modules in `finalModules`
      finalModules       = filteredImports
      existingModules    = fmap fst finalModules
      doesModuleExist m  = any (== m) existingModules

      allImports         = nubOn importName (concatMap snd finalModules)
      externImports      = sort $ filter filterExternImport $ filter (not . doesModuleExist) (fmap importName allImports)
      isExternalModule m = any (== m) externImports

      -- Now we can build a tree:
      qtrees :: Forest (String,ModuleName,Maybe [Import])
      qtrees = treeMergeAll [ treeMake qname imps | (qname,imps) <- finalModules]

      
      allDependencies    = [ (name,imp) | (name,is) <- finalModules, imp <- is ]
      normalDependencies = 
         fmap (second importName)
         $ filter ((== NormalImp) . importType . snd) allDependencies
      sourceDependencies = 
         fmap (second importName)
         $ filter ((== SourceImp) . importType . snd) allDependencies



      ----------------------------------------------------
      -- Finally we draw the diagrams
      ----------------------------------------------------


      --diag :: QDiagram SVG V2 Float Any
      diag :: QDiagram Rasterific V2 Float Any
      diag = vsep 4
               [ moduleForest
               , if showExternModules
                  then center $ hsep 2 (fmap drawExternModule externImports)
                  else mempty
               ]
                        # connectModules (extractForestChildren qtrees)
                        # connectDeps allDependencies

      drawExternModule m = (text mname <> rect 25 2 # lwG 0.2 # lc purple) # named mname
         where mname = joinModName m
      
      moduleForest = hsep 2 (fmap diagModuleForest qtrees) 

      moduleArrow m1 m2 = connectOutside' (with & shaftStyle %~ lwG 0.1 . lc gray
                                                & headLength .~ global 1
                                                & tailLength .~ global 1
                                                & arrowHead .~ mempty
                                          ) m1 m2

      depArrow m1 m2    = connectOutside' (with & shaftStyle %~ lwG 0.1 . lc blue . opacity 0.5
                                                & headLength .~ global 1
                                                & tailLength .~ global 1
                                                & arrowShaft .~ (arc yDir (makeArrowShaftAngle m2 @@ turn))
                                                & headStyle  %~ fc blue
                                          ) m1 m2

      sourceArrow m1 m2 = connectOutside' (with & shaftStyle %~ lwG 0.1 . lc red . opacity 0.5
                                                & headLength .~ global 1
                                                & tailLength .~ global 1
                                                & arrowShaft .~ (arc yDir (makeArrowShaftAngle m2 @@ turn))
                                                & headStyle  %~ fc red
                                          ) m1 m2

      makeArrowShaftAngle tgt = if isExternalModule (splitModName tgt) then externArrowShaftAngle else arrowShaftAngle

      connectModules [] d         = d
      connectModules ((a,b):cs) d = connectModules cs (d # moduleArrow (joinModName a) (joinModName b))

      connectDeps [] d                             = d
      connectDeps ((mname,Import imname itype):cs) d =
         connectDeps cs (d # if filters
                                 then arr name iname
                                 else id
                        )
         where
            iname   = joinModName imname
            name    = joinModName mname

            filters = filterImport name iname
                      && (showSiblingsDeps || init imname /= init mname)
                      && (showParentToChildDeps || init imname /= mname)
                      && (showChildToParentDeps || init mname /= imname)

            arr = case itype of
                     NormalImp -> depArrow
                     SourceImp -> sourceArrow

      extractNodeChildren node = [ (getQualifiedName node, getQualifiedName c) | c <- subForest node] ++ extractForestChildren (subForest node)
      extractForestChildren = concatMap extractNodeChildren

      getQualifiedName (Node (_,qname,_) _) = qname

      diagModuleForest n
         | null (subForest n) = drawModule (rootLabel n)
         | otherwise          = 
            vsep (2*sqrt (1 + fromIntegral (sumChildren n)))
            [ drawModule (rootLabel n)
            , center (hsep 1 (map diagModuleForest (subForest n)))
            ]


      drawModule (lbl,qname,_) =
         (text lbl 
         <> rect 14 2 # lwG 0.2 # lc gray # if doesModuleExist qname
                                                then id
                                                else dashingG [0.3,0.3] 0
         ) # named (joinModName qname)

      maxChildren :: Tree a -> Int
      maxChildren (Node _ []) = 1
      maxChildren (Node _ cs) = maximum (length cs : fmap maxChildren cs)

      sumChildren :: Tree a -> Int
      sumChildren (Node _ []) = 1
      sumChildren (Node _ cs) = max (length cs) (sum (fmap sumChildren cs))



treeQualify :: Tree (String, Maybe a) -> Tree (String,String,Maybe a)
treeQualify = go ""
   where
      go q (Node (name,v) cs) =
         let q' = if q /= "" then q++"."++name else name
         in Node (name,q',v) (fmap (go q') cs)
      
treeMake :: forall a. ModuleName -> a -> Tree (String,ModuleName,Maybe a)
treeMake modName val = treeMake' [] modName
   where
      treeMake' _ []     = Node ("{empty}",modName,Just val) []
      treeMake' _ [name] = Node (name     ,modName,Just val) []
      treeMake' c (q:qs) = Node (q        ,c++[q] ,Nothing)
                              [ treeMake' (c++[q]) qs]

treeMerge :: Tree (String,ModuleName,Maybe a) -> Tree (String,ModuleName,Maybe a) -> Maybe (Tree (String,ModuleName,Maybe a))
treeMerge t1 t2
   | (lbl1,qname,val1) <- rootLabel t1
   , (lbl2,_,val2) <- rootLabel t2
   , lbl1 == lbl2 =
         let ret x = Just (Node (lbl1,qname,x)
                        (treeMergeAll (subForest t1 ++ subForest t2)))
         in
         case (val1,val2) of
            (Nothing,Nothing) -> ret Nothing
            (Just x ,Nothing) -> ret (Just x)
            (Nothing,Just x ) -> ret (Just x)
            (Just _ ,Just _ ) -> error $ "Module defined twice: "++lbl1
   | otherwise = Nothing

treeMergeAll :: Forest (String, ModuleName, Maybe a) -> Forest (String, ModuleName, Maybe a)
treeMergeAll [] = []
treeMergeAll fs = foldr makeForest [] (sortOn srt fs)
   where
      srt = (\(x,_,_) -> x) . rootLabel
      makeForest t1 []      = [t1]
      makeForest t1 (t2:ts) = case treeMerge t1 t2 of
                                 Nothing -> t1:t2:ts
                                 Just t  -> t:ts

