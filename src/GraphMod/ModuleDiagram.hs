{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternGuards #-}

module GraphMod.ModuleDiagram where

import Diagrams.Prelude hiding ((|>))
import Diagrams.Backend.SVG
import Diagrams.Backend.Rasterific
import Diagrams.TwoD.Text
import GraphMod.Utils

import Data.Tree
import Data.List (sortOn, sort)
import Data.List.Extra (nubOn)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Arrow (second)

-- | Apply a function
--
-- >>> 5 |> (*2)
-- 10
(|>) :: a -> (a -> b) -> b
{-# INLINABLE (|>) #-}
x |> f = f x

infixl 0 |>


type ModuleName = [String]

data DepOptions = DepOptions
   { filterModule           :: ModuleName -> [Import] -> Bool
   , filterImport           :: ModuleName -> Import -> Bool
   , filterExternImport     :: ModuleName -> Bool
   , showExternModules      :: Bool                           -- ^ Show dependencies to other modules
   , showIndependentModules :: Bool                           -- ^ Show modules that are neither a dependency target nor a source
   , showSiblingsDeps       :: Bool
   , showParentToChildDeps  :: Bool
   , showChildToParentDeps  :: Bool
   , showNormalDeps         :: Bool
   , showSourceDeps         :: Bool
   , arrowShaftAngle        :: Float
   , externArrowShaftAngle  :: Float
   , outputFile             :: String
   , scaleFactor            :: Float
   , renameModules          :: Map ModuleName ModuleName
   }

defaultOptions :: DepOptions
defaultOptions = DepOptions
   { filterModule           = \_ _ -> True
   , filterImport           = \_ _ -> True
   , filterExternImport     = const True
   , showExternModules      = False
   , showIndependentModules = False
   , showSiblingsDeps       = True
   , showParentToChildDeps  = True
   , showChildToParentDeps  = True
   , showNormalDeps         = True
   , showSourceDeps         = True
   , arrowShaftAngle        = 1/24
   , externArrowShaftAngle  = 1/60
   , outputFile             = "deps"
   , scaleFactor            = 1
   , renameModules          = Map.empty
   }

make_diagrams :: DepOptions -> [(ModName,[Import])] -> IO ()
make_diagrams DepOptions{..} rawModuleImports = do
      renderSVG' (outputFile ++ ".svg") (SVGOptions (mkWidth 1000) Nothing "" [] True) diag
      --renderRasterific (outputFile ++ ".png") (mkWidth diagWidth) diag
   where
      -- Rename modules
      renameName n = Map.lookup n renameModules
      renameImp i = case renameName (importName i) of
         Nothing -> i
         Just n  -> i { importName = n }
      renameModImp (m,is) = case renameName m of
         Nothing -> (m,  fmap renameImp is)
         Just m' -> (m', fmap renameImp is)
      renamedModuleImports = fmap renameModImp rawModuleImports

      -- First we have the list of the considered modules and their imports in
      -- `rawModuleImports`. We allow module filtering with `filterModules`.
      filteredModules = filter (uncurry filterModule) renamedModuleImports

      -- Filter imports
      filteredImports = fmap (\x -> second (filter (filterImport (fst x))) x) filteredModules

      -- finally we have all the modules in `finalModules`
      finalModules       = filteredImports
      existingModules    = fmap fst finalModules
      doesModuleExist m  = any (== m) existingModules

      allImports         = nubOn importName (concatMap snd finalModules)
      externImports      = sort $ filter filterExternImport $ filter (not . doesModuleExist) (fmap importName allImports)
      isExternalModule m = any (== m) externImports

      
      -- Extract dependencies ("arrows")
      allDependencies    = [ (name,imp) | (name,is) <- finalModules, imp <- is ]

      depsFilters (name,iname) =    (showSiblingsDeps      || init iname /= init name)
                                 && (showParentToChildDeps || init iname /= name)
                                 && (showChildToParentDeps || init name /= iname)
                                 && (showExternModules     || not (isExternalModule iname))

      allFilteredDeps  = filter (depsFilters . second importName) allDependencies

      filteredNormalDeps = if not showNormalDeps
         then []
         else fmap (second importName)
              $ filter ((== NormalImp) . importType . snd) allFilteredDeps
      filteredSourceDeps = if not showSourceDeps
         then []
         else fmap (second importName)
              $ filter ((== SourceImp) . importType . snd) allFilteredDeps

      -- Module tree
      moduleForest :: Forest (String,ModuleName,Maybe [Import])
      moduleForest = treeMergeAll [ treeMake qname imps | (qname,imps) <- finalModules]

      getQualifiedName (Node (_,qname,_) _) = qname

      -- prune independent modules (that are not parents of dependent ones)
      isInDep x =    any (\(name,iname) -> name == x || iname == x) filteredNormalDeps
                  || any (\(name,iname) -> name == x || iname == x) filteredSourceDeps

      pruneTree n = if isInDep (getQualifiedName n) || not (null cs)
                     then Just (Node (rootLabel n) cs)
                     else Nothing
         where cs = mapMaybe pruneTree (subForest n)

      filteredModuleForest = if showIndependentModules
         then moduleForest
         else mapMaybe pruneTree moduleForest

      -- module hierarchical links
      extractNodeChildren node = [ (getQualifiedName node, getQualifiedName c) | c <- subForest node] ++ extractForestChildren (subForest node)
      extractForestChildren = concatMap extractNodeChildren

      moduleHierarchy = extractForestChildren filteredModuleForest


      ----------------------------------------------------
      -- Finally we draw the diagrams
      ----------------------------------------------------

      diag :: QDiagram SVG V2 Float Any
      --diag :: QDiagram Rasterific V2 Float Any
      diag = vsep 4
               [ drawModuleForest
               , if showExternModules
                  then center $ hsep 2 (fmap drawExternModule externImports)
                  else mempty
               ]
                        |> connectModules moduleHierarchy
                        |> connectDeps normalArrow filteredNormalDeps
                        |> connectDeps sourceArrow filteredSourceDeps
         where

            drawExternModule m = (text (joinModName mname) <> rect 25 2 # lwG 0.2 # lc purple) # named mname
               where mname = m
            
            drawModuleForest = hsep 2 (fmap diagModuleForest filteredModuleForest) 

            moduleArrow m1 m2 = connectOutside' (with & shaftStyle %~ lwG 0.1 . lc gray
                                                      & headLength .~ global 1
                                                      & tailLength .~ global 1
                                                      & arrowHead  .~ mempty
                                                ) m1 m2

            normalArrow m1 m2    = connectOutside' (with & shaftStyle %~ lwG 0.1 . lc blue . opacity 0.5
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

            makeArrowShaftAngle tgt = if isExternalModule tgt then externArrowShaftAngle else arrowShaftAngle

            connectModules [] d         = d
            connectModules ((a,b):cs) d = connectModules cs (d # moduleArrow a b)

            connectDeps _ [] d                  = d
            connectDeps arr ((name,iname):cs) d = connectDeps arr cs (d # arr name iname)

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
               ) # named (qname)

      maxChildren :: Tree a -> Int
      maxChildren (Node _ []) = 1
      maxChildren (Node _ cs) = max (length cs) (sum (fmap maxChildren cs))

      sumChildren :: Tree a -> Int
      sumChildren (Node _ []) = 1
      sumChildren (Node _ cs) = max (length cs) (sum (fmap sumChildren cs))

      diagWidth = scaleFactor * (fromIntegral $ 200 * maximum
         [ sum (fmap maxChildren filteredModuleForest)
         , if showExternModules then length externImports else 0
         ])



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

