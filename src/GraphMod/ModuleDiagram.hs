{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module GraphMod.ModuleDiagram where

import Diagrams.Prelude
--import Diagrams.Backend.SVG
--import Diagrams.Backend.Cairo
import Diagrams.Backend.Rasterific
import GraphMod.Utils

import Data.Tree
import Data.List (sortOn,isPrefixOf)

type ModuleName = [String]

data DepOptions = DepOptions
   { filterModules :: ModuleName -> [Import] -> Bool
   }

defaultOptions :: DepOptions
defaultOptions = DepOptions
   { filterModules = \_ _ -> True
   }



make_diagrams :: DepOptions -> [(ModName,[Import])] -> IO ()
make_diagrams DepOptions{..} rawModuleImports = do
      --renderSVG' "deps.svg" (SVGOptions (mkWidth 3000) Nothing "" [] True) diag
      --renderCairo "deps.png" (mkWidth 20000) diag
      renderRasterific "deps.png" (mkWidth 10000) diag
   where
      -- filterImport modName importName = not (  "GHC.Entity."      `isPrefixOf` importName
      --                                       || "GHC.Utils"        `isPrefixOf` importName
      --                                       || "GHC.Data."        `isPrefixOf` importName
      --                                       || "GHC.Config.Flags" `isPrefixOf` importName
      --                                       )
      filterImport modName importName = "GHC.IR.Haskell.TypeChecker" `isPrefixOf` importName
      --filterImport = const True


      -- First we have the list of the considered modules and their imports in
      -- rawModuleImports

      -- We allow module filtering with `filterModules`
      filteredModules = filter (uncurry filterModules) rawModuleImports

      -- unqualified forest (name,value)
      utrees = treeMergeAll [ treeMake qname imps | (qname,imps) <- rawModuleImports]

      -- qualified forest (name,qualified name,value)
      qtrees :: Forest (String,String,Maybe [Import])
      qtrees = fmap treeQualify utrees

      -- show siblings dependencies
      siblingDeps = False

      -- show parent-to-child and child-to-parent dependencies
      parentToChildDeps = False
      childToParentDeps = False
      

      --diag :: QDiagram SVG V2 Float Any
      diag :: QDiagram Rasterific V2 Float Any
      diag = moduleForest
      
      moduleForest = hsep 2 (map diagModuleForest qtrees) 
                        # connectModules (extractForestChildren qtrees)
                        # connectDeps [ (name,imp) | (name,is) <- rawModuleImports
                                                   , imp <- is
                                                   ]

      moduleArrow m1 m2 = connectOutside' (with & shaftStyle %~ lwG 0.1 . lc gray
                                                & headLength .~ global 1
                                                & tailLength .~ global 1
                                                & arrowHead .~ mempty
                                          ) m1 m2

      depArrow m1 m2    = connectOutside' (with & shaftStyle %~ lwG 0.1 . lc blue . opacity 0.5
                                                & headLength .~ global 1
                                                & tailLength .~ global 1
                                                & arrowShaft .~ (arc yDir (1/24 @@ turn))
                                                & headStyle  %~ fc blue
                                          ) m1 m2

      sourceArrow m1 m2 = connectOutside' (with & shaftStyle %~ lwG 0.1 . lc red . opacity 0.5
                                                & headLength .~ global 1
                                                & tailLength .~ global 1
                                                & arrowShaft .~ (arc yDir (1/24 @@ turn))
                                                & headStyle  %~ fc red
                                          ) m1 m2

      connectModules [] d         = d
      connectModules ((a,b):cs) d = connectModules cs (d # moduleArrow a b)

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
                      && (siblingDeps || init imname /= init mname)
                      && (parentToChildDeps || init imname /= mname)
                      && (childToParentDeps || init mname /= imname)

            arr = case itype of
                     NormalImp -> depArrow
                     SourceImp -> sourceArrow

      filterModuleImports f (mname,imports) = (mname, filter (f mname) imports)
         

      existingModules = fmap (joinModName . fst) rawModuleImports
      doesModuleExist m = any (== m) existingModules

      extractNodeChildren node = [ (getQualifiedName node, getQualifiedName c) | c <- subForest node] ++ extractForestChildren (subForest node)
      extractForestChildren = concatMap extractNodeChildren

      getQualifiedName (Node (_,qname,_) _) = qname

      diagModuleForest (Node v []) = drawModule v
      diagModuleForest n@(Node v cs) =
         vsep (2*sqrt (1 + fromIntegral (sumChildren n)))
         [ drawModule v
         , center (hsep 1 (map diagModuleForest cs))
         ]


      drawModule (lbl,qname,_) =
         (text lbl 
         <> rect 14 2 # lwG 0.2 # lc gray # if doesModuleExist qname then id else dashingG [0.3,0.3] 0
         ) # named qname

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
      
treeMake :: ModName -> a -> Tree (String,Maybe a)
treeMake [] val     = Node ("{empty}",Just val) []
treeMake [name] val = Node (name,Just val) []
treeMake (q:qs) val = Node (q,Nothing) [ treeMake qs val]

treeMerge :: Tree (String,Maybe a) -> Tree (String,Maybe a) -> Maybe (Tree (String,Maybe a))
treeMerge t1 t2
   | fst (rootLabel t1) == fst (rootLabel t2) =
         let ret x = Just (Node (fst (rootLabel t1),x)
                        (treeMergeAll (subForest t1 ++ subForest t2)))
         in
         case (snd (rootLabel t1), snd (rootLabel t2)) of
            (Nothing,Nothing) -> ret Nothing
            (Just x ,Nothing) -> ret (Just x)
            (Nothing,Just x ) -> ret (Just x)
            (Just _ ,Just _ ) -> error $ "Module defined twice: "++fst (rootLabel t1)
   | otherwise = Nothing

treeMergeAll :: Forest (String, Maybe a) -> Forest (String, Maybe a)
treeMergeAll [] = []
treeMergeAll fs = foldr makeForest [] (sortOn (fst . rootLabel) fs)
   where
      makeForest t1 []      = [t1]
      makeForest t1 (t2:ts) = case treeMerge t1 t2 of
                                 Nothing -> t1:t2:ts
                                 Just t  -> t:ts

