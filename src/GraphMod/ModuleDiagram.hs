{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module GraphMod.ModuleDiagram where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import GraphMod.Utils

import Data.Tree
import Data.List (sortOn)

make_diagrams :: [(ModName,[Import])] -> IO ()
make_diagrams fs = do
      renderSVG' "deps.svg" (SVGOptions (mkWidth 1000) Nothing "" [] True) diag
      putStrLn $ drawForest (fmap (fmap fst) utrees)
      putStrLn $ drawForest (fmap (fmap (\(_,qname,_) -> qname)) qtrees)
   where
      diag :: QDiagram SVG V2 Float Any
      diag = moduleForest
      
      moduleForest = hsep 2 (map diagModuleForest qtrees) # connectModules (extractForestChildren qtrees)

      moduleArrow m1 m2 = connectPerim m1 m2 (3/4 @@ turn) (1/4 @@ turn)

      connectModules [] d         = d
      connectModules ((a,b):cs) d = connectModules cs (d # moduleArrow a b)

      extractNodeChildren node = [ (getQualifiedName node, getQualifiedName c) | c <- subForest node] ++ extractForestChildren (subForest node)
      extractForestChildren = concatMap extractNodeChildren

      getQualifiedName (Node (_,qname,_) _) = qname

      diagModuleForest (Node v []) = drawModule v
      diagModuleForest (Node v cs) =
         vsep 5
         [ drawModule v
         , center (hsep 1 (map diagModuleForest cs))
         ]


      drawModule (lbl,qname,_) =
         (text lbl <> rect 15 2) # named qname

      -- unqualified forest (name,value)
      utrees = treeMergeAll [ treeMake qname imps | (qname,imps) <- fs]

      -- qualified forest (name,qualified name,value)
      qtrees :: Forest (String,String,Maybe [Import])
      qtrees = fmap treeQualify utrees


treeQualify :: Tree (String, Maybe a) -> Tree (String,String,Maybe a)
treeQualify = go ""
   where
      go q (Node (name,v) cs) =
         let q' = if q /= "" then q++"."++name else name
         in Node (name,q',v) (fmap (go q') cs)
      
treeMake :: ModName -> a -> Tree (String,Maybe a)
treeMake ([],name) val   = Node (name,Just val) []
treeMake (q:qs,name) val = Node (q,Nothing)
                              [ treeMake (qs,name) val]

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

