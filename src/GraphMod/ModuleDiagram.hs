{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module GraphMod.ModuleDiagram where

import Diagrams.Prelude
--import Diagrams.Backend.SVG
import Diagrams.Backend.Cairo
import GraphMod.Utils

import Data.Tree
import Data.List (sortOn,isPrefixOf)

make_diagrams :: [(ModName,[Import])] -> IO ()
make_diagrams fs = do
      --renderSVG' "deps.svg" (SVGOptions (mkWidth 3000) Nothing "" [] True) diag
      renderCairo "deps.png" (mkWidth 20000) diag
      --putStrLn $ drawForest (fmap (fmap fst) utrees)
      --putStrLn $ drawForest (fmap (fmap (\(_,qname,_) -> qname)) qtrees)
   where
      --diag :: QDiagram SVG V2 Float Any
      diag = moduleForest
      
      moduleForest = hsep 2 (map diagModuleForest qtrees) 
                        # connectModules (extractForestChildren qtrees)
                        # connectDeps [ (joinModName name,imp) | (name,is) <- fs
                                                               , imp <- is
                                                               ]

      moduleArrow m1 m2 = connectOutside' (with & shaftStyle %~ lwG 0.1
                                                & headLength .~ global 1
                                                & tailLength .~ global 1
                                                & arrowHead .~ mempty
                                          ) m1 m2

      depArrow m1 m2    = connectOutside' (with & shaftStyle %~ lwG 0.1 . lc blue . opacity 0.5
                                                & headLength .~ global 1
                                                & tailLength .~ global 1
                                                & arrowShaft .~ (arc yDir (1/8 @@ turn))
                                          ) m1 m2

      sourceArrow m1 m2 = connectOutside' (with & shaftStyle %~ lwG 0.1 . lc red . opacity 0.5
                                                & headLength .~ global 1
                                                & tailLength .~ global 1
                                                & arrowShaft .~ (arc yDir (1/8 @@ turn))
                                          ) m1 m2

      connectModules [] d         = d
      connectModules ((a,b):cs) d = connectModules cs (d # moduleArrow a b)

      connectDeps [] d                             = d
      connectDeps ((name,Import iname itype):cs) d =
         connectDeps cs (d # if filterImport (joinModName iname)
                                 then arr name (joinModName iname)
                                 else id
                        )
         where
            filterImport x = not ("GHC.Entity." `isPrefixOf` x
                                 ||"GHC.Utils" `isPrefixOf` x
                                 ||"GHC.Data." `isPrefixOf` x
                                 )
            arr = case itype of
                     NormalImp -> depArrow
                     SourceImp -> sourceArrow

      existingModules = fmap (joinModName . fst) fs
      doesModuleExist m = any (== m) existingModules

      extractNodeChildren node = [ (getQualifiedName node, getQualifiedName c) | c <- subForest node] ++ extractForestChildren (subForest node)
      extractForestChildren = concatMap extractNodeChildren

      getQualifiedName (Node (_,qname,_) _) = qname

      diagModuleForest (Node v []) = drawModule v
      diagModuleForest n@(Node v cs) =
         vsep (fromIntegral $ sumChildren n)
         [ drawModule v
         , center (hsep 1 (map diagModuleForest cs))
         ]


      drawModule (lbl,qname,_) =
         (text lbl 
         <> rect 10 2 # lwG 0.2 # if doesModuleExist qname then id else dashingG [0.3,0.3] 0
         ) # named qname

      maxChildren :: Tree a -> Int
      maxChildren (Node _ []) = 1
      maxChildren (Node _ cs) = maximum (length cs : fmap maxChildren cs)

      sumChildren :: Tree a -> Int
      sumChildren (Node _ []) = 1
      sumChildren (Node _ cs) = max (length cs) (sum (fmap sumChildren cs))

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

