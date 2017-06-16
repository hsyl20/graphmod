{-# LANGUAGE OverloadedStrings #-}
module GraphMod.ModuleDiagram where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import GraphMod.Types

make_diagrams :: (AllEdges,Nodes) -> IO ()
make_diagrams (es,t) = do
   let
      diag :: QDiagram SVG V2 Float Any
      diag = circle 0.5 <> unitCircle
   renderSVG' "deps.svg" (SVGOptions (mkWidth 250) Nothing "" [] True) diag



