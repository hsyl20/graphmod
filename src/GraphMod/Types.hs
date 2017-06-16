module GraphMod.Types where

import qualified GraphMod.Trie as Trie
import qualified Data.IntMap as IMap
import qualified Data.IntSet as ISet

type Nodes   = Trie.Trie String [((NodeT,String),Int)]
                    -- Maps a path to:   ((node, label), nodeId)

type Edges    = IMap.IntMap ISet.IntSet

data NodeT    = ModuleNode

              | ModuleInItsCluster
                -- ^ A module that has been relocated to its cluster

              | Redirect
                -- ^ This is not rendered. It is there to support replacing
                -- one node with another (e.g., when collapsing)

              | Deleted
                -- ^ This is not rendered, and edges to/from it are also
                -- not rendered.

              | CollapsedNode Bool
                -- ^ indicates if it contains module too.
                deriving (Show,Eq,Ord)

data AllEdges = AllEdges
  { normalEdges   :: Edges
  , sourceEdges   :: Edges
  }


