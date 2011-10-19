-- This module is modelled after Distribution.Client.InstallPlan. If/when this
-- code becomes part of cabal-install, it'd be nice to merge both modules
-- somehow.

module BuildPlan
       where

import qualified Data.Array as Array
import qualified Data.Graph as Graph
import Data.Array ((!))
import Data.Graph (Graph)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import System.FilePath -- (replaceExtension)

type TargetId = FilePath
data Target = TargetModule TargetId [TargetId]
            | TargetInterface TargetId TargetId
            deriving Show

-- | Given a Target, return its ID.
targetId :: Target -> TargetId
targetId (TargetModule    tId _ ) = tId
targetId (TargetInterface tId _ ) = tId

-- | Given a Target, return its dependencies.
depends :: Target -> [TargetId]
depends (TargetModule _ deps) = deps
depends (TargetInterface _ d) = [d]

-- | A graph of all dependencies between targets.
data BuildPlan = BuildPlan {
  planGraph :: Graph,
  planTargetOf :: Graph.Vertex -> Target,
  planVertexOf :: TargetId -> Maybe Graph.Vertex
}

instance Show BuildPlan where
  show = show . planGraph

-- | Create a new BuildPlan from a list of (target, dependency) pairs. This is
-- mostly a copy of Distribution.Client.PackageIndex.dependencyGraph.
new :: [(TargetId, TargetId)] -> BuildPlan
new deps = BuildPlan graph vertexToTarget targetIdToVertex
  where
    graph = Array.listArray bounds
            [ [ v | Just v <- map targetIdToVertex (depends target)]
            | target <- targets ]
    vertexToTarget vertex = targetTable ! vertex
    targetIdToVertex      = binarySearch 0 topBound

    targetTable   = Array.listArray bounds targets
    targetIdTable = Array.listArray bounds (map targetId targets)
    targets       = sortBy (comparing targetId) (depsToTargets deps)
    topBound      = length targets - 1
    bounds        = (0, topBound)

    binarySearch a b key
      | a > b     = Nothing
      | otherwise = case compare key (targetIdTable ! mid) of
          LT -> binarySearch a (mid-1) key
          EQ -> Just mid
          GT -> binarySearch (mid+1) b key
      where mid = (a + b) `div` 2


-- | Given a list of (target, dependency) pairs, produce a list of Targets. Note
-- that we need to make all implicit *.hi -> *.o dependencies explicit.
depsToTargets :: [(TargetId, TargetId)] -> [Target]
depsToTargets deps = interfaceTargets deps ++ moduleTargets deps
  where
    -- [(A.o,A.hs),(A.o,B.hi),(B.o,B.hs)] =>
    -- [TargetModule A.o [A.hs,B.hi], TargetModule B.o [B.hs]]
    moduleTargets :: [(TargetId, TargetId)] -> [Target]
    moduleTargets = map (\l -> TargetModule (fst . head $ l) (map snd l)) .
                    groupBy (\a b -> fst a == fst b)

    -- [(A.o,A.hs),(A.o,B.hi),(A.o,C.hi),(B.o,B.hs),(C.o,C.hs)] =>
    -- [TargetInterface B.hi B.o, TargetInterface C.hi C.o]
    interfaceTargets :: [(TargetId, TargetId)] -> [Target]
    interfaceTargets =
      map (\tId -> TargetInterface tId (replaceExtension tId ".o"))
      . filter isInterface . flatten

    isInterface :: TargetId -> Bool
    isInterface tId = let ext = takeExtension tId
                      in ext == ".hi"

    -- [(a,b),(c,d)] => [a,b,c,d]
    flatten :: [(TargetId, TargetId)] -> [TargetId]
    flatten l = flatten' l []
      where
        flatten' []           accum = accum
        flatten' ((s1,s2):ss) accum = flatten' ss (s1:s2:accum)

-- ready :: BuildPlan -> [Target]
-- ready = undefined

-- building :: BuildPlan -> Target -> Target
-- building = undefined

-- completed :: BuildPlan -> Target -> Target
-- completed = undefined

-- In the future, this can be used to implement the analog of 'make -k', but for
-- now we just abort (like GHC does).
-- failed :: BuildPlan -> Target -> Target
-- failed = undefined
