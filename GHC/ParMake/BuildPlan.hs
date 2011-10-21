-- This module is modelled after Distribution.Client.InstallPlan. If/when this
-- code becomes part of cabal-install, it'd be nice to merge both modules
-- somehow.

module GHC.ParMake.BuildPlan
       (new, ready, building, completed, size
       , numCompleted, markCompleted, numBuilding, hasBuilding
       , markReadyAsBuilding, BuildPlan, Target, TargetId, targetId, depends)
       where

import qualified Data.Array as Array
import qualified Data.Graph as Graph
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Control.Exception (assert)
import Data.Array ((!))
import Data.Graph (Graph)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List (groupBy, sort, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import System.FilePath (replaceExtension, takeExtension)

import GHC.ParMake.Common (appendMap, uniq)

type TargetId = FilePath
data Target = TargetModule TargetId [TargetId]
            | TargetInterface TargetId TargetId
            deriving (Show,Eq)

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
  planGraph     :: Graph,
  planGraphRev  :: Graph,
  planTargets   :: Graph.Table Target,
  planTargetIds :: Graph.Table TargetId,

  -- Target => number of dependencies that are not built yet.
  planNumDeps   :: IntMap Int,
  -- Targets that are ready to be built.
  planReady     :: IntSet,
  -- Targets that are currently building.
  planBuilding  :: IntSet
} deriving Show

-- TODO: Move these back into BuildPlan
planTargetOf :: BuildPlan -> Graph.Vertex -> Target
planTargetOf plan vertex = targetTable ! vertex
  where
    targetTable = planTargets plan

planVertexOf :: BuildPlan -> TargetId -> Maybe Graph.Vertex
planVertexOf plan = binarySearch lowBound topBound
  where
    targetIdTable = planTargetIds plan
    (lowBound, topBound) = Array.bounds targetIdTable
    binarySearch a b key
      | a > b     = Nothing
      | otherwise = case compare key (targetIdTable ! mid) of
        LT -> binarySearch a (mid-1) key
        EQ -> Just mid
        GT -> binarySearch (mid+1) b key
      where mid = (a + b) `div` 2

-- | Create a new BuildPlan from a list of (target, dependency) pairs. This is
-- mostly a copy of Distribution.Client.PackageIndex.dependencyGraph.
new :: [(TargetId, TargetId)] -> BuildPlan
new deps = plan
  where
    plan = BuildPlan graph graphRev targetTable targetIdTable
           numDepsMap readySet buildingSet
    targetIdToVertex = planVertexOf plan

    graph = Array.listArray bounds
            [ [ v | Just v <- map targetIdToVertex (depends target)]
            | target <- targets ]
    graphRev = Graph.transposeG graph

    numDepsMap = IntMap.fromList . map (\(n,t) -> (n, countNumDeps t))
                 . zip [0..] $ targets
      where
        -- Each '.o' node has an additional dependency on a '.hs' node which is
        -- not in the graph
        countNumDeps t = let numDeps = length . depends $ t
                         in if not . isInterface $ t
                            then numDeps - 1 else numDeps
        isInterface (TargetInterface _ _) = True
        isInterface _                     = False
    readySet = IntSet.fromList . map fst . filter hasSingleSourceDep
               . zip [0..] $ targets
      where hasSingleSourceDep (_,t) = case depends t of
              [d] -> let ext = takeExtension d in ext == ".hs"
              _   -> False
    buildingSet = IntSet.empty

    targetTable   = Array.listArray bounds targets
    targetIdTable = Array.listArray bounds (map targetId targets)
    targets       = sortBy (comparing targetId) (depsToTargets deps)
    topBound      = length targets - 1
    bounds        = (0, topBound)

-- | Total number of targets in the BuildPlan.
size :: BuildPlan -> Int
size = (+) 1 . snd . Array.bounds . planGraph

-- | Given a list of (target, dependency) pairs, produce a list of Targets. Note
-- that we need to make all implicit *.hi -> *.o dependencies explicit.
depsToTargets :: [(TargetId, TargetId)] -> [Target]
depsToTargets deps = interfaceTargets {- ++ -} (moduleTargets deps)
  where
    -- [(A.o,B.hi),(A.o,C.hi),...] =>
    -- [TargetInterface B.hi B.hs, TargetInterface C.hu C.hs] ++ rest
    interfaceTargets :: [Target] -> [Target]
    interfaceTargets = appendMap mkInterfaceTarget
                       (uniq . sort . filter isInterface . flatten $ deps)

    -- [(A.o,A.hs),(A.o,B.hi),(B.o,B.hs)] =>
    -- [TargetModule A.o [A.hs,B.hi], TargetModule B.o [B.hs]]
    moduleTargets :: [(TargetId, TargetId)] -> [Target]
    moduleTargets = map (\l -> TargetModule (fst . head $ l) (map snd l)) .
                    groupBy (\a b -> fst a == fst b)

    -- B.hi => TargetInterface B.hi B.o
    mkInterfaceTarget :: TargetId -> Target
    mkInterfaceTarget tId = TargetInterface tId (replaceExtension tId ".o")

    isInterface :: TargetId -> Bool
    isInterface tId = let ext = takeExtension tId
                      in ext == ".hi"

    -- [(a,b),(c,d)] => [a,b,c,d]
    flatten :: [(TargetId, TargetId)] -> [TargetId]
    flatten l = flatten' l []
      where
        flatten' []           accum = accum
        flatten' ((s1,s2):ss) accum = flatten' ss (s1:s2:accum)

verticesToTargets :: (BuildPlan -> IntSet) -> BuildPlan -> [Target]
verticesToTargets f plan = map (planTargetOf plan) (IntSet.toList $ f plan)

-- | Get all targets that are ready to be built.
ready :: BuildPlan -> [Target]
ready = verticesToTargets planReady

-- | Return all targets that are currently building.
building :: BuildPlan -> [Target]
building = verticesToTargets planBuilding

-- | Return all targets that were built successfully.
completed :: BuildPlan -> [Target]
completed plan = map (planTargetOf plan) keysCompleted
  where
    keysCompleted = IntMap.foldWithKey f [] (planNumDeps plan)
    f key n ks = if n == 0
                    && (not $ n `IntSet.member` planBuilding plan)
                    && (not $ n `IntSet.member` planReady plan)
                 then key:ks else ks

numCompleted :: BuildPlan -> Int
numCompleted plan = IntMap.fold f 0 (planNumDeps plan)
  where
    f n total = if n == 0 then total + 1 else total

-- | Mark all "ready" targets as "currently building".
markReadyAsBuilding :: BuildPlan -> BuildPlan
markReadyAsBuilding plan = plan {
  planReady = IntSet.empty,
  planBuilding = planBuilding plan `IntSet.union` planReady plan
  }

-- | How many targets are we building currently?
numBuilding :: BuildPlan -> Int
numBuilding = IntSet.size . planBuilding

-- | Are there any targets in the "currently building" state?
hasBuilding :: BuildPlan -> Bool
hasBuilding = not . IntSet.null . planBuilding

-- | Mark target as built successfully.
markCompleted :: BuildPlan -> Target -> BuildPlan
markCompleted plan target = assert check newPlan
  where
    check = vertex `IntSet.member` planBuilding plan
    vertex = fromMaybe
             (error $ "Target '" ++ targetId target ++ "' not in the graph!")
             (planVertexOf plan (targetId target))

    newBuilding = planBuilding plan `IntSet.difference` IntSet.singleton vertex

    deps = planGraphRev plan ! vertex
    (newReady, newNumDeps) = foldr updateNumDeps
                             (planReady plan, planNumDeps plan) deps
    updateNumDeps curVertex (rdy, numDeps) = assert check' (ready', numDeps')
      where
        check' = oldDepsCount > 0
        oldDepsCount = numDeps IntMap.! curVertex
        newDepsCount = oldDepsCount - 1
        ready' = if newDepsCount == 0
                 then rdy `IntSet.union` IntSet.singleton curVertex
                 else rdy
        numDeps' = IntMap.insert curVertex newDepsCount numDeps

    newPlan = plan {
      planBuilding = newBuilding,
      planReady = newReady,
      planNumDeps = newNumDeps
      }

-- TODO: In the future, this can be used to implement '-keep-going' (aka 'make
-- -k'), but for now we just abort (like GHC does).
-- failed :: BuildPlan -> Target -> BuildPlan
-- failed = undefined
