-- This module is modelled after Distribution.Client.InstallPlan. If/when this
-- code becomes part of cabal-install, it'd be nice to merge both modules
-- somehow.

module GHC.ParMake.BuildPlan
       (new, ready, building, completed, size
       , numCompleted, markCompleted
       , markReadyAsBuilding, numBuilding, hasBuilding
       , BuildPlan, Target, TargetId, targetId, depends, source, object, objects)
       where

import qualified Data.Array as Array
import qualified Data.Graph as Graph
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Control.Exception (assert)
import Data.Array ((!))
import Data.Graph (Graph)
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List (find, groupBy, sort, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import System.FilePath (replaceExtension, takeExtension)

type TargetId = FilePath
data Target = Target TargetId  -- ^ Target (e.g. 'Main.o')
              FilePath         -- ^ Source (e.g. 'Main.hs')
              [TargetId]       -- ^ Dependencies (e.g. 'A.hi', 'B.hi')
            deriving (Show)

instance Eq Target where
  (==) = (==) `on` targetId

-- | Given a Target, return its ID.
targetId :: Target -> TargetId
targetId (Target tId _ _) = tId

-- | Given a Target, return its dependencies.
depends :: Target -> [TargetId]
depends (Target _ _ deps) = deps

-- | Given a Target, return the name of the source file from which it can be
-- produced.
source :: Target -> FilePath
source (Target _ tSrc _) = tSrc

-- | Given a Target, return the name of the object file produced from it that
-- should be fed to the linker.
object :: Target -> Maybe FilePath
object (Target tId _ _) = case takeExtension tId
                          of ".o-boot" -> Nothing
                             _         -> Just tId

-- | Given a BuildPlan, return the list of object files for all completed
-- targets.
objects :: BuildPlan -> [FilePath]
objects = mapMaybe object . completed

sourceExts, interfaceExts, objExts :: [String]
sourceExts    = [".hs", ".lhs", ".hs-boot", ".lhs-boot"]
interfaceExts = [".hi", ".hi-boot"]
objExts       = [".o", ".o-boot"]

-- | A graph of all dependencies between targets.
data BuildPlan = BuildPlan {
  planGraph     :: Graph,
  planGraphRev  :: Graph,
  planVertexOf  :: TargetId -> Maybe Graph.Vertex,
  planTargetOf  :: Graph.Vertex -> Target,

  -- Target => number of dependencies that are not built yet.
  planNumDeps   :: IntMap Int,
  -- Targets that are ready to be built.
  planReady     :: IntSet,
  -- Targets that are currently building.
  planBuilding  :: IntSet
}

-- Custom Show instance for debugging.
instance Show BuildPlan where
  show p = "BuildPlan {\n planGraph = "
           ++ show (planGraph p)
           ++ ",\n"
           ++ " planTargetIdOf = " ++ show numberedTargetIds ++ ",\n"
           ++ " planTargets = " ++ show targets ++ ",\n"
           ++ " planNumDeps = " ++ show (planNumDeps p) ++ ",\n"
           ++ " planReady = " ++ show (planReady p) ++ ",\n"
           ++ " planBuilding = " ++ show (planBuilding p)
           ++ "\n}"
    where
      targets           = map (planTargetOf p)[0..topBound]
      targetIds         = map targetId targets
      numberedTargetIds = (zip [(0::Int)..] targetIds)
      topBound          = snd . Array.bounds . planGraph $ p

-- | Create a new BuildPlan from a list of (target, dependency) pairs. This is
-- mostly a copy of Distribution.Client.PackageIndex.dependencyGraph.
new :: [(TargetId, TargetId)] -> BuildPlan
new deps = BuildPlan graph graphRev targetIdToVertex vertexToTargetId
           numDepsMap readySet buildingSet
  where
    targetIdToVertex   = binarySearch 0 topBound
    vertexToTargetId v = targetTable ! v

    -- TODO: This doesn't work well when -odir != -hidir.
    graph = Array.listArray bounds
            [ [ v | Just v <- map targetIdToVertex
                              . map interfaceToObj $ depends target]
            | target <- targets ]
      where
        -- We don't keep '.hi' targets in the graph, only in the depends list.
        interfaceToObj tId =
          case takeExtension tId of
            ".hi"      -> replaceExtension tId ".o"
            ".hi-boot" -> replaceExtension tId ".o-boot"
            _          -> tId

    graphRev = Graph.transposeG graph

    numDepsMap = IntMap.fromList . map (\(n,t) -> (n, countNumDeps t))
                 . zip [0..] $ targets
      where
        -- Each target has an additional dependency on a '.hs' file which is not
        -- in the graph.
        countNumDeps = subtract 1 . length . depends

    -- TODO: It is possible to create a BuildPlan that is non-empty, but has
    --       no buildable parts and thus is stuck.
    --       We should raise some error when that happens.
    readySet = IntSet.fromList . map fst . filter hasSingleSourceDep
               . zip [0..] $ targets
      where hasSingleSourceDep (_,t) = case depends t of
              -- TODO: This invariant should be enforced everywhere.
              []  -> error "BuildPlan: BUG: A target should never have 0 dependencies"
              [d] -> (takeExtension d) `elem` sourceExts
              _   -> False
    buildingSet = IntSet.empty

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

-- | Given a list of (target, dependency) pairs, produce a list of Targets.
depsToTargets :: [(TargetId, TargetId)] -> [Target]
depsToTargets = map (\l -> mkModuleTarget (fst . head $ l) (map snd l)) .
                groupBy ((==) `on` fst) .
                sort -- sort to not assume Makefile is not in order
  where
    mkModuleTarget tId tDeps = assert check (Target tId tSrc tDeps)
      where
        tSrc = fromMaybe (error "No source file in dependencies!")
               $ find ((`elem` sourceExts). takeExtension) tDeps

        check = (takeExtension tId `elem` objExts)
                && (length tDeps == 1
                    || or [ takeExtension d `elem` interfaceExts | d <- tDeps ])

-- | Total number of targets in the BuildPlan.
size :: BuildPlan -> Int
size = (+) 1 . snd . Array.bounds . planGraphRev

verticesToTargets :: BuildPlan -> IntSet -> [Target]
verticesToTargets plan vertices =
  map (planTargetOf plan) (IntSet.toList vertices)

-- | Get all targets that are ready to be built.
ready :: BuildPlan -> [Target]
ready p = verticesToTargets p $ planReady p

-- | Return all targets that are currently building.
building :: BuildPlan -> [Target]
building p = verticesToTargets p $ planBuilding p

-- | Return all targets that were built successfully.
completed :: BuildPlan -> [Target]
completed plan = map (planTargetOf plan) keysCompleted
  where
    keysCompleted = IntMap.foldWithKey f [] (planNumDeps plan)
    bldng         = planBuilding plan
    rdy           = planReady plan
    f key n ks = if n == 0
                    && (not $ key `IntSet.member` bldng)
                    && (not $ key `IntSet.member` rdy)
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

-- | Mark a target as successfully built.
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
