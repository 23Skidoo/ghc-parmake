{-# LANGUAGE NamedFieldPuns #-}

-- This module is modelled after Distribution.Client.InstallPlan. If/when this
-- code becomes part of cabal-install, it'd be nice to merge both modules
-- somehow.

module GHC.ParMake.BuildPlan
       (new, ready, building, completed, size
       , numCompleted, markCompleted
       , markReadyAsBuilding, numBuilding, hasBuilding
       , BuildPlan, Target, TargetId
       , targetId, allDepends, source, object, objects
       , Settings(..), defaultSettings)
       where

import qualified Data.Array as Array
import qualified Data.Graph as Graph
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Array ((!))
import Data.Graph (Graph)
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)

import Data.List (find, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import System.FilePath (replaceExtension, takeExtension)

import GHC.ParMake.Types (Dep(..))

-- | Settings for a BuildPlan
data Settings = Settings
  { osuf  :: String -- without dot
  , hisuf :: String -- without dot
  } deriving (Eq, Ord, Show)

defaultSettings :: Settings
defaultSettings = Settings
  { osuf = "o"
  , hisuf = "hi"
  }

type TargetId = FilePath
type ExternalDep = FilePath
data Target = Target
  { targetId           :: TargetId   -- ^ Target (e.g. 'Main.o')
  , targetSource       :: FilePath   -- ^ Source (e.g. 'Main.hs')
  , targetDeps         :: [TargetId] -- ^ Dependencies (e.g. 'A.hi', 'B.hi')
  , targetExternalDeps :: [ExternalDep]
    -- ^ External dependencies (e.g. from the system:
    -- '/usr/local/ghc/ghc-7.6.3/lib/ghc-7.6.3/base-4.6.0.1/Prelude.hi' or
    -- packages: 'cabal-dev/lib/mypackage-0.0.0.1/ghc-7.6.3/Module.hi')
  }
  deriving (Show)

instance Eq Target where
  (==) = (==) `on` targetId


-- | Given a Target, return its dependencies (excluding external ones).
depends :: Target -> [TargetId]
depends = targetDeps

-- | Given a Target, return its external dependencies.
externalDepends :: Target -> [TargetId]
externalDepends = targetExternalDeps

-- | Given a Target, return all its dependencies (internal + external).
allDepends :: Target -> [TargetId]
allDepends t = depends t ++ externalDepends t

-- | Given a Target, return the name of the source file from which it can be
-- produced.
source :: Target -> FilePath
source = targetSource

-- | Given a Target, return the name of the object file produced from it that
-- should be fed to the linker.
object :: Target -> Maybe FilePath
object (Target tId _ _ _) = case takeExtension tId
                            of ".o-boot" -> Nothing
                               _         -> Just tId

-- | Given a BuildPlan, return the list of object files for all completed
-- targets.
objects :: BuildPlan -> [FilePath]
objects = mapMaybe object . completed

sourceExts, defaultInterfaceExts, defaultObjExts :: [String]
sourceExts    = [".hs", ".lhs", ".hs-boot", ".lhs-boot"]
defaultInterfaceExts = [".hi", ".hi-boot"]
defaultObjExts       = [".o", ".o-boot"]

-- | `ext` must start with a dot.
isValidInterfaceExt :: Settings -> String -> Bool
isValidInterfaceExt Settings{ hisuf } ext = case ext of
  '.':_ -> ext `elem` defaultInterfaceExts || ext == ('.':hisuf)
  _     -> False

-- | `ext` must start with a dot.
isValidObjectExt :: Settings -> String -> Bool
isValidObjectExt Settings{ osuf } ext = case ext of
  '.':_ -> ext `elem` defaultObjExts || ext == ('.':osuf)
  _     -> False

-- | A graph of all dependencies between targets.
data BuildPlan = BuildPlan {
  planGraph     :: Graph,
  planGraphRev  :: Graph,
  planVertexOf  :: TargetId -> Maybe Graph.Vertex,
  planTargetOf  :: Graph.Vertex -> Target,

  -- | Target => number of dependencies that are not built yet.
  planNumDeps   :: IntMap Int,
  -- | Targets that are ready to be built.
  planReady     :: IntSet,
  -- | Targets that are currently building.
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
new :: Settings -> [Dep] -> BuildPlan
new settings@Settings{ osuf, hisuf } deps = BuildPlan graph graphRev targetIdToVertex vertexToTargetId
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
            -- TODO: Should we remove this in favour of osuf/hisuf?
            --       If yes, how do we deal with -boot files?
            ".hi"      -> replaceExtension tId ".o"
            ".hi-boot" -> replaceExtension tId ".o-boot"
            ext | ext == '.':hisuf -> replaceExtension tId ('.':osuf)
            _          -> tId

    graphRev = Graph.transposeG graph

    numDepsMap = IntMap.fromList . map (\(n,t) -> (n, countNumDeps t))
                 . zip [0..] $ targets
      where
        -- Each target has an additional dependency on a '.hs' file which is not
        -- in the graph.
        countNumDeps t = case length (depends t) of
          n | n > 0 -> n - 1
          _         -> error $ "GHC.ParMake.BuildPlan.countNumDeps: "
                       ++ "BUG: A target should never have 0 dependencies"

    -- TODO: It is possible to create a BuildPlan that is non-empty, but has
    --       no buildable parts and thus is stuck (e.g. when all targets
    --       have more than a single dependency).
    --       We should raise some error when that happens.
    readySet = IntSet.fromList . map fst . filter hasSingleSourceDep
               . zip [0..] $ targets
      where hasSingleSourceDep (_,t) = case depends t of
              -- TODO: This invariant should be enforced everywhere.
              []  -> error $ "GHC.ParMake.BuildPlan.hasSingleSourceDep: "
                     ++ "BUG: A target should never have 0 dependencies"
              [d] -> (takeExtension d) `elem` sourceExts
              _   -> False
    buildingSet = IntSet.empty

    targetTable   = Array.listArray bounds targets
    targetIdTable = Array.listArray bounds (map targetId targets)
    targets       = sortBy (comparing targetId) (depsToTargets settings deps)
    topBound      = length targets - 1
    bounds        = (0, topBound)

    binarySearch a b key
      | a > b     = Nothing
      | otherwise = case compare key (targetIdTable ! mid) of
        LT -> binarySearch a (mid-1) key
        EQ -> Just mid
        GT -> binarySearch (mid+1) b key
      where mid = (a + b) `div` 2

-- | Given a list of (target, [dependency]), perform some checks and produce
-- a list of build plan targets.
depsToTargets :: Settings -> [Dep] -> [Target]
depsToTargets settings@Settings{ hisuf } = map mkModuleTarget
  where
    mkModuleTarget (Dep t intDeps extDeps)
      | badExtension = error $ "GHC.ParMake.BuildPlan.depsToTargets: "
                       ++ "target must end with " ++ show (('.':hisuf):defaultObjExts)
      | not depsOK   = error $ "GHC.ParMake.BuildPlan.depsToTargets: "
                       ++ "dependencies are invalid: " ++ show intDeps
      | otherwise    = Target t tSrc intDeps extDeps
      where
        tSrc = fromMaybe (error "No source file in dependencies!")
               $ find ((`elem` sourceExts). takeExtension) intDeps

        badExtension = not $ isValidObjectExt settings (takeExtension t)
        depsOK = length intDeps == 1 -- TODO: Must this not be a sourceExts?
                   || or [ isValidInterfaceExt settings (takeExtension d) | d <- intDeps ]

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
markCompleted plan target
  | vertex `IntSet.notMember` planBuilding plan =
      error $ "GHC.ParMake.BuildPlan.markCompleted: "
      ++ "BUG: vertex not in planBuilding"
  | otherwise = newPlan
  where
    vertex = fromMaybe
             (error $ "Target '" ++ targetId target ++ "' not in the graph!")
             (planVertexOf plan (targetId target))

    newBuilding = planBuilding plan `IntSet.difference` IntSet.singleton vertex

    deps = planGraphRev plan ! vertex
    (newReady, newNumDeps) = foldr updateNumDeps
                             (planReady plan, planNumDeps plan) deps
    updateNumDeps curVertex (rdy, numDeps)
      | oldDepsCount <= 0 = error $ "GHC.ParMake.BuildPlan.updateNumDeps: "
                            ++ "BUG: oldDepsCount is " ++ show oldDepsCount
      | otherwise = (ready', numDeps')
      where
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
