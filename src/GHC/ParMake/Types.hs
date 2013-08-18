module GHC.ParMake.Types
       ( Dep(..) )
       where

-- | A single dependency extracted from the 'ghc -M' output.
data Dep = Dep
  { depTarget   :: FilePath   -- ^ The target file.
  , depInternal :: [FilePath] -- ^ Dependencies in our build.
  , depExternal :: [FilePath] -- ^ External dependencies: dependencies given by
                              -- 'ghc -M -include-pkg-deps' minus the internal
                              -- ones.
  } deriving (Eq, Ord, Show)
