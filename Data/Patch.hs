-- | For gory implementation details, please see "Data.Patch.Internal"
module Data.Patch
       (
         -- * Patches
         Patch
       , toList
       , fromList
       , unsafeFromList
       , inverse
         -- ** Transformations and merges
       , transformWith
         -- *** Conflict strategies
       , transform
       , ours
       , theirs
         -- * Documents
       , apply
       , diff
         -- * Edits
       , Edit (..)
       , index
       , old
       , new
       )
       where

import Data.Patch.Internal
