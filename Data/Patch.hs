-- | For gory implementation details, please see "Data.Patch.Internal"
module Data.Patch
       (
         -- * Patches
         Patch
       , toList
       , fromList
       , unsafeFromList
       , inverse
       , composable
         -- * Documents
       , apply
       , applicable
       , diff
         -- ** Transformations and merges
       , transformWith
         -- *** Conflict strategies
       , transform
       , ours
       , theirs
         -- * Edits
       , Edit (..)
       , index
       , old
       , new
         -- * Viewing Patches and Hunks
       , Hunks
       , HunkStatus (..)
       , hunks
       )
       where

import Data.Patch.Internal
