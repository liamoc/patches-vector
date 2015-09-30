-- | For gory implementation details, please see "Data.Patch.Internal"
module Data.Patch
       (
         -- * Patches
         Patch
       , toList
       , fromList
       , unsafeFromList
       , inverse
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
