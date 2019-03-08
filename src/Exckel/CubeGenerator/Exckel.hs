module Exckel.CubeGenerator.Exckel
( writeCube
) where
import           Data.Array.Repa     ((:.), Array, DIM3, U)
import qualified Data.Array.Repa     as R
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Exckel.Types
import           Lens.Micro.Platform
import           Text.Printf
import Data.List.Split

-- | Write a Gaussian cube file. Everything in Bohr.
writeCube :: Cube -> T.Text
writeCube c = T.unlines
  [ -- comment lines
    T.pack $ c ^. comment

    -- number of atoms and origin vector of the cell
  , T.pack $ printf "%5d    %10.7f  %10.7f  %10.7f"
               (length $ c ^. atoms)
               (c ^. volumeOrigin . _1)
               (c ^. volumeOrigin . _2)
               (c ^. volumeOrigin . _3)

    -- number of voxels in A vector direction (usually x) and A vector
  , T.pack $ printf "%5d    %10.7f  %10.7f  %10.7f"
               (c ^. voxelDimension . _1)
               (c ^. volumeVectorA . _1)
               (c ^. volumeVectorA . _2)
               (c ^. volumeVectorA . _3)

    -- number of voxels in B vector direction (usually y) and B vector
  , T.pack $ printf "%5d    %10.7f  %10.7f  %10.7f"
               (c ^. voxelDimension . _2)
               (c ^. volumeVectorB . _1)
               (c ^. volumeVectorB . _2)
               (c ^. volumeVectorB . _3)
    -- number of voxels in C vector direction (usually z) and C vector
  , T.pack $ printf "%5d    %10.7f  %10.7f  %10.7f"
               (c ^. voxelDimension . _3)
               (c ^. volumeVectorC . _1)
               (c ^. volumeVectorC . _2)
               (c ^. volumeVectorC . _3)
  -- atoms and volumetric data
  , T.concat $
      [ -- atoms
        T.unlines $ map (\a -> T.pack $ printf "  %3d  %10.7f    %10.7f  %10.7f  %10.7f"
                                      (a ^. atomicNumber)
                                      ((fromIntegral :: Int -> Float) $ a ^. atomicNumber)
                                      (a ^. coordinate . _1)
                                      (a ^. coordinate . _2)
                                      (a ^. coordinate . _3)
                    ) (c ^. atoms)
        -- volumetric data
      , T.concat $ map zChunkWriter zChunks
      ]
  ]
  where
    -- convert REPA to plain list and split the list into chunks of zVoxel length
    zChunks = chunksOf (c ^. voxelDimension . _3) (R.toList $ c ^. volumetricData)
    -- writes a single line of volumetric data
    lineChunkWriter lC = T.pack $ concatMap (printf "%16.7e  ") lC
    -- write a complete chunk of volumetric data belongig to the same x and y coordinates layer
    zChunkWriter zC = T.unlines . map lineChunkWriter $ chunksOf 6 zC
