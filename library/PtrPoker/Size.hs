{-|
Functions that compute the required allocation size by value.
-}
module PtrPoker.Size
where

import PtrPoker.Prelude


{-# INLINE word64Dec #-}
word64Dec :: Word64 -> Int
word64Dec x =
  if x > 9999999999
    then if x > 99999999999999
      then if x > 9999999999999999
        then if x > 99999999999999999
          then if x > 999999999999999999
            then if x > 9999999999999999999
              then 20
              else 19
            else 18
          else 17
        else if x > 999999999999999
          then 16
          else 15
      else if x > 999999999999
        then if x > 9999999999999
          then 14
          else 13
        else if x > 99999999999
          then 12
          else 11
    else if x > 99999
      then if x > 9999999
        then if x > 99999999
          then if x > 999999999
            then 10
            else 9
          else 8
        else if x > 999999
          then 7
          else 6
      else if x > 99
        then if x > 999
          then if x > 9999
            then 5
            else 4
          else 3
        else if x > 9
          then 2
          else 1

{-# INLINE int64Dec #-}
int64Dec :: Int64 -> Int
int64Dec x =
  if x < 0
    then if x < -9999999999
      then if x < -99999999999999
        then if x < -9999999999999999
          then if x < -99999999999999999
            then if x < -999999999999999999
              then 20
              else 19
            else 18
          else if x < -999999999999999
            then 17
            else 16
        else if x < -999999999999
          then if x < -9999999999999
            then 15
            else 14
          else if x < -99999999999
            then 13
            else 12
      else if x < -99999
        then if x < -9999999
          then if x < -99999999
            then if x < -999999999
              then 11
              else 10
            else 9
          else if x < -999999
            then 8
            else 7
        else if x < -99
          then if x < -999
            then if x < -9999
              then 6
              else 5
            else 4
          else if x < -9
            then 3
            else 2
    else if x > 9999999999
      then if x > 99999999999999
        then if x > 9999999999999999
          then if x > 99999999999999999
            then if x > 999999999999999999
              then 19
              else 18
            else 17
          else if x > 999999999999999
            then 16
            else 15
        else if x > 999999999999
          then if x > 9999999999999
            then 14
            else 13
          else if x > 99999999999
            then 12
            else 11
      else if x > 99999
        then if x > 9999999
          then if x > 99999999
            then if x > 999999999
              then 10
              else 9
            else 8
          else if x > 999999
            then 7
            else 6
        else if x > 99
          then if x > 999
            then if x > 9999
              then 5
              else 4
            else 3
          else if x > 9
            then 2
            else 1
