module Data.Machine.Concurrent.Compat (
  wye,
  scatter,
  mergeSum,
  splitSum,
  splitProd,
) where

import Data.Machine.Concurrent.Compat.Scatter (mergeSum, scatter, splitProd, splitSum)
import Data.Machine.Concurrent.Compat.Wye (wye)
