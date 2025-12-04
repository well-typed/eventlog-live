{-
This file was taken verbatim from the concurrent-machines package,
and is licensed under the following license:

Copyright (c) 2014, Anthony Cowley

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Anthony Cowley nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE CPP, GADTs, FlexibleContexts, RankNTypes, ScopedTypeVariables,
             TupleSections #-}
-- | Support for machines with two inputs from which input may be
-- drawn deterministically or non-deterministically. In contrast to
-- "Data.Machine.Wye", the two inputs are eagerly run concurrently in
-- this implementation.
module Data.Machine.Concurrent.Compat.Wye (wye) where
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Concurrent.Async.Lifted (wait, waitEither)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Machine hiding (wye, (~>), (<~))
import Data.Machine.Concurrent.Compat.AsyncStep

isX :: Is a c -> Y a b c
isX Refl = X

isY :: Is b c -> Y a b c
isY Refl = Y

-- | Only the 'X' input of a 'Wye' is not yet stopped, so we may employ
-- simpler dispatch logic.
wyeOnlyX :: forall a a' b b' c m. MonadBaseControl IO m
         => AsyncStep m (Is a) a' -> WyeT m a' b' c -> WyeT m a b c
wyeOnlyX src snk = MachineT $ runMachineT snk >>= \v -> case v of
  Stop -> return Stop
  Yield o k -> return $ Yield o (wyeOnlyX src k)
  Await _ Y ff -> runMachineT $ wye stopped stopped ff
  Await f X ff -> runMachineT $ stepAsync isX src f ff (encased v) wyeOnlyX
  Await f Z ff -> runMachineT $
    stepAsync isX src (f . Left) ff (encased v) wyeOnlyX

-- | Only the 'Y' input of a 'Wye' is not yet stopped, so we may
-- employ simpler dispatch logic.
wyeOnlyY :: MonadBaseControl IO m
         => AsyncStep m (Is b) b' -> WyeT m a' b' c -> WyeT m a b c
wyeOnlyY src m = MachineT $ runMachineT m >>= \v -> case v of
  Stop -> return Stop
  Yield o k -> return $ Yield o (wyeOnlyY src k)
  Await _ X ff -> runMachineT $ wye stopped stopped ff
  Await f Y ff -> runMachineT $ stepAsync isY src f ff (encased v) wyeOnlyY
  Await f Z ff ->
    runMachineT $ stepAsync isY src (f . Right) ff (encased v) wyeOnlyY

-- | Precompose a 'Process' onto each input of a 'Wye' (or 'WyeT').
--
-- When the choice of input is free (using the 'Z' input descriptor)
-- the two sources will be interleaved.
wye :: forall m a a' b b' c.
       (MonadBaseControl IO m)
    => ProcessT m a a' -> ProcessT m b b' -> WyeT m a' b' c -> WyeT m a b c
wye ma mb m = MachineT $ do srcL <- asyncRun ma
                            srcR <- asyncRun mb
                            go True m srcL srcR
  where go :: Bool
           -> WyeT m a' b' c
           -> AsyncStep m (Is a) a'
           -> AsyncStep m (Is b) b'
           -> m (MachineStep m (Y a b) c)
        go fair snk srcL srcR = runMachineT snk >>= \v -> case v of
          Stop         -> return Stop
          Yield o k    -> return . Yield o . MachineT $ go fair k srcL srcR
          Await f X ff -> wait srcL >>=
                          \(u :: MachineStep m (Is a) a') -> case u of
            Stop -> runMachineT $ wyeOnlyY srcR ff
            Yield a k -> asyncRun k >>= flip (go fair (f a)) srcR
            Await g Refl fg ->
              asyncAwait g X fg $ MachineT . flip (go fair (encased v)) srcR
          Await f Y ff -> wait srcR >>=
                          \(u :: MachineStep m (Is b) b') -> case u of
            Stop -> runMachineT $ wyeOnlyX srcL ff
            Yield b k -> asyncRun k >>= go fair (f b) srcL
            Await h Refl fh ->
              asyncAwait h Y fh $ MachineT . go fair (encased v) srcL

          -- Wait for whoever yields first
          Await f Z _  ->
            waitFair fair srcL srcR
            >>= \(u :: Either (MachineStep m (Is a) a')
                              (MachineStep m (Is b) b')) -> case u of
            Left (Yield a k) ->
              asyncRun k >>= \srcL' -> go (not fair) (f $ Left a) srcL' srcR
            Right (Yield b k) ->
              asyncRun k >>= \srcR' -> go (not fair) (f $ Right b) srcL srcR'
            Left Stop -> runMachineT $ wyeOnlyY srcR (encased v)
            Right Stop -> runMachineT $ wyeOnlyX srcL (encased v)

            -- The first source to respond wants to await, see what
            -- the other source has to offer.
            Left la@(Await g Refl fg) ->
              wait srcR >>= \(w :: MachineStep m (Is b) b') -> case w of
                Stop -> asyncAwait g X fg $ \l' -> wyeOnlyX l' (encased v)
                Yield b k -> runMachineT $ wye (encased la) k (f $ Right b)
                ra@(Await h Refl fh) -> return $
                  Await (\c -> case c of
                                 Left a -> wye (g a) (encased ra) (encased v)
                                 Right b -> wye (encased la) (h b) (encased v))
                        Z
                        (wye fg fh $ encased v)
            Right ra@(Await h Refl fh) ->
              wait srcL >>= \(w :: MachineStep m (Is a) a') -> case w of
                Stop -> asyncAwait h Y fh $ \r' -> wyeOnlyY r' (encased v)
                Yield a k -> runMachineT $ wye k (encased ra) (f $ Left a)
                la@(Await g Refl fg) -> return $
                  Await (\c -> case c of
                                 Left a -> wye (g a) (encased ra) (encased v)
                                 Right b -> wye (encased la) (h b) (encased v))
                        Z
                        (wye fg fh $ encased v)
          where waitFair True l r = waitEither l r
                waitFair False l r = either Right Left <$> waitEither r l
