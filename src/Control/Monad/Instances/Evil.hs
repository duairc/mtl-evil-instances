{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module exports several very overlapping instances for the type classes
defined in the @mtl@ library, and should be used with caution, or not at all
(see the package description). The instances are defined:

  * @instance ('MonadTrans' t, 'Monad' (t m),        'MonadBase' b m)      => 'MonadBase' b        (t m)@

  * @instance ('MonadTransControl' t, 'Monad' (t m), 'MonadCont' m)        => 'MonadCont'          (t m)@

  * @instance ('MonadTransControl' t, 'Monad' (t m), 'MonadError' e m)     => 'MonadError' e       (t m)@

  * @instance ('MonadTransControl' t, 'Monad' (t m), 'MonadFix' m)         => 'MonadFix'           (t m)@

  * @instance ('MonadTrans' t, 'Monad' (t m),        'MonadIO' m)          => 'MonadIO'            (t m)@

  * @instance ('MonadTrans' t, 'Monad' (t m),        'MonadReader' r m)    => 'MonadReader' r      (t m)@

  * @instance ('MonadTrans' t, 'Monad' (t m),        'MonadRWS' r w s m)   => 'MonadRWS' r w s     (t m)@

  * @instance ('MonadTrans' t, 'Monad' (t m),        'MonadState' s m)     => 'MonadState' s       (t m)@

  * @instance ('MonadTrans' t, 'Monad' (t m),        'MonadWriter' w m)    => 'MonadWriter' w      (t m)@

  * @instance ('MonadBaseControl' b m,             'MonadCont' b)        => 'MonadCont'          m@

  * @instance ('MonadBaseControl' b m,             'MonadError' e b)     => 'MonadError' e       m@

  * @instance ('MonadBaseControl' b m,             'MonadFix' b)         => 'MonadFix'           m@

  * @instance ('MonadBase' b m,                    'MonadIO' b)          => 'MonadIO'            m@

  * @instance ('MonadBase' b m,                    'MonadReader' r b)    => 'MonadReader' r      m@

  * @instance ('MonadBase' b m,                    'MonadRWS' r w s b)   => 'MonadRWS' r w s     m@

  * @instance ('MonadBase' b m,                    'MonadState' s b)     => 'MonadState' s       m@

  * @instance ('MonadBase' b m,                    'MonadWriter' w b)    => 'MonadWriter' w      m@

Note that the following instance is not included, as currently it cannot be due to GHC bug #4259:

  * @instance ('MonadTransControl' t, 'Monad' (t m), 'MonadBaseControl' b m) => 'MonadBaseControl' b (t m)@


-}

module Control.Monad.Instances.Evil
    ()
where

import           Control.Applicative (Applicative (..))
import           Control.Monad.Base (MonadBase (..))
import           Control.Monad.Cont.Class (MonadCont(..))
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.Fix (MonadFix (..), fix)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader.Class (MonadReader (..))
import           Control.Monad.RWS.Class (MonadRWS (..))
import           Control.Monad.State.Class (MonadState (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Control
                     ( ComposeSt
                     , MonadBaseControl (..)
                     , MonadTransControl (..)
                     , Run
                     , control
                     , defaultLiftBaseWith
                     , defaultRestoreM
                     )
import           Control.Monad.Writer.Class (MonadWriter (..))


------------------------------------------------------------------------------
instance (MonadTrans t, Applicative (t m), Monad (t m), MonadBase b m) => MonadBase b (t m) where
    liftBase = lift . liftBase


------------------------------------------------------------------------------
{- This doesn't work, see: http://hackage.haskell.org/trac/ghc/ticket/4259
instance (MonadTransControl t, Monad (t m), MonadBaseControl b m) => MonadBaseControl b (t m) where
    newtype StM (t m) a = StMT {unStMT :: ComposeSt t m a}
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM = defaultRestoreM unStMT
-}


------------------------------------------------------------------------------
instance (MonadTransControl t, Monad (t m), MonadCont m) => MonadCont (t m) where
    callCC f = controlT $ \run -> callCC $ \c -> run . f $
        \a -> lift (run (return a) >>= c)


------------------------------------------------------------------------------
instance (MonadBaseControl b m, MonadCont b) => MonadCont m where
    callCC f = control $ \run -> callCC $ \c -> run . f $
        \a -> liftBase (run (return a) >>= c)


------------------------------------------------------------------------------
instance (MonadTransControl t, Monad (t m), MonadError e m) => MonadError e (t m) where
    throwError = lift . throwError
    catchError t h = controlT $ \run -> catchError (run t) (\e -> run (h e))


------------------------------------------------------------------------------
instance (MonadBaseControl b m, MonadError e b) => MonadError e m where
    throwError = liftBase . throwError
    catchError t h = control $ \run -> catchError (run t) (\e -> run (h e))


------------------------------------------------------------------------------
instance (MonadTransControl t, Monad (t m), MonadFix m) => MonadFix (t m) where
    mfix f = controlT $ \run -> mfix (\a -> run (restoreT (return a) >>= f))


------------------------------------------------------------------------------
instance (MonadBaseControl b m, MonadFix b) => MonadFix m where
    mfix f = control $ \run -> mfix (\a -> run (restoreM a >>= f))


------------------------------------------------------------------------------
instance (MonadTrans t, Monad (t m), MonadIO m) => MonadIO (t m) where
    liftIO = lift . liftIO


------------------------------------------------------------------------------
instance (MonadBase b m, MonadIO b) => MonadIO m where
    liftIO = liftBase . liftIO


------------------------------------------------------------------------------
instance (MonadTrans t, Monad (t m), MonadReader r m) => MonadReader r (t m) where
    ask = lift ask
    local f m = m >>= lift . local f . return


------------------------------------------------------------------------------
instance (MonadBase b m, MonadReader r b) => MonadReader r m where
    ask = liftBase ask
    local f m = m >>= liftBase . local f . return


------------------------------------------------------------------------------
instance (MonadTrans t, Monad (t m), MonadRWS r w s m) => MonadRWS r w s (t m)


------------------------------------------------------------------------------
instance (MonadBase b m, MonadRWS r w s b) => MonadRWS r w s m


------------------------------------------------------------------------------
instance (MonadTrans t, Monad (t m), MonadState s m) => MonadState s (t m) where
    get = lift get
    put s = lift $ put s


------------------------------------------------------------------------------
instance (MonadBase b m, MonadState s b) => MonadState s m where
    get = liftBase get
    put s = liftBase $ put s


------------------------------------------------------------------------------
instance (MonadTrans t, Monad (t m), MonadWriter w m) => MonadWriter w (t m) where
    tell w = lift $ tell w
    listen m = m >>= lift . listen . return
    pass m = m >>= lift . pass . return


------------------------------------------------------------------------------
instance (MonadBase b m, MonadWriter w b) => MonadWriter w m where
    tell w = liftBase $ tell w
    listen m = m >>= liftBase . listen . return
    pass m = m >>= liftBase . pass . return


------------------------------------------------------------------------------
controlT
    :: (MonadTransControl t, Monad (t m), Monad m)
    => (Run t -> m (StT t a)) -> t m a
controlT f = liftWith f >>= restoreT . return
