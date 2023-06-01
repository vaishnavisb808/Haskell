import Control.Monad 
    
guard :: (MonadPlus m ) => Bool -> m()
guard True = return()
guard False = mzero