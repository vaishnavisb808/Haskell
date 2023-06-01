class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a-> m a-> m a
  
instance MonadPlus[] where
  mzero = []
  mplus = (++)

 

guard :: (MonadPlus m)=>Bool->m()
guard True= return()
guard False= mzero