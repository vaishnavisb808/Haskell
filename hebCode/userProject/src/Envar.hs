{-# LANGUAGE DeriveGeneric #-}
module Envar where
import System.Environment
--import Network.HTTP.Req
import GHC.Generics
main=do 
     setEnv "connecti" "1" 
     putStrLn =<<(getEnv "connecti")
     val<-getEnv "msg"
     print val