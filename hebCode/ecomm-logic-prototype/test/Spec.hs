{-# LANGUAGE OverloadedStrings #-}

module Main (main) where 

import     TransformersSpec  
import     Test.Hspec 
import     ApiSpec
main :: IO ()
main = do
    --hspec spec
    hspec businessLogicSpec

