module API where

import GHC.Int
import GHC.Generics
import Lens.Micro

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Servant.Swagger
import Data.Swagger