{-
    Transformers Module
    exports convertApiTypeToCoreType and finalResponse function to convert Logic type in API types to Logic type in Core type and viceversa
-}
{-# LANGUAGE DuplicateRecordFields #-}

module EcomApi.Core.Transformers    (
      -- * Covert API Type to Core Type
      convertApiTypeToCoreType
      -- * Convert Core Type to API Type
    , convertLogicListToResponse
    ) where

import           Data.List                          (sortOn)
import           Data.Map                           (Map, empty, lookup)
import           Data.Maybe                         (fromMaybe)
import           Data.Time                          (UTCTime)
import qualified EcomApi.Api.Types                  as Api (ApiResponse)
import           EcomApi.Core.Transformers.Internal (combineExceptions,
                                                     convertCoreTypeToApiType,
                                                     convertRule)
import qualified EcomApi.Core.Types                 as Core (upc)


import           Control.Applicative                (Alternative (empty))
import           Control.Monad.Time                 (MonadTime (currentTime))
import           Data.Aeson                         (encode)
import           Data.ByteString                    (map)
import           Data.Map                           hiding (null)
import           Data.Ord                           (Down (Down), Ord)
import           Data.Semigroup                     (diff)
import qualified Data.Text                          as T ()
import           EcomApi.Api.Types                  as Api (Logic (Logic),
                                                            Request (Request),
                                                            Response (Response),
                                                            effective)
import           EcomApi.Core.Types                 as Core (ExceptionType (StoreException, ZoneException),
                                                             Logic (Logic),
                                                             effectiveFrom,
                                                             exceptions, rule)
import           GHC.RTS.Flags                      (MiscFlags (installSEHHandlers))



-- | convert Request type to Logic type
convertApiTypeToCoreType :: Api.Request -> Core.Logic
convertApiTypeToCoreType ( Request ( Api.Logic storeExceptions
                                              zoneExceptions
                                              rule
                                              effective
                                   )
                                   upc
                         ) = Core.Logic exception rules effective upc
  where
    rules = convertRule rule
    exception = combineExceptions storeExceptions zoneExceptions


-- | Conversion of list of Core Logic type into API Response
convertLogicListToResponse :: [Core.Logic] -> UTCTime -> Maybe Int-> Bool -> Api.Response
convertLogicListToResponse [] _ _ _ = Prelude.error "empty list"
convertLogicListToResponse listOfCoreLogic currentTime nextPageKey isOlderPage
    | isOlderPage = Api.Response Nothing
                                 Nothing
                                 (Just (getListOfApiLogic listOfCoreLogic))
                                 upc'
                                 nextPageKey
    | isFuture = Api.Response (Just latestLogic)
                            (if null remainingLogic
                                then Nothing
                                else Just (last remainingLogic)
                            )
                            (if null remainingLogic
                                then Nothing
                                else Just $ init remainingLogic
                            )
                            upc'
                            nextPageKey
    | otherwise = Api.Response Nothing
                            (Just latestLogic)
                            (if null remainingLogic
                                then Nothing
                                else Just remainingLogic
                            )
                            upc'
                            nextPageKey
  where
    latestLogic              = last sorting
    remainingLogic           = init sorting
    sorting                  = sortOn Api.effective (getListOfApiLogic listOfCoreLogic)
    isFuture                 = Api.effective latestLogic > currentTime
    getListOfApiLogic        = Prelude.map convertCoreTypeToApiType
    upc'    = Core.upc (head listOfCoreLogic)
