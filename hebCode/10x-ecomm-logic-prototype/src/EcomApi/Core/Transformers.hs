{-
    Transformers Module
    exports convertApiTypeToCoreType and finalResponse function to convert Logic type in API types to Logic type in Core type and viceversa
-}
{-#   OPTIONS_GHC -fno-warn-orphans    #-}
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
import qualified EcomApi.Api.Types                  as Api
import           EcomApi.Core.Transformers.Internal
import qualified EcomApi.Core.Types                 as Core


import           Control.Applicative (Alternative (empty))
import           Control.Monad.Time  (MonadTime (currentTime))
import           Data.Aeson
import           Data.ByteString     (map)
import           Data.List
import           Data.Map hiding (null) 
import           Data.Maybe
import           Data.Ord            (Ord,Down(Down))
import           Data.Semigroup      (diff)
import qualified Data.Text           as T
import           Data.Time
import           EcomApi.Api.Types   as Api 
import           EcomApi.Core.Types  as Core
import           GHC.Generics
import           GHC.RTS.Flags       (MiscFlags (installSEHHandlers))


-- | Conversion of Core Logic type into API Logic type
convertCoreTypeToApiType :: Core.Logic -> Api.Logic
convertCoreTypeToApiType logic = Api.Logic
                                    (if null storeExceptions
                                        then Nothing
                                        else Just storeExceptions
                                    )
                                    (if null zoneExceptions
                                        then Nothing
                                        else Just zoneExceptions
                                    )
                                    logicRule
                                    effective
  where
    storeExceptions       = convertExceptionToStoreException <$>
                            fromMaybe [] ( Data.Map.lookup
                            Core.StoreException exceptionMap)
    zoneExceptions        = convertExceptionToZoneException  <$>
                            fromMaybe [] ( Data.Map.lookup
                            Core.ZoneException  exceptionMap)
    logicRule             = convertCoreRuleToApiRule . Core.rule $ logic
    effective             = Core.effectiveFrom logic
    exceptionMap          = createExceptionTypeMap (Core.exceptions logic)
                            Data.Map.empty



-- | Conversion of list of Core Logic type into API Response
convertLogicListToResponse :: [Core.Logic] -> UTCTime ->Integer -> Api.Response
convertLogicListToResponse [] _ _ = Prelude.error "empty list"
convertLogicListToResponse listOfCoreLogic currentTime page =
    if page > 0 
        then Api.Response Nothing Nothing (Just (getListOfApiLogic listOfCoreLogic)) upc'
        else if isFuture
            then Api.Response (Just latestLogic)
                            (if null remainingLogic
                                then Nothing
                                else Just (last remainingLogic)
                            )
                            (if null remainingLogic
                                then Nothing
                                else Just $ init remainingLogic
                            )
                            upc'
            else Api.Response Nothing
                            (Just latestLogic)
                            (if null remainingLogic
                                then Nothing
                                else Just remainingLogic
                            )
                            upc'
  where
    latestLogic              = last sorting
    remainingLogic           = init sorting
    sorting                  = sortOn Api.effective (getListOfApiLogic listOfCoreLogic)
    isFuture                 = Api.effective latestLogic > currentTime
    getListOfApiLogic        = Prelude.map convertCoreTypeToApiType
    upc'    = Core.upc (head listOfCoreLogic)
