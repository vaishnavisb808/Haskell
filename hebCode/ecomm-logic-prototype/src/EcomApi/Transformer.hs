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


import           Data.Map                           (Map, empty, lookup)
import           Control.Applicative (Alternative (empty))
import           Control.Monad.Time  (MonadTime (currentTime))
import           Data.Aeson
import           Data.ByteString     (map)
import           Data.List
import           Data.Map hiding (null)
import           Data.Maybe
import           Data.Ord            (Ord,Down(Down))
import           Data.Semigroup      (diff)
import           Data.Time
import           GHC.Generics
import           GHC.RTS.Flags       (MiscFlags (installSEHHandlers))


import           EcomApi.Core.Transformers.Internal


import qualified Data.Text           as T


-- import qualified EcomApi.Api.Types                  as Api
-- import qualified EcomApi.Core.Types                 as Core


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
convertLogicListToResponse listOfCoreLogic currentTime page
  | page > 0 = Api.Response Nothing Nothing (Just (getListOfApiLogic listOfCoreLogic)) upc'
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
  | otherwise = Api.Response Nothing
               (Just latestLogic)
               (if null remainingLogic
                   then Nothing
                   else Just remainingLogic
               )
               upc'
  where
      latestLogic = last sorting
      remainingLogic = init sorting
      sorting = sortOn Api.effective (getListOfApiLogic listOfCoreLogic)
      isFuture = Api.effective latestLogic > currentTime
      getListOfApiLogic = Prelude.map convertCoreTypeToApiType
      upc' = Core.upc (head listOfCoreLogic)