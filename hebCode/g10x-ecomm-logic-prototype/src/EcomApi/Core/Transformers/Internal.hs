{-
    Module : Internal Module
    Description : exports all the functions used for API to Core conversion
    and Core to API conversion for testing
-}

{-# LANGUAGE DuplicateRecordFields #-}

module EcomApi.Core.Transformers.Internal where

import           Control.Applicative (Alternative (empty))
import           Control.Monad.Time  (MonadTime (currentTime))
import           Data.Aeson          (encode)
import           Data.ByteString     (map)
import           Data.List           ()
import           Data.Map            (Map, empty, insertWith, lookup)
import           Data.Maybe          (fromMaybe)
import           Data.Ord            (Ord)
import           Data.Semigroup      (diff)
import qualified Data.Text           as T
import           Data.Time           (getCurrentTime)
import           EcomApi.Api.Types   as Api (Logic (Logic), Request (Request),
                                             Rule (Rule),
                                             StoreException (StoreException),
                                             ZoneException (ZoneException))
import           EcomApi.Core.Types  as Core (Exception (Exception),
                                              ExceptionData (StoreExceptionData, ZoneExceptionData),
                                              ExceptionType (StoreException, ZoneException),
                                              Logic (Logic), Rule (Rule),
                                              RuleData (RuleData),
                                              RuleType (Markup),
                                              StoreNumber (StoreNumber),
                                              ZoneNumber (ZoneNumber),
                                              effectiveFrom, exceptionData,
                                              exceptionRule, exceptionType,
                                              exceptions, rule, storeNumber,
                                              unStoreNumber, unZoneNumber,
                                              zoneNumber)



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


-- | convert Rule type in API types to Rule type in Core type
-- Core Rule type includes ruleData and ruleType
convertRule :: Api.Rule -> Core.Rule
convertRule rule = Core.Rule (convertRuleToRuleData rule) Markup


-- | convert ZoneException type in API types to Exception type in Core type
-- Core Exception type includes exceptionRule, exceptionData and exceptionType
convertZoneExceptionToException :: Api.ZoneException -> Core.Exception
convertZoneExceptionToException ( Api.ZoneException rule zone ) =
    Core.Exception ( Core.Rule exceptionruledata Markup )
                   ( Core.ZoneExceptionData (ZoneNumber zone))
                   Core.ZoneException
  where
    exceptionruledata = convertRuleToRuleData rule


-- | convert StoreException type in API types to Exception type in Core type
-- Core Exception type includes exceptionRule, exceptionData and exceptionType
convertStoreExceptiontoException :: Api.StoreException -> Core.Exception
convertStoreExceptiontoException (Api.StoreException rule store zone) =
    Core.Exception ( Core.Rule exceptionruledata Markup )
                   ( Core.StoreExceptionData (ZoneNumber zone)
                                             (StoreNumber store)
                   )
                   Core.StoreException
  where
    exceptionruledata = convertRuleToRuleData rule


-- | combine ZoneExceptions and StoreExceptions to list of Exceptions
combineExceptions :: Maybe [Api.StoreException]
                  -> Maybe [Api.ZoneException]
                  -> [Core.Exception]
combineExceptions storeexceptions zoneexceptions =
    store (maybeToEmptyList storeexceptions) ++ zone  (maybeToEmptyList zoneexceptions)
  where
    zone [] = []
    zone y  = convertZoneExceptionToException <$> y
    store [] = []
    store x  = convertStoreExceptiontoException <$> x
    maybeToEmptyList (Just a) = a
    maybeToEmptyList Nothing  = []


{-| convert Rule type in API types to RuleData type in Core type
    Core RuleData type includes adjustmentMethod, ignoreClearancePrice,
    noMarkupIfOnAd and markupBasisPoints
-}
convertRuleToRuleData :: Api.Rule -> Core.RuleData
convertRuleToRuleData ( Api.Rule adjustmentMethod
                                 ignoreClearancePrice
                                 noMarkupIfOnAd
                                 markupBasisPoints
                      ) =
     RuleData adjustmentMethod ignoreClearancePrice noMarkupIfOnAd markupBasisPoints


-- | Conversion of Core Rule type into API Rule
convertCoreRuleToApiRule :: Core.Rule -> Api.Rule
convertCoreRuleToApiRule ( Core.Rule
                                ( Core.RuleData adjtMethod
                                                ignorCPrice
                                                noMarkup
                                                mBasisPoints
                                )
                                _
                         ) = Api.Rule
                                  adjtMethod
                                  ignorCPrice
                                  noMarkup
                                  mBasisPoints


-- | Diffrentiate the exception into store and zone exception
createExceptionTypeMap :: [Core.Exception]
            -> Map Core.ExceptionType [Core.Exception]
            -> Map Core.ExceptionType [Core.Exception]
createExceptionTypeMap [] finalMap    = finalMap
createExceptionTypeMap (x:xs) map     = createExceptionTypeMap xs $
                                            insertWith (++) (exceptionType x) [x] map


-- | Conversion of Core exception type into API StoreException type
convertExceptionToStoreException :: Core.Exception -> Api.StoreException
convertExceptionToStoreException exception     =
    Api.StoreException (getRuleFromException exception)
                       (getStoreFromException exception)
                       (getZoneFromException exception)
  where
    getRuleFromException  = convertCoreRuleToApiRule . exceptionRule
    getStoreFromException = unStoreNumber  . storeNumber . exceptionData
    getZoneFromException  = unZoneNumber   . zoneNumber  . exceptionData


-- | Conversion of Core exception type into API zoneException type
convertExceptionToZoneException :: Core.Exception -> Api.ZoneException
convertExceptionToZoneException exception =
    Api.ZoneException (getRuleFromException' exception)
                      (getZoneFromException' exception)
  where
    getRuleFromException' = convertCoreRuleToApiRule . exceptionRule
    getZoneFromException' = unZoneNumber   . zoneNumber . exceptionData

    
