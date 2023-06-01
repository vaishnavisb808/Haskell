{-
    Module : Internal Module 
    Description : exports all the functions used for API to Core conversion
    and Core to API conversion for testing  
-} 

{-#   OPTIONS_GHC -fno-warn-orphans    #-} 
{-# LANGUAGE DuplicateRecordFields #-}

module EcomApi.Core.Transformers.Internal where 

import           Control.Applicative (Alternative (empty))
import           Control.Monad.Time  (MonadTime (currentTime))
import           Data.Aeson
import           Data.ByteString     (map)
import           Data.List
import           Data.Map hiding (null) 
import           Data.Maybe
import           Data.Ord            (Ord)
import           Data.Semigroup      (diff)
import qualified Data.Text           as T
import           Data.Time
import           EcomApi.API.Types   as Api 
import           EcomApi.Core.Types  as Core
import           GHC.Generics
import           GHC.RTS.Flags       (MiscFlags (installSEHHandlers))


-- Core type to API Type Conversion

-- | convert Logic type in API types to Logic type in Core type
-- Core Logic type includes exceptions, rule, effectiveFrom and upc
convertApiTypeToCoreType :: Api.Request -> Core.Logic
convertApiTypeToCoreType (Request (Api.Logic storeExceptions zoneExceptions rule effective) upc) = do
            Core.Logic exception rules effective upc
             where  rules = convertRule rule
                    exception = combineExceptions storeExceptions zoneExceptions


-- | convert Rule type in API types to Rule type in Core type
-- Core Rule type includes ruleData and ruleType
convertRule :: Api.Rule -> Core.Rule
convertRule rule = Core.Rule (convertRuleToRuleData rule) Markup

-- | convert ZoneException type in API types to Exception type in Core type
-- Core Exception type includes exceptionRule, exceptionData and exceptionType
convertZoneExceptionToException :: Api.ZoneException -> Core.Exception
convertZoneExceptionToException (Api.ZoneException rule zone) =
    Core.Exception (Core.Rule exceptionruledata Markup ) (Core.ZoneExceptionData (ZoneNumber zone)) Core.ZoneException
      where  exceptionruledata = convertRuleToRuleData rule

-- | convert StoreException type in API types to Exception type in Core type
-- Core Exception type includes exceptionRule, exceptionData and exceptionType
convertStoreExceptiontoException :: Api.StoreException -> Core.Exception
convertStoreExceptiontoException (Api.StoreException rule store zone) =
    Core.Exception (Core.Rule exceptionruledata Markup) (Core.StoreExceptionData (ZoneNumber zone) (StoreNumber store))
                    Core.StoreException
      where  exceptionruledata = convertRuleToRuleData rule


-- | combine ZoneExceptions and StoreExceptions to list of Exceptions              
combineExceptions ::Maybe [Api.StoreException] -> Maybe [Api.ZoneException] -> [Core.Exception]
combineExceptions storeexceptions zoneexceptions = 
    store (maybeToEmptyList storeexceptions) ++ zone  (maybeToEmptyList zoneexceptions)
            
       where
            zone [] = [] 
            zone y = convertZoneExceptionToException <$> y
            store [] = []
            store x = convertStoreExceptiontoException <$> x
            maybeToEmptyList (Just a) = a
            maybeToEmptyList Nothing = []


-- | convert Rule type in API types to RuleData type in Core type
-- Core RuleData type includes adjustmentMethod, ignoreClearancePrice, noMarkupIfOnAd and markupBasisPoints
convertRuleToRuleData :: Api.Rule -> Core.RuleData
convertRuleToRuleData (Api.Rule adjustmentMethod ignoreClearancePrice noMarkupIfOnAd markupBasisPoints) =
     RuleData adjustmentMethod ignoreClearancePrice noMarkupIfOnAd markupBasisPoints

     

-- | Conversion of Core Rule type into API Rule
ruleConvertion :: Core.Rule -> Api.Rule
ruleConvertion (Core.Rule (Core.RuleData adjtMethod ignorCPrice noMarkup mBasisPoints) _) = Api.Rule
                  adjtMethod
                  ignorCPrice
                  noMarkup
                  mBasisPoints


-- | Diffrentiate the exception into store and zone exception
storeOrZone
            :: [Core.Exception]
            -> Map Core.ExceptionType [Core.Exception]
            -> Map Core.ExceptionType [Core.Exception]
storeOrZone [] finalMap    = finalMap
storeOrZone (x:xs) map     = storeOrZone xs $ insertWith (++) (exceptionType x) [x] map

-- | Conversion of Core Logic type into API Response
finalResponse :: [Core.Logic] -> UTCTime -> Api.Response
finalResponse logicOfList  = sortByDate listOfLogics upcNumber

  where
   listOfLogics              = Prelude.map finalLogic logicOfList
   upcNumber                 = Core.upc (head logicOfList)

-- | Conversion of Core Logic type into API Response
sortByDate :: [Api.Logic] -> Integer -> UTCTime -> Api.Response
sortByDate [] _ _ = Prelude.error "empty list"
sortByDate logicList upc' currentTime =
         if isFuture
             then Api.Response (Just latestLogic)
                               (if null remainingLogic then Nothing else Just (last remainingLogic ))
                               (if null remainingLogic then Nothing else Just $ init remainingLogic) 
                               upc'
             else Api.Response Nothing
                               (Just latestLogic)
                               (if null remainingLogic then Nothing else Just remainingLogic )
                               upc'
  where
     latestLogic              = last sorting
     remainingLogic           =  init sorting
     sorting                  = sortOn effective logicList
     isFuture                 = effective latestLogic > currentTime


-- | Conversion of Core Logic type into API Logic type
finalLogic :: Core.Logic -> Api.Logic
finalLogic logics             = Api.Logic
                                (if null storeExceptions then Nothing else Just storeExceptions)
                                (if null zoneExceptions then Nothing else Just zoneExceptions)
                                logicRule
                                effective
  where
        storeExceptions       = storeConversion <$>
                                fromMaybe [] ( Data.Map.lookup
                                Core.StoreException exceptionMap)
        zoneExceptions        = zoneConversion  <$>
                                fromMaybe [] ( Data.Map.lookup
                                Core.ZoneException  exceptionMap)
        logicRule             = ruleConvertion . Core.rule $ logics
        effective             = effectiveFrom logics
        exceptionMap          = storeOrZone (exceptions logics)
                                Data.Map.empty

-- | Conversion of Core exception type into API StoreException type
storeConversion :: Core.Exception -> Api.StoreException
storeConversion exception     =  Api.StoreException
                                 (getRuleFromException exception)
                                 (getStoreFromException exception)
                                 (getZoneFromException exception)
  where
        getRuleFromException  = ruleConvertion . exceptionRule
        getStoreFromException = unStoreNumber  . storeNumber . exceptionData
        getZoneFromException  = unZoneNumber   . zoneNumber  . exceptionData

-- | Conversion of Core exception type into API zoneException type
zoneConversion :: Core.Exception -> Api.ZoneException
zoneConversion exception      =   Api.ZoneException
                                  (getRuleFromException' exception)
                                  (getZoneFromException' exception)
  where
        getRuleFromException' = ruleConvertion . exceptionRule
        getZoneFromException' = unZoneNumber   . zoneNumber . exceptionData
