{-|
    Module : TransformersSpec
    Description : Test suit for different functions used for API Request type to
    Core Logic type conversion
-}

{-# LANGUAGE DuplicateRecordFields #-}

module TransformersSpec where

import           Data.Time
import           Data.Map
import           EcomApi.Api.Types                  as Api
import           EcomApi.Core.Transformers          as T
import           EcomApi.Core.Transformers.Internal as TI
import           EcomApi.Core.Types                 as Core
import           Test.Hspec



newDate = read "2022-01-25 18:28:52.607875 UTC" :: UTCTime
currentTime = read "2022-01-21 18:28:52.607875 UTC" :: UTCTime
pastDate    = read "2021-01-02 18:28:52.607875 UTC" :: UTCTime

coreRuleData =  Core.RuleData 
    {   Core.adjustmentMethod = "bankersRounding"
    ,   Core.ignoreClearancePrice = "true"
    ,   Core.noMarkupIfOnAd = "true"
    ,   Core.markupBasisPoints = 23
    }

apiRule      =  Api.Rule     
    {   Api.adjustmentMethod = "bankersRounding"
    ,   Api.ignoreClearancePrice = "true"
    ,   Api.noMarkupIfOnAd = "true"
    ,   Api.markupBasisPoints = 23
    }
coreRule     =  Core.Rule    
    {   ruleData = coreRuleData
    ,   ruleType = Markup
    }
    
detailsStore = StoreNumber 723
detailsZone = ZoneNumber 123

storeException = Core.Exception 
    {   exceptionRule = coreRule
    ,   exceptionData = Core.StoreExceptionData   detailsZone  detailsStore
    ,   exceptionType = Core.StoreException
    }

zoneException =  storeException  
    {   exceptionData = Core.ZoneExceptionData detailsZone
    ,   exceptionType = Core.ZoneException
    }

coreLogic =  Core.Logic 
    {   exceptions = [storeException,zoneException]
    ,   Core.rule  = coreRule
    ,   effectiveFrom = pastDate
    ,   Core.upc = 123
    }

emptyLogic = coreLogic { exceptions  = [] }

coreLogicWithPresentDate = coreLogic { effectiveFrom   = currentTime }

coreLogicWithPastDate = coreLogic { effectiveFrom   = pastDate }

coreRuleWithoutRuleType =  Core.Rule { ruleData = coreRuleData }

storeExceptionsDetails = Api.StoreException
    {   rule = apiRule
    ,   store = 723
    ,   zone = 123
    }
zoneExceptionsDetails =   Api.ZoneException  
    {   rule = apiRule
    ,   zone = 123
    }

storeNoAndZoneNo =  StoreExceptionData        
    {   zoneNumber = ZoneNumber { unZoneNumber = 123 }
    ,   storeNumber = StoreNumber { unStoreNumber = 723 }
    }
zoneNo = Core.ZoneExceptionData 
    {   zoneNumber = ZoneNumber {unZoneNumber = 123}
    }

exceptionIntoStoreAndZone = fromList
    [
        (Core.StoreException
        ,   [Core.Exception
                {   exceptionRule = coreRule  
                ,   exceptionData = storeNoAndZoneNo
                ,   exceptionType = Core.StoreException
                }
            ]
        )
    ,   (Core.ZoneException
        ,   [Core.Exception
                {   exceptionRule = coreRule  
                ,   exceptionData = zoneNo
                ,   exceptionType = Core.ZoneException
                }
            ]
        )
    ]


exceptionWithStoreOnly = fromList
    [
        (Core.StoreException
        ,   [Exception 
                {   exceptionRule = coreRule
                ,   exceptionData = storeNoAndZoneNo
                ,   exceptionType = Core.StoreException
                }
            ]
        )
    ]

exceptionWithZoneOnly = fromList
    [
        ( Core.ZoneException
        ,   [Exception
                {   exceptionRule = coreRule
                ,   exceptionData = zoneNo
                ,   exceptionType = Core.ZoneException
                }
            ]
        )
    ]

    
responseWithOnlyHistoricLogic = Response
    {   futureLogic = Nothing
    ,   presentLogic = Nothing
    ,   historicalLogic = Just 
            [Api.Logic 
                {   storeExceptions = Just [storeExceptionsDetails]
                ,   zoneExceptions = Just [zoneExceptionsDetails]
                ,   rule = apiRule
                ,   effective = currentTime
                    }
    ] 
    ,   upc = 123
    ,   pageKey = Just 1
    }

responseWithPastDate = Api.Response 
    {
        futureLogic = Nothing
    ,   presentLogic = Nothing
    ,   historicalLogic = Just 
            [ Api.Logic 
                {   storeExceptions = Just [storeExceptionsDetails]
                ,   zoneExceptions = Just [zoneExceptionsDetails]
                ,   rule = apiRule
                ,   effective = pastDate
                }
            ]
            
    ,   upc = 123
    ,   pageKey = Just 2
    }

responseWithEmptyLogic = Api.Response
    {   futureLogic = Nothing
    ,   presentLogic = Nothing
    ,   historicalLogic = Just 
        [ Api.Logic
            {   storeExceptions = Nothing
            ,   zoneExceptions = Nothing
            ,   rule = apiRule
            ,   effective = pastDate
            }
        ]
    ,  upc = 123
    ,  pageKey = Just 4}

zoneExc = Api.ZoneException
    {   Api.rule = apiRule
    ,   Api.zone = 123
    }

storeExc = Api.StoreException
    {   Api.rule = apiRule
    ,   Api.zone = 123
    ,   Api.store= 723
    }

logicData = Api.Logic
    {   storeExceptions = Just [storeExc]
    ,   zoneExceptions = Just [zoneExc]
    ,   Api.rule = apiRule
    ,   effective= newDate
    }

logicData1 = logicData { storeExceptions = Nothing }
logicData2 = logicData { zoneExceptions = Nothing }
logicData3 = logicData
    {   storeExceptions = Nothing
    ,   zoneExceptions = Nothing
    }
request = Request
    {   futureLogic =logicData
    ,   upc = 3
    }
request1 :: Request
request1 = request { futureLogic  = logicData1 }

request2 :: Request
request2 = request { futureLogic  = logicData2 }

request3 :: Request
request3 = request { futureLogic = logicData3 }


spec :: Spec
spec = do
  

    describe "convertCoreRuleToApiRule" $ do
        it "Rule Core data type to API data type" $ do
            convertCoreRuleToApiRule coreRule `shouldBe`  
                apiRule
        it "Rule Core data type to API data type with missing one parameter in core rule data type" $ do
            convertCoreRuleToApiRule coreRuleWithoutRuleType `shouldBe`  
                apiRule
    
    
    describe "convertExceptionToStoreException" $ do
        it "Convert Exception data type in Core to store exception In API type" $ do
            convertExceptionToStoreException storeException `shouldBe` 
                storeExceptionsDetails
        
    
    describe "convertExceptionToZoneException" $ do
        it "Convert Exception data type in Core  to zone exception in API data type" $ do
            convertExceptionToZoneException storeException `shouldBe` 
                zoneExceptionsDetails
    
    
    describe "createExceptionTypeMap" $ do
        it "differentiate the list if exceptions into store and zone exception" $ do
            createExceptionTypeMap [storeException,zoneException] Prelude.mempty `shouldBe` 
                exceptionIntoStoreAndZone
        it "Passing  list of stores  only" $ do
            createExceptionTypeMap [storeException] Prelude.mempty `shouldBe` 
                exceptionWithStoreOnly
        it "passing list of zones only" $ do
            createExceptionTypeMap [zoneException] Prelude.mempty `shouldBe` 
                exceptionWithZoneOnly


    
    describe "convertLogicListToResponse" $ do
        it "converting into response type in API data type  by sorting the date" $ do
            convertLogicListToResponse [coreLogicWithPresentDate] currentTime (Just 1) True `shouldBe` 
                responseWithOnlyHistoricLogic
        it "Giving pastDate instead of CurrentTime" $ do
            convertLogicListToResponse [coreLogicWithPastDate] pastDate (Just 2) True `shouldBe` 
                responseWithPastDate
        it "passing empty list for logic " $ do
            convertLogicListToResponse [emptyLogic] pastDate (Just 4) True `shouldBe`  
                responseWithEmptyLogic

    describe "convertRule" $ do
        it "converts Api.Rule to Core.Rule" $ do
                TI.convertRule apiRule `shouldBe`
                    coreRule
                    


    describe "convertStoreExceptiontoException" $ do
        it "converts given StoreException type to Exception type" $ do
              TI.convertStoreExceptiontoException storeExc  `shouldBe`
                    (Exception
                        {   exceptionRule = coreRule
                        ,   exceptionData = storeNoAndZoneNo
                        ,   exceptionType = Core.StoreException}
                    )

    describe "convertZoneExceptionToException" $ do
        it "zone to exception" $ do
              TI.convertZoneExceptionToException zoneExc `shouldBe`
                    (Exception
                        {   exceptionRule = coreRule
                        ,   exceptionData = zoneNo
                        ,   exceptionType = Core.ZoneException
                        }
                    )

    describe "convertRuleToRuleData" $ do
        it "API Rule to Ruledata in Core" $ do
            TI.convertRuleToRuleData apiRule `shouldBe`
                coreRuleData
                  


    describe "combineExceptions" $ do
        it "Combining store and zone exceptions in API to form exception in Core" $ do
               TI.combineExceptions (Just [storeExc]) (Just [zoneExc]) `shouldBe`
                   [Exception
                        {   exceptionRule = coreRule
                        ,   exceptionData = storeNoAndZoneNo
                        ,   exceptionType = Core.StoreException
                        }
                   ,Exception
                        {   exceptionRule = coreRule
                        ,   exceptionData = zoneNo
                        ,   exceptionType = Core.ZoneException
                        }
                    ]
        it "Combining null values of zone exception and store exception" $ do
                TI.combineExceptions Nothing Nothing `shouldBe`[]

        it "Only store exception exist " $ do
                TI.combineExceptions (Just [storeExc]) Nothing `shouldBe`
                    [ Exception
                        {   exceptionRule = coreRule
                        ,   exceptionData = storeNoAndZoneNo
                        ,   exceptionType = Core.StoreException
                        }
                    ]

        it "Only zone exception exist " $ do
                TI.combineExceptions Nothing (Just [zoneExc]) `shouldBe`
                    [ Exception
                        {   exceptionRule = coreRule
                        ,   exceptionData = zoneNo
                        ,   exceptionType = Core.ZoneException
                        }
                    ]



    describe "convertApiTypeToCoreType" $ do
        it "API Request with store and zone exceptions to Core Logic " $ do
                T.convertApiTypeToCoreType request `shouldBe`
                    ( Core.Logic
                        {   exceptions =
                            [Exception
                                {   exceptionRule = coreRule
                                ,   exceptionData = storeNoAndZoneNo
                                ,   exceptionType = Core.StoreException
                                }
                        ,   Exception
                                {   exceptionRule = coreRule
                                ,   exceptionData = zoneNo
                                ,   exceptionType = Core.ZoneException
                                }
                                    ]
                        ,   rule = coreRule
                        ,   effectiveFrom = newDate
                        ,   upc = 3
                        }
                    )

        it "API Request with store exception to Core Logic" $ do
                T.convertApiTypeToCoreType request2 `shouldBe`
                    ( Core.Logic
                        {   exceptions =
                            [ Exception
                                {   exceptionRule = coreRule
                                ,   exceptionData = storeNoAndZoneNo
                                ,   exceptionType = Core.StoreException
                                }
                            ]
                        ,   rule = coreRule
                        ,   effectiveFrom = newDate
                        ,   upc = 3
                        } 
                    )

        it "API Request with zone exception to Core Logic" $ do
                T.convertApiTypeToCoreType request1 `shouldBe`
                    ( Core.Logic
                        {   exceptions =
                            [ Exception
                                {   exceptionRule = coreRule
                                ,   exceptionData = zoneNo
                                ,   exceptionType = Core.ZoneException
                                }
                            ]
                        ,   rule = coreRule
                        ,   effectiveFrom = newDate
                        ,   upc = 3
                        }
                    )

        it "API Request without any exceptions to Core Logic" $ do
               T.convertApiTypeToCoreType request3 `shouldBe`
                    (Core.Logic
                        { exceptions = []
                        , rule = coreRule
                        , effectiveFrom = newDate
                        , upc = 3
                        }
                    )
