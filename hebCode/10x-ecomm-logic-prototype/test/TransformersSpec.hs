{-|
    Module : TransformersSpec 
    Description : Test suit for different functions used for API Request type to 
    Core Logic type conversion
-} 

{-# LANGUAGE DuplicateRecordFields #-} 

module TransformersSpec where 
    
import    Test.Hspec
import    EcomApi.Core.Transformers.Internal as T 
import    EcomApi.Api.Types as Api
import    EcomApi.Core.Types as Core 
import    Data.Time 



newDate = (read "2022-01-19 18:28:52.607875 UTC") :: UTCTime 
ruleDetails = Api.Rule 
    { 
      Api.adjustmentMethod = "bankersRounding"
    , Api.ignoreClearancePrice = "true"
    , Api.noMarkupIfOnAd= "true"
    , Api.markupBasisPoints= 23
    }

zoneExc = Api.ZoneException
    {
      Api.rule = ruleDetails
    , Api.zone = 56
    }

storeExc = Api.StoreException
    {
      Api.rule = ruleDetails
    , Api.zone = 56
    , Api.store= 88
    }

logicData = Api.Logic
    {
      storeExceptions = Just [storeExc]
    , zoneExceptions = Just [zoneExc]
    , Api.rule = ruleDetails
    , effective= newDate
    }  

logicData1 = logicData { storeExceptions = Nothing }
logicData2 = logicData { zoneExceptions = Nothing }
logicData3 = logicData 
    { 
      storeExceptions = Nothing
    , zoneExceptions = Nothing
    }
request = Request 
    {
      futureLogic =logicData
    , upc = 3
    } 
request1 :: Request   
request1 = request { futureLogic  = logicData1 }

request2 :: Request
request2 = request { futureLogic  = logicData2 }

request3 :: Request
request3 = request { futureLogic = logicData3 }  


spec :: Spec
spec = do
      
      describe "convertRule" $ do
          it "converts Api.Rule to Core.Rule" $ do
                (T.convertRule ruleDetails) `shouldBe` 
                    ( Core.Rule
                        { ruleData = RuleData
                            { adjustmentMethod = "bankersRounding"
                            , ignoreClearancePrice="true"
                            , noMarkupIfOnAd="true"
                            , markupBasisPoints=23 
                            } 
                        , ruleType = Markup 
                        }
                    ) 
     
     
      describe "convertStoreExceptiontoException" $ do
          it "converts given StoreException type to Exception type" $ do
              (T.convertStoreExceptiontoException storeExc)  `shouldBe`
                  (Exception 
                     { exceptionRule = Core.Rule 
                        { ruleData = RuleData
                            { adjustmentMethod = "bankersRounding"
                            , ignoreClearancePrice="true"
                            , noMarkupIfOnAd="true"
                            , markupBasisPoints=23
                            }
                        , ruleType = Markup}
                     , exceptionData = StoreExceptionData 
                        { zoneNumber = ZoneNumber
                            { unZoneNumber = 56
                            }
                        , storeNumber = StoreNumber 
                            { unStoreNumber = 88}
                        }
                     , exceptionType = Core.StoreException} 
                  )

     
      describe "convertZoneExceptionToException" $ do
          it "zone to exception" $ do  
              (T.convertZoneExceptionToException zoneExc) `shouldBe` 
                  (Exception 
                     { exceptionRule = Core.Rule 
                         { ruleData = RuleData 
                            { adjustmentMethod = "bankersRounding"
                            , ignoreClearancePrice = "true"
                            , noMarkupIfOnAd = "true"
                            , Core.markupBasisPoints = 23
                            } 
                        , ruleType = Markup
                        }
                     , exceptionData = ZoneExceptionData
                                        {zoneNumber = ZoneNumber 
                                            { unZoneNumber = 56}
                                        }
                     , exceptionType = Core.ZoneException
                     }
                  )
  


      describe "convertRuleToRuleData" $ do  
          it "API Rule to Ruledata in Core" $ do 
               (T.convertRuleToRuleData ruleDetails) `shouldBe`  
                  ( RuleData 
                     { adjustmentMethod = "bankersRounding"
                     , ignoreClearancePrice = "true"
                     , noMarkupIfOnAd= "true"
                     , markupBasisPoints= 23 
                     }
                  ) 


      describe "combineExceptions" $ do 
          it "Combining store and zone exceptions in API to form exception in Core" $ do 
               (T.combineExceptions (Just [storeExc]) (Just [zoneExc])) `shouldBe` 
                  ( [Exception 
                        { exceptionRule = Core.Rule 
                            { ruleData = RuleData 
                                { adjustmentMethod = "bankersRounding"
                                , ignoreClearancePrice = "true"
                                , noMarkupIfOnAd = "true"
                                , markupBasisPoints = 23
                                }
                            , ruleType = Markup
                            }
                        , exceptionData = StoreExceptionData 
                            { zoneNumber = ZoneNumber 
                                { unZoneNumber = 56 }
                            , storeNumber = StoreNumber
                                { unStoreNumber = 88 }
                            }
                        , exceptionType = Core.StoreException
                        }
                        ,
                     Exception 
                        { exceptionRule = Core.Rule 
                            { ruleData = RuleData 
                                { adjustmentMethod = "bankersRounding"
                                , ignoreClearancePrice = "true"
                                , noMarkupIfOnAd = "true"
                                , markupBasisPoints = 23
                                }
                            , ruleType = Markup
                            }
                        , exceptionData = ZoneExceptionData 
                            { zoneNumber = ZoneNumber 
                                { unZoneNumber = 56 }
                            }
                        , exceptionType = Core.ZoneException
                        }
                    ])                                                               
          it "Combining null values of zone exception and store exception" $ do 
                (T.combineExceptions Nothing Nothing) `shouldBe` ([])  

          it "Only store exception exist " $ do 
                (T.combineExceptions (Just [storeExc]) Nothing) `shouldBe` 
                    ( [ Exception 
                        { exceptionRule = Core.Rule 
                            { ruleData = RuleData 
                                { adjustmentMethod = "bankersRounding"
                                , ignoreClearancePrice = "true"
                                , noMarkupIfOnAd = "true"
                                , markupBasisPoints = 23 }
                            , ruleType = Markup 
                            }
                        , exceptionData = StoreExceptionData 
                            { zoneNumber = ZoneNumber 
                                { unZoneNumber = 56 }
                            , storeNumber = StoreNumber 
                                { unStoreNumber = 88 }
                            }
                        , exceptionType = Core.StoreException 
                        }
                      ]) 
                                                                                                                                  
          it "Only zone exception exist " $ do 
                (T.combineExceptions Nothing (Just [zoneExc])) `shouldBe` 
                    ( [ Exception 
                        { exceptionRule = Core.Rule 
                            { ruleData =RuleData
                                { adjustmentMethod = "bankersRounding"
                                , ignoreClearancePrice = "true"
                                , noMarkupIfOnAd = "true"
                                , markupBasisPoints = 23 
                                }
                            , ruleType = Markup
                            }
                        , exceptionData = ZoneExceptionData 
                                { zoneNumber = ZoneNumber 
                                    { unZoneNumber = 56 }
                                }
                        , exceptionType = Core.ZoneException 
                        }
                     ]) 



      describe "convertApiTypeToCoreType" $ do  
          it "API Request with store and zone exceptions to Core Logic " $ do 
                (T.convertApiTypeToCoreType request) `shouldBe`  
                    ( Core.Logic 
                        { 
                         exceptions = 
                        [ Exception 
                            { exceptionRule = Core.Rule 
                                { ruleData = RuleData 
                                    { adjustmentMethod =      "bankersRounding"
                                    , ignoreClearancePrice = "true"
                                    , noMarkupIfOnAd = "true"
                                    , markupBasisPoints = 23
                                    }
                                    , 
                                    ruleType = Markup
                                }
                            , exceptionData = StoreExceptionData 
                                { zoneNumber = ZoneNumber 
                                    { unZoneNumber = 56 }
                                , storeNumber = StoreNumber 
                                    { unStoreNumber = 88 }
                                }
                            , exceptionType = Core.StoreException 
                            }
                        , Exception 
                            { exceptionRule = Core.Rule 
                                { ruleData = RuleData 
                                    { adjustmentMethod = "bankersRounding"
                                    , ignoreClearancePrice = "true"
                                    , noMarkupIfOnAd = "true"
                                    , markupBasisPoints = 23
                                    }
                                , ruleType = Markup
                                }
                            , exceptionData = ZoneExceptionData 
                                                { zoneNumber = ZoneNumber 
                                                    { unZoneNumber = 56 }
                                                }
                            , exceptionType = Core.ZoneException 
                            }
                                    ]
                    , rule = Core.Rule 
                        { ruleData = RuleData 
                            { adjustmentMethod = "bankersRounding"
                            , ignoreClearancePrice = "true"
                            , noMarkupIfOnAd = "true"
                            , markupBasisPoints = 23
                            }
                        , ruleType = Markup
                        }
                    , effectiveFrom = newDate
                    , upc = 3
                    } )       
  
          it "API Request with store exception to Core Logic" $ do 
                (T.convertApiTypeToCoreType request2) `shouldBe`  
                    ( Core.Logic 
                        { exceptions = 
                            [ Exception 
                                { exceptionRule = Core.Rule 
                                    { ruleData = RuleData 
                                        { adjustmentMethod = "bankersRounding"
                                        , ignoreClearancePrice = "true"
                                        , noMarkupIfOnAd = "true"
                                        , markupBasisPoints = 23
                                        }
                                    , ruleType = Markup
                                    }
                                , exceptionData = StoreExceptionData 
                                    { zoneNumber = ZoneNumber 
                                        { unZoneNumber = 56 }
                                    , storeNumber = StoreNumber 
                                        { unStoreNumber = 88 }
                                    }
                                , exceptionType = Core.StoreException
                                }
                                        ]
                        , rule = Core.Rule 
                            { ruleData = RuleData 
                                { adjustmentMethod = "bankersRounding"
                                , ignoreClearancePrice = "true"
                                , noMarkupIfOnAd = "true"
                                , markupBasisPoints = 23
                                }
                            , ruleType = Markup
                            }
                        , effectiveFrom = newDate
                        , upc = 3
                        } )                                                             
                                                                      
          it "API Request with zone exception to Core Logic" $ do 
                (T.convertApiTypeToCoreType request1) `shouldBe`  
                    ( Core.Logic 
                        { exceptions = 
                            [ Exception 
                                { exceptionRule = Core.Rule 
                                    { ruleData = RuleData
                                        { adjustmentMethod = "bankersRounding"
                                        , ignoreClearancePrice = "true"
                                        , noMarkupIfOnAd = "true"
                                        , markupBasisPoints = 23
                                        }
                                    , ruleType = Markup
                                    }
                                , exceptionData =ZoneExceptionData 
                                    { zoneNumber = ZoneNumber 
                                        { unZoneNumber = 56 }
                                    }
                                , exceptionType = Core.ZoneException
                                }
                                        ]
                        , rule = Core.Rule 
                            { ruleData = RuleData 
                                { adjustmentMethod = "bankersRounding"
                                , ignoreClearancePrice = "true"
                                , noMarkupIfOnAd = "true"
                                , markupBasisPoints = 23
                                }
                            , ruleType = Markup
                            }
                        , effectiveFrom = newDate
                        , upc = 3
                        })    

          it "API Request without any exceptions to Core Logic" $ do 
               (T.convertApiTypeToCoreType request3) `shouldBe`  
                    (Core.Logic 
                        { exceptions = []
                        , rule = Core.Rule 
                            { ruleData = RuleData 
                                { adjustmentMethod = "bankersRounding"
                                , ignoreClearancePrice = "true"
                                , noMarkupIfOnAd = "true"
                                , markupBasisPoints = 23
                                }
                            , ruleType = Markup
                            }
                        , effectiveFrom = newDate
                        , upc = 3
                        })                                                                


   

    

    
    