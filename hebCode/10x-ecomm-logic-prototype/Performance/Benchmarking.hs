module Benchmarking where

import Criterion.Main
import    Data.Time

import EcomApi.Core.Transformers
import EcomApi.Core.Types as Core

newDate = (read "2022-01-19 18:28:52.607875 UTC") ::UTCTime 
logic= Core.Logic 
                        { exceptions = [ Exception 
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
                                } ]
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
                        } 


testMain= defaultMain [ bgroup "transformers" 
                                    [ bench "convertCoreTypeToApiType"   $ whnf convertCoreTypeToApiType   logic 
                                    , bench "convertLogicListToResponse"  $ whnf convertLogicListToResponse [logic]
                                    ]
                      ]