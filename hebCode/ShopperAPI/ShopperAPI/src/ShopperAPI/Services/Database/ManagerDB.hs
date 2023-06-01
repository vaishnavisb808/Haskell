{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module          ShopperAPI.Services.Database.ManagerDB where

import          Database.PostgreSQL.Simple (Connection,execute)


orderAcceptance orderid ordstatus conn= do
    
    updatedRow <- execute conn                    "UPDATE shopper.order \
                                                  \SET order_status = ? \
                                                  \WHERE order_id = ?"(ordstatus,orderid) 
    if updatedRow == 1
        then return $ Right 1
        else return (Left "order table status updation failed") 
    