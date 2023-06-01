module Types where
import           Servant



-- | API Structure
type API = ViewLogicAPI :<|> ModifyLogicAPI


{- | API endpoint to view all logic(historic,present
     and future) for a given UPC
-}
type ViewLogicAPI = "logic"
                  :> Capture "upc" Int
                  :> Get '[JSON] (ApiResponse PriceLogics)


{- | API endpoint to create or modify future logic for
     given upc
-}
type ModifyLogicAPI = "logic"
            :> ReqBody '[JSON] Logic
            :> Post '[JSON] (ApiResponse Logic)